use beesafe::allocator::{Allocator, Cell, Heap, Markable};
use beesafe::symbols::Object as BeeSafeObject;

#[derive(Debug, PartialEq)]
enum Object {
    Number(i32),
}

impl Markable<Object> for Object {
    fn get_references(&self) -> Vec<Cell<Object>> {
        Vec::new()
    }
}

#[test]
fn test_heap_init() {
    let mut heap = Heap::new();
    let c1 = heap.allocate_cell(Object::Number(3));
    dbg!(c1);
}

#[test]
fn test_heap_allocate() {
    let mut heap = Heap::new();
    let c1 = heap.allocate_cell(Object::Number(3));

    assert_eq!(c1.as_ref(), &Object::Number(3));
    // free c1
    heap.free_cell(c1);

    assert_eq!(heap.view_cell(c1), None);
    // dbg!(c1);
}

#[test]
fn test_heap_free() {
    let mut heap = Heap::new();
    let c1 = heap.allocate_cell(Object::Number(3));
    // 1st free cell 1
    heap.free_cell(c1);
    dbg!(c1);
    // second free cell 1
    heap.free_cell(c1);
    dbg!(c1);
}

#[test]
fn test_inner_cell() {
    let mut heap = Heap::new();
    let c1 = heap.allocate_cell(Object::Number(3));
}

#[test]
fn test_gc_basic_collection() {
    let mut heap: Heap<Object> = Heap::new();

    let obj1 = heap.allocate_cell(Object::Number(42));
    let obj2 = heap.allocate_cell(Object::Number(100));

    assert_eq!(heap.get_cells_count(), 2);

    heap.free_cell(obj1);
    assert_eq!(heap.get_cells_count(), 1);

    heap.free_cell(obj2);
    assert_eq!(heap.get_cells_count(), 0);
}

#[test]
fn test_gc_mark_and_sweep() {
    let mut heap: Heap<Object> = Heap::new();

    let root_obj = heap.allocate_cell(Object::Number(100));
    let unreachable_obj = heap.allocate_cell(Object::Number(200));

    heap.add_root(root_obj);

    println!("Before GC - Cells: {}", heap.get_cells_count());
    heap.collect_garbage();
    println!("After GC - Cells: {}", heap.get_cells_count());

    assert!(heap.is_marked(&root_obj));
    assert!(!heap.is_marked(&unreachable_obj));
}

#[test]
fn test_gc_root_management() {
    let mut heap: Heap<Object> = Heap::new();

    let obj1 = heap.allocate_cell(Object::Number(1));
    let obj2 = heap.allocate_cell(Object::Number(2));

    heap.add_root(obj1);
    heap.add_root(obj2);

    assert_eq!(heap.get_roots_count(), 2);

    heap.remove_root(&obj1);
    assert_eq!(heap.get_roots_count(), 1);

    heap.remove_root(&obj2);
    assert_eq!(heap.get_roots_count(), 0);
}

#[test]
fn test_gc_with_beesafe_objects() {
    let mut heap: Heap<BeeSafeObject> = Heap::new();

    let element1 = heap.allocate_cell(BeeSafeObject::Number(1));
    let element2 = heap.allocate_cell(BeeSafeObject::Number(2));
    let elements = vec![element1, element2];

    let array_obj = heap.allocate_cell(BeeSafeObject::Array(elements));
    let orphaned_obj = heap.allocate_cell(BeeSafeObject::String("orphaned".to_string()));

    heap.add_root(array_obj);

    println!("Before GC - Cells: {}", heap.get_cells_count());
    heap.collect_garbage();
    println!("After GC - Cells: {}", heap.get_cells_count());

    assert!(heap.is_marked(&array_obj));
    assert!(!heap.is_marked(&orphaned_obj));
}

#[test]
fn test_automatic_gc_trigger() {
    let mut heap: Heap<Object> = Heap::new();
    heap.set_gc_threshold(3);

    let obj1 = heap.allocate_cell(Object::Number(1));
    let obj2 = heap.allocate_cell(Object::Number(2));
    let _obj3 = heap.allocate_cell(Object::Number(3));

    assert_eq!(heap.get_allocation_count(), 3);
    assert_eq!(heap.get_cells_count(), 3);

    heap.add_root(obj1);

    // Crossing the threshold collects *before* allocating, so the unrooted cells
    // go and the cell handed back is live rather than already swept.
    let obj4 = heap.allocate_cell(Object::Number(4));
    assert_eq!(heap.get_allocation_count(), 1);
    assert_eq!(heap.get_cells_count(), 2);
    assert!(heap.is_marked(&obj1));
    assert!(!heap.is_marked(&obj2));
    assert!(heap.view_cell(obj4).is_some());
}

#[test]
fn test_manual_gc_force() {
    let mut heap: Heap<Object> = Heap::new();

    let obj1 = heap.allocate_cell(Object::Number(1));
    let obj2 = heap.allocate_cell(Object::Number(2));

    heap.add_root(obj1);

    assert_eq!(heap.get_cells_count(), 2);
    heap.force_gc();
    assert_eq!(heap.get_cells_count(), 1);
    assert_eq!(heap.get_allocation_count(), 0);
}

/// Counts drops so the heap's own Drop can be observed. Carries a payload
/// because Cell hashes by address, and every zero-sized allocation shares one.
struct Counted(u64);

static DROPPED: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

impl Drop for Counted {
    fn drop(&mut self) {
        DROPPED.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    }
}

impl Markable<Counted> for Counted {
    fn get_references(&self) -> Vec<Cell<Counted>> {
        Vec::new()
    }
}

#[test]
fn dropping_the_heap_frees_every_cell() {
    use std::sync::atomic::Ordering::SeqCst;
    DROPPED.store(0, SeqCst);
    {
        let mut heap: Heap<Counted> = Heap::new();
        heap.set_gc_threshold(1000);
        for _ in 0..5 {
            heap.allocate_cell(Counted(0));
        }
        heap.intern(Counted(1));
        assert_eq!(DROPPED.load(SeqCst), 0);
    }
    // Five collectable cells plus the interned one, each freed exactly once.
    assert_eq!(DROPPED.load(SeqCst), 6);
}
