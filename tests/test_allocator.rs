use beesafe::allocator::{Allocator, Heap};

#[derive(Debug, PartialEq)]
enum Object {
    Number(i32),
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
