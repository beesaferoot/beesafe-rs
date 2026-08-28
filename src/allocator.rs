/*
    allocator.rs contains the Heap allocation logic for the GC
*/

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Trait for objects that can be marked during garbage collection
pub trait Markable<T> {
    fn get_references(&self) -> Vec<Cell<T>>;
}

pub trait Allocator<T> {
    fn new() -> Self;
    fn allocate_cell(&mut self, valute: T) -> Cell<T>;
    fn intern(&mut self, value: T) -> Cell<T>;
    fn temp_mark(&self) -> usize;
    fn temp_push(&mut self, cell: Cell<T>);
    fn temp_release(&mut self, mark: usize);
    fn free_cell(&mut self, cell: Cell<T>);
    fn collect_garbage(&mut self);
    fn mark_live_cells(&mut self);
    fn view_cell(&self, cell: Cell<T>) -> Option<&T>;
    fn view_mut_cell(&mut self, cell: Cell<T>) -> Option<&mut T>;
    fn sweep_unmarked_objects(&mut self);
    fn mark_recursive(&mut self, ptr: *mut T);
    fn add_root(&mut self, cell: Cell<T>);
    fn remove_root(&mut self, cell: &Cell<T>);
    fn is_marked(&self, cell: &Cell<T>) -> bool;
    fn get_cells_count(&self) -> usize;
    fn get_roots_count(&self) -> usize;
    fn set_gc_threshold(&mut self, threshold: usize);
    fn get_allocation_count(&self) -> usize;
    fn force_gc(&mut self);
}

#[derive(Debug)]
pub struct Cell<T> {
    ptr: *mut T,
    marked: bool,
}

pub struct Heap<T> {
    cells: HashSet<Cell<T>>,
    // Refcounted: one cell can be rooted by several owners (a binding and a
    // statement result, say), so a single remove_root must not unroot it.
    roots: HashMap<*mut T, usize>,
    permanent: Vec<Cell<T>>,       // interned constants: always live, never swept
    temps: Vec<Cell<T>>,           // shadow stack of in-flight temporaries
    marked_cells: HashSet<*mut T>, // Track marked cells by pointer
    allocation_count: usize,       // Track allocations since last GC
    gc_threshold: usize,           // Trigger GC after this many allocations
}

impl<T: Markable<T>> Allocator<T> for Heap<T> {
    fn new() -> Self {
        Self {
            cells: HashSet::default(),
            roots: HashMap::default(),
            permanent: Vec::new(),
            temps: Vec::new(),
            marked_cells: HashSet::default(),
            allocation_count: 0,
            gc_threshold: 100,
        }
    }

    fn allocate_cell(&mut self, value: T) -> Cell<T> {
        if self.allocation_count >= self.gc_threshold {
            self.collect_garbage();
            self.allocation_count = 0;
        }

        let ptr = Box::into_raw(Box::new(value));
        let cell = Cell { ptr, marked: false };
        self.cells.insert(cell);
        self.allocation_count += 1;
        cell
    }

    // An interned constant. Permanently live, so it is safe to share and it never
    // needs rooting by whoever holds it.
    fn intern(&mut self, value: T) -> Cell<T> {
        let ptr = Box::into_raw(Box::new(value));
        let cell = Cell { ptr, marked: false };
        self.cells.insert(cell);
        self.permanent.push(cell);
        cell
    }

    // Shadow stack. Temporaries held in Rust locals are invisible to the collector,
    // so a visitor pushes them here for the duration and truncates back on the way
    // out. Truncate-to-mark means nested calls compose without counting.
    fn temp_mark(&self) -> usize {
        self.temps.len()
    }

    fn temp_push(&mut self, cell: Cell<T>) {
        self.temps.push(cell);
    }

    fn temp_release(&mut self, mark: usize) {
        self.temps.truncate(mark);
    }

    fn free_cell(&mut self, cell: Cell<T>) {
        // println!(
        //     "free type {} of size {}",
        //     std::any::type_name::<T>(),
        //     size_of::<T>()
        // );
        if self.cells.contains(&cell) {
            // auto cleanup by converting raw pointer back to a managed box
            _ = unsafe { Box::from_raw(cell.ptr) };
            _ = self.cells.remove(&cell)
        }
    }

    fn view_cell(&self, cell: Cell<T>) -> Option<&T> {
        if self.cells.contains(&cell) {
            Some(unsafe { &*cell.ptr })
        } else {
            None
        }
    }

    fn view_mut_cell(&mut self, cell: Cell<T>) -> Option<&mut T> {
        if self.cells.contains(&cell) {
            Some(unsafe { &mut *cell.ptr })
        } else {
            None
        }
    }

    fn collect_garbage(&mut self) {
        self.mark_live_cells();
        self.sweep_unmarked_objects();
    }

    fn sweep_unmarked_objects(&mut self) {
        let mut to_remove = Vec::new();

        for cell in &self.cells {
            if !self.marked_cells.contains(&cell.ptr) {
                to_remove.push(*cell);
            }
        }

        for cell in to_remove {
            self.free_cell(cell);
        }
    }

    fn mark_live_cells(&mut self) {
        self.marked_cells.clear();
        let mut start: Vec<*mut T> = self.roots.keys().copied().collect();
        start.extend(self.permanent.iter().map(|c| c.ptr));
        start.extend(self.temps.iter().map(|c| c.ptr));
        for ptr in start {
            self.mark_recursive(ptr);
        }
    }

    // Explicit worklist rather than native recursion, so a deep object graph
    // cannot overflow the stack. Cell hashes by pointer, so `contains` is O(1).
    fn mark_recursive(&mut self, ptr: *mut T) {
        let mut work = vec![ptr];
        while let Some(ptr) = work.pop() {
            let cell = Cell { ptr, marked: false };
            if self.marked_cells.contains(&ptr) || !self.cells.contains(&cell) {
                continue;
            }
            self.marked_cells.insert(ptr);
            if let Some(obj) = self.view_cell(cell) {
                work.extend(obj.get_references().iter().map(|r| r.ptr));
            }
        }
    }

    fn add_root(&mut self, cell: Cell<T>) {
        *self.roots.entry(cell.ptr).or_insert(0) += 1;
    }

    fn remove_root(&mut self, cell: &Cell<T>) {
        if let Some(count) = self.roots.get_mut(&cell.ptr) {
            *count -= 1;
            if *count == 0 {
                self.roots.remove(&cell.ptr);
            }
        }
    }

    fn is_marked(&self, cell: &Cell<T>) -> bool {
        self.marked_cells.contains(&cell.ptr)
    }

    fn get_cells_count(&self) -> usize {
        self.cells.len()
    }

    fn get_roots_count(&self) -> usize {
        self.roots.len()
    }

    fn set_gc_threshold(&mut self, threshold: usize) {
        self.gc_threshold = threshold;
    }

    fn get_allocation_count(&self) -> usize {
        self.allocation_count
    }

    fn force_gc(&mut self) {
        self.collect_garbage();
        self.allocation_count = 0;
    }
}

impl<T> PartialEq for Cell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T> Eq for Cell<T> {}

impl<T> Hash for Cell<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<T> Clone for Cell<T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            marked: self.marked,
        }
    }
}

impl<T> AsRef<T> for Cell<T> {
    fn as_ref(&self) -> &T {
        unsafe { &*self.ptr }
    }
}

impl<T> AsMut<T> for Cell<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { &mut *self.ptr }
    }
}

impl<T> Copy for Cell<T> {}
