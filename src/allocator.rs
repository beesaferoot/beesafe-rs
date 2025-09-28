/*
    allocator.rs contains the Heap allocation logic for the GC
*/

use std::collections::HashSet;
use std::hash::Hash;

/// Trait for objects that can be marked during garbage collection
pub trait Markable<T> {
    fn get_references(&self) -> Vec<Cell<T>>;
}

pub trait Allocator<T> {
    fn new() -> Self;
    fn allocate_cell(&mut self, valute: T) -> Cell<T>;
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
    roots: HashSet<Cell<T>>,       // GC roots - references from environment
    marked_cells: HashSet<*mut T>, // Track marked cells by pointer
    allocation_count: usize,       // Track allocations since last GC
    gc_threshold: usize,           // Trigger GC after this many allocations
}

impl<T: Markable<T>> Allocator<T> for Heap<T> {
    fn new() -> Self {
        Self {
            cells: HashSet::default(),
            roots: HashSet::default(),
            marked_cells: HashSet::default(),
            allocation_count: 0,
            gc_threshold: 100,
        }
    }

    fn allocate_cell(&mut self, value: T) -> Cell<T> {
        let ptr = Box::into_raw(Box::new(value));
        let cell = Cell { ptr, marked: false };

        self.cells.insert(cell);
        self.allocation_count += 1;

        if self.allocation_count >= self.gc_threshold {
            self.collect_garbage();
            self.allocation_count = 0;
        }

        cell
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
        println!("-- gc begin");
        self.mark_live_cells();
        self.sweep_unmarked_objects();
        print!("-- gc end");
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
        let root_ptrs: Vec<*mut T> = self.roots.iter().map(|root| root.ptr).collect();
        for ptr in root_ptrs {
            self.mark_recursive(ptr);
        }
    }

    fn mark_recursive(&mut self, ptr: *mut T) {
        if self.marked_cells.contains(&ptr) {
            return;
        }

        let cell_exists = self.cells.iter().any(|cell| cell.ptr == ptr);
        if !cell_exists {
            return;
        }

        // Mark this cell
        self.marked_cells.insert(ptr);

        if let Some(obj) = self.view_cell(Cell { ptr, marked: false }) {
            for reference in obj.get_references() {
                self.mark_recursive(reference.ptr);
            }
        }
    }

    fn add_root(&mut self, cell: Cell<T>) {
        self.roots.insert(cell);
    }

    fn remove_root(&mut self, cell: &Cell<T>) {
        self.roots.remove(cell);
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
