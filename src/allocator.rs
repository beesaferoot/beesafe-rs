/*
    allocator.rs contains a mark-sweep heap: cells are allocated individually and
    freed by tracing from the roots and dropping whatever was not reached.
*/

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Enumerates the references a value holds, so a collector can reach everything
/// live from a set of roots.
///
/// Every variant that can hold a reference must be listed. A wildcard arm that
/// returns nothing makes the collector treat those references as unreachable and
/// free values that are still in use.
pub trait Markable<T> {
    fn get_references(&self) -> Vec<Cell<T>>;
}

pub trait Allocator<T> {
    fn allocate_cell(&mut self, value: T) -> Cell<T>;

    /// Allocates a value that is never collected, for constants shared across
    /// the whole program.
    fn intern(&mut self, value: T) -> Cell<T>;

    /// Returns `None` for a reference whose value has been collected.
    fn view_cell(&self, cell: Cell<T>) -> Option<&T>;
    fn view_mut_cell(&mut self, cell: Cell<T>) -> Option<&mut T>;

    /// Roots are refcounted: a value reachable through several owners needs one
    /// `remove_root` per `add_root` before it can be collected.
    fn add_root(&mut self, cell: Cell<T>);
    fn remove_root(&mut self, cell: &Cell<T>);

    /// Shadow stack for values that are live but not reachable from any root.
    /// Push on entry, truncate back to the mark on exit.
    fn temp_mark(&self) -> usize;
    fn temp_push(&mut self, cell: Cell<T>);
    fn temp_release(&mut self, mark: usize);

    /// Frees everything not reachable from the roots or the shadow stack.
    fn force_gc(&mut self);

    /// Number of allocations after which the next allocation collects first.
    fn set_gc_threshold(&mut self, threshold: usize);
}

#[derive(Debug)]
pub struct Cell<T> {
    ptr: *mut T,
    marked: bool,
}

pub struct Heap<T> {
    cells: HashSet<Cell<T>>,
    roots: HashMap<*mut T, usize>, // cell -> times rooted
    permanent: Vec<Cell<T>>,       // interned constants: always live, never swept
    temps: Vec<Cell<T>>,           // shadow stack of in-flight temporaries
    marked_cells: HashSet<*mut T>, // Track marked cells by pointer
    allocation_count: usize,       // Track allocations since last GC
    gc_threshold: usize,           // Trigger GC after this many allocations
}

impl<T: Markable<T>> Allocator<T> for Heap<T> {
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

    fn intern(&mut self, value: T) -> Cell<T> {
        let ptr = Box::into_raw(Box::new(value));
        let cell = Cell { ptr, marked: false };
        self.cells.insert(cell);
        self.permanent.push(cell);
        cell
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

    fn temp_mark(&self) -> usize {
        self.temps.len()
    }

    fn temp_push(&mut self, cell: Cell<T>) {
        self.temps.push(cell);
    }

    fn temp_release(&mut self, mark: usize) {
        self.temps.truncate(mark);
    }

    fn force_gc(&mut self) {
        self.collect_garbage();
        self.allocation_count = 0;
    }

    fn set_gc_threshold(&mut self, threshold: usize) {
        self.gc_threshold = threshold;
    }
}

impl<T: Markable<T>> Heap<T> {
    pub fn new() -> Self {
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

    pub fn free_cell(&mut self, cell: Cell<T>) {
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

    pub fn collect_garbage(&mut self) {
        self.mark_live_cells();
        self.sweep_unmarked_objects();
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

    pub fn is_marked(&self, cell: &Cell<T>) -> bool {
        self.marked_cells.contains(&cell.ptr)
    }

    pub fn get_cells_count(&self) -> usize {
        self.cells.len()
    }

    pub fn get_roots_count(&self) -> usize {
        self.roots.len()
    }

    pub fn get_allocation_count(&self) -> usize {
        self.allocation_count
    }
}

impl<T> Drop for Heap<T> {
    // `cells` solely owns every box, interned ones included, so draining it
    // frees each exactly once.
    fn drop(&mut self) {
        for cell in self.cells.drain() {
            drop(unsafe { Box::from_raw(cell.ptr) });
        }
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
