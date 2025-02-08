/*
    allocator.rs contains the Heap allocation logic for the GC
*/

use std::collections::HashSet;
use std::hash::Hash;
// use std::mem::size_of;

pub trait Allocator<T> {
    fn new() -> Self;
    fn allocate_cell(&mut self, valute: T) -> Cell<T>;
    fn free_cell(&mut self, cell: Cell<T>);
    fn collect_garbage(&self);
    fn make_live_cells(&mut self);
    fn view_cell(&self, cell: Cell<T>) -> Option<&T>;
    fn view_mut_cell(&mut self, cell: Cell<T>) -> Option<&mut T>;
}

#[derive(Debug)]
pub struct Cell<T> {
    ptr: *mut T,
}

pub struct Heap<T> {
    cells: HashSet<Cell<T>>,
}

impl<T> Allocator<T> for Heap<T> {
    fn new() -> Self {
        Self {
            cells: HashSet::default(),
        }
    }

    fn allocate_cell(&mut self, value: T) -> Cell<T> {
        let ptr = Box::into_raw(Box::new(value));
        let cell = Cell { ptr };
        // println!(
        //     "allocated type {} of size {}",
        //     std::any::type_name::<T>(),
        //     size_of::<T>()
        // );

        self.cells.insert(cell);

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

    fn collect_garbage(&self) {
        println!("-- gc begin");
        // TODO: implement gc collection
        print!("-- gc end");
    }

    fn make_live_cells(&mut self) {
        todo!()
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
        Self { ptr: self.ptr }
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
