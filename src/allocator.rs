/*
    allocator.rs contains a mark-sweep heap. Values live in an indexable grid of
    cells; a collection traces from the roots and empties whatever it did not
    reach.
*/

use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

/// Enumerates the references a value holds, so a collector can reach everything
/// live from a set of roots.
///
/// Every variant that can hold a reference must be listed. A wildcard arm that
/// returns nothing makes the collector treat those references as unreachable and
/// free values that are still in use.
pub trait Markable<T> {
    fn get_references(&self) -> Vec<CellView<T>>;
}

pub trait Allocator<T> {
    fn allocate_cell(&mut self, value: T) -> CellView<T>;

    /// Allocates a value that is never collected, for constants shared across
    /// the whole program.
    fn intern(&mut self, value: T) -> CellView<T>;

    /// Returns `None` for a view whose cell has been freed or refilled.
    fn read_cell(&self, view: CellView<T>) -> Option<&T>;
    fn read_mut_cell(&mut self, view: CellView<T>) -> Option<&mut T>;

    /// Roots are refcounted: a value reachable through several owners needs one
    /// `remove_root` per `add_root` before it can be collected.
    fn add_root(&mut self, view: CellView<T>);
    fn remove_root(&mut self, view: &CellView<T>);

    /// Shadow stack for values that are live but not reachable from any root.
    /// Push on entry, truncate back to the mark on exit.
    fn temp_mark(&self) -> usize;
    fn temp_push(&mut self, view: CellView<T>);
    fn temp_release(&mut self, mark: usize);

    /// Frees everything not reachable from the roots or the shadow stack.
    fn force_gc(&mut self);

    /// Number of allocations after which the next allocation collects first.
    fn set_gc_threshold(&mut self, threshold: usize);
}

/// One position in the grid.
struct Cell<T> {
    /// Incremented whenever the position is emptied, so views held by the
    /// previous occupant stop resolving.
    version: u32,
    value: Option<T>,
}

/// Names a cell. Holds no data of its own, so copies are free and a value can be
/// named from several places at once.
pub struct CellView<T> {
    index: u32,
    version: u32,
    _marker: PhantomData<T>,
}

pub struct Heap<T> {
    cells: Vec<Cell<T>>,
    free: Vec<u32>,              // positions available for reuse
    roots: HashMap<u32, usize>,  // position -> times rooted
    permanent: Vec<u32>,         // interned positions, never collected
    temps: Vec<CellView<T>>,
    marked: HashSet<u32>,
    allocation_count: usize,
    gc_threshold: usize,
}

impl<T: Markable<T>> Allocator<T> for Heap<T> {
    fn allocate_cell(&mut self, value: T) -> CellView<T> {
        if self.allocation_count >= self.gc_threshold {
            self.collect_garbage();
            self.allocation_count = 0;
        }
        self.allocation_count += 1;
        self.fill_cell(value)
    }

    fn intern(&mut self, value: T) -> CellView<T> {
        let view = self.fill_cell(value);
        self.permanent.push(view.index);
        view
    }

    fn read_cell(&self, view: CellView<T>) -> Option<&T> {
        let cell = self.cells.get(view.index as usize)?;
        if cell.version != view.version {
            return None;
        }
        cell.value.as_ref()
    }

    fn read_mut_cell(&mut self, view: CellView<T>) -> Option<&mut T> {
        let cell = self.cells.get_mut(view.index as usize)?;
        if cell.version != view.version {
            return None;
        }
        cell.value.as_mut()
    }

    fn add_root(&mut self, view: CellView<T>) {
        *self.roots.entry(view.index).or_insert(0) += 1;
    }

    fn remove_root(&mut self, view: &CellView<T>) {
        if let Some(count) = self.roots.get_mut(&view.index) {
            *count -= 1;
            if *count == 0 {
                self.roots.remove(&view.index);
            }
        }
    }

    fn temp_mark(&self) -> usize {
        self.temps.len()
    }

    fn temp_push(&mut self, view: CellView<T>) {
        self.temps.push(view);
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
            cells: Vec::new(),
            free: Vec::new(),
            roots: HashMap::default(),
            permanent: Vec::new(),
            temps: Vec::new(),
            marked: HashSet::default(),
            allocation_count: 0,
            gc_threshold: 100,
        }
    }

    /// Empties the cell and bumps its version, so every outstanding view of it
    /// stops resolving and the position can be reused.
    pub fn free_cell(&mut self, view: CellView<T>) {
        let index = view.index as usize;
        match self.cells.get_mut(index) {
            Some(cell) if cell.version == view.version && cell.value.is_some() => {
                cell.value = None;
                cell.version += 1;
                self.free.push(view.index);
            }
            _ => (),
        }
    }

    pub fn collect_garbage(&mut self) {
        self.mark_live_cells();
        self.sweep_unmarked_objects();
    }

    pub fn is_marked(&self, view: &CellView<T>) -> bool {
        self.marked.contains(&view.index)
    }

    pub fn get_cells_count(&self) -> usize {
        self.cells.len() - self.free.len()
    }

    pub fn get_roots_count(&self) -> usize {
        self.roots.len()
    }

    pub fn get_allocation_count(&self) -> usize {
        self.allocation_count
    }

    fn fill_cell(&mut self, value: T) -> CellView<T> {
        match self.free.pop() {
            Some(index) => {
                let cell = &mut self.cells[index as usize];
                cell.value = Some(value);
                CellView::new(index, cell.version)
            }
            None => {
                self.cells.push(Cell {
                    version: 0,
                    value: Some(value),
                });
                CellView::new((self.cells.len() - 1) as u32, 0)
            }
        }
    }

    fn mark_live_cells(&mut self) {
        self.marked.clear();
        let mut start: Vec<u32> = self.roots.keys().copied().collect();
        start.extend(self.permanent.iter().copied());
        start.extend(self.temps.iter().map(|view| view.index));
        for index in start {
            self.mark_recursive(index);
        }
    }

    /// Explicit worklist rather than recursion, so a deep object graph cannot
    /// overflow the stack.
    fn mark_recursive(&mut self, index: u32) {
        let mut work = vec![index];
        while let Some(index) = work.pop() {
            if self.marked.contains(&index) {
                continue;
            }
            let references = match self.cells.get(index as usize) {
                Some(cell) => match &cell.value {
                    Some(value) => value.get_references(),
                    None => continue,
                },
                None => continue,
            };
            self.marked.insert(index);
            for view in references {
                // A stale view names a cell that was already emptied, so its
                // contents are not reachable through it.
                if self.holds(&view) {
                    work.push(view.index);
                }
            }
        }
    }

    fn sweep_unmarked_objects(&mut self) {
        for index in 0..self.cells.len() as u32 {
            if self.marked.contains(&index) {
                continue;
            }
            let cell = &mut self.cells[index as usize];
            if cell.value.is_some() {
                cell.value = None;
                cell.version += 1;
                self.free.push(index);
            }
        }
    }

    fn holds(&self, view: &CellView<T>) -> bool {
        match self.cells.get(view.index as usize) {
            Some(cell) => cell.version == view.version && cell.value.is_some(),
            None => false,
        }
    }
}

impl<T> CellView<T> {
    fn new(index: u32, version: u32) -> Self {
        Self {
            index,
            version,
            _marker: PhantomData,
        }
    }
}

// Written out rather than derived: deriving would demand the same bounds of T,
// which a view never touches.
impl<T> PartialEq for CellView<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index && self.version == other.version
    }
}

impl<T> Eq for CellView<T> {}

impl<T> std::hash::Hash for CellView<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.version.hash(state);
    }
}

impl<T> Clone for CellView<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for CellView<T> {}

impl<T> std::fmt::Debug for CellView<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CellView({}, v{})", self.index, self.version)
    }
}
