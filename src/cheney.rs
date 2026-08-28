/*
    cheney.rs contains a scaffold for a semispace copying collector.

    Not wired up: allocator.rs is still the live collector. This exists so the
    design can be built out and tested on its own before anything depends on it.

    Cheney's algorithm evacuates live objects from one space to another, using
    to-space itself as the work queue:

        flip                     to-space becomes from-space
        evacuate the roots       copying each into to-space
        scan forward             for every object copied, evacuate what it
                                 references, appending to to-space
        stop                     when the scan pointer reaches the end

    Everything before `scan` has had its references evacuated; everything
    between `scan` and the end has not. There is no separate mark phase, no
    mark bit, and no sweep: what is not copied is simply left behind. Cost is
    proportional to *live* data rather than heap size, which suits beesafe --
    measured survival is under 10%.

    Design note: handles are indirect. `Environment` stores handles in hash maps
    the heap cannot reach, so a collector that rewrote object addresses would
    leave those stale. Indirecting through a slot table means collection rewrites
    only the table, and a handle held anywhere stays valid. It also means objects
    need no reference *updating* -- their handles are already stable, so `Trace`
    only has to enumerate.
*/

/// Objects tell the collector which handles they hold. Same contract as
/// `Markable::get_references`, and it carries the same obligation: enumerate
/// every variant, no wildcard arm.
pub trait Trace {
    fn refs(&self) -> Vec<Handle>;
}

/// Stable reference to an object. Survives collection: the object moves, the
/// handle does not.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Handle {
    slot: u32,
}

#[derive(Debug, Clone, Copy)]
struct Slot {
    /// Index into the live space, or None once the object is unreachable.
    index: Option<usize>,
}

pub struct CheneyHeap<T> {
    /// Live objects, densely packed. Allocation is a push; collection rebuilds
    /// this compacted, so there are no holes to reuse or fragment.
    space: Vec<T>,
    /// handle slot -> index in `space`.
    slots: Vec<Slot>,
    /// `space` index -> owning handle slot, so collection can rebuild the table.
    owner: Vec<u32>,
    free_slots: Vec<u32>,
}

impl<T: Trace> CheneyHeap<T> {
    pub fn new() -> Self {
        Self {
            space: Vec::new(),
            slots: Vec::new(),
            owner: Vec::new(),
            free_slots: Vec::new(),
        }
    }

    pub fn allocate(&mut self, value: T) -> Handle {
        let index = self.space.len();
        self.space.push(value);

        let slot = match self.free_slots.pop() {
            Some(slot) => {
                self.slots[slot as usize].index = Some(index);
                slot
            }
            None => {
                self.slots.push(Slot { index: Some(index) });
                (self.slots.len() - 1) as u32
            }
        };
        self.owner.push(slot);
        Handle { slot }
    }

    pub fn get(&self, handle: Handle) -> Option<&T> {
        self.slots
            .get(handle.slot as usize)
            .and_then(|s| s.index)
            .map(|i| &self.space[i])
    }

    pub fn get_mut(&mut self, handle: Handle) -> Option<&mut T> {
        match self.slots.get(handle.slot as usize).and_then(|s| s.index) {
            Some(i) => Some(&mut self.space[i]),
            None => None,
        }
    }

    pub fn live_count(&self) -> usize {
        self.space.len()
    }

    /// Evacuate everything reachable from `roots` into a fresh space.
    ///
    /// Roots must be complete: anything the program will still use has to appear
    /// here or be reachable from something that does. In beesafe that means
    /// environment bindings, interned constants, and the shadow stack.
    pub fn collect(&mut self, roots: &[Handle]) {
        let from_owner = std::mem::take(&mut self.owner);
        let mut from: Vec<Option<T>> = std::mem::take(&mut self.space)
            .into_iter()
            .map(Some)
            .collect();

        let mut to: Vec<T> = Vec::with_capacity(from.len());
        let mut to_owner: Vec<u32> = Vec::with_capacity(from.len());
        // from-space index -> to-space index, the forwarding pointer.
        let mut forwarded: Vec<Option<usize>> = vec![None; from.len()];

        for &root in roots {
            evacuate(
                root,
                &self.slots,
                &from_owner,
                &mut from,
                &mut to,
                &mut to_owner,
                &mut forwarded,
            );
        }

        // to-space is the queue. Objects appended past `scan` still owe us their
        // references; the loop ends when copying stops adding any.
        let mut scan = 0;
        while scan < to.len() {
            for handle in to[scan].refs() {
                evacuate(
                    handle,
                    &self.slots,
                    &from_owner,
                    &mut from,
                    &mut to,
                    &mut to_owner,
                    &mut forwarded,
                );
            }
            scan += 1;
        }

        // Repoint the table at to-space. Anything not copied is unreachable, so
        // its slot is released for reuse.
        for slot in self.slots.iter_mut() {
            slot.index = None;
        }
        for (index, &slot) in to_owner.iter().enumerate() {
            self.slots[slot as usize].index = Some(index);
        }
        self.free_slots = (0..self.slots.len() as u32)
            .filter(|&s| self.slots[s as usize].index.is_none())
            .collect();

        self.space = to;
        self.owner = to_owner;
    }
}

/// Copy one object into to-space if it is not already there. Idempotent: the
/// forwarding table makes repeat visits, and so cycles, terminate.
fn evacuate<T>(
    handle: Handle,
    slots: &[Slot],
    from_owner: &[u32],
    from: &mut [Option<T>],
    to: &mut Vec<T>,
    to_owner: &mut Vec<u32>,
    forwarded: &mut [Option<usize>],
) {
    let old = match slots.get(handle.slot as usize).and_then(|s| s.index) {
        Some(old) => old,
        None => return,
    };
    if forwarded[old].is_some() {
        return;
    }
    let object = match from[old].take() {
        Some(object) => object,
        None => return,
    };
    forwarded[old] = Some(to.len());
    to.push(object);
    to_owner.push(from_owner[old]);
}

#[cfg(test)]
mod tests {
    use super::*;

    enum Node {
        Leaf(i32),
        Pair(Handle, Handle),
    }

    impl Trace for Node {
        fn refs(&self) -> Vec<Handle> {
            match self {
                Node::Leaf(_) => Vec::new(),
                Node::Pair(a, b) => vec![*a, *b],
            }
        }
    }

    fn leaf(heap: &CheneyHeap<Node>, handle: Handle) -> i32 {
        match heap.get(handle) {
            Some(Node::Leaf(n)) => *n,
            _ => panic!("expected a leaf"),
        }
    }

    #[test]
    fn collects_unreachable_and_keeps_handles_valid() {
        let mut heap = CheneyHeap::new();
        let a = heap.allocate(Node::Leaf(1));
        let b = heap.allocate(Node::Leaf(2));
        let _garbage = heap.allocate(Node::Leaf(99));
        let pair = heap.allocate(Node::Pair(a, b));

        assert_eq!(heap.live_count(), 4);
        heap.collect(&[pair]);

        // The garbage is gone, the reachable children came with the parent, and
        // the handles still resolve even though the objects moved.
        assert_eq!(heap.live_count(), 3);
        assert_eq!(leaf(&heap, a), 1);
        assert_eq!(leaf(&heap, b), 2);
        assert!(heap.get(_garbage).is_none());
    }

    #[test]
    fn cycles_terminate() {
        let mut heap = CheneyHeap::new();
        let leaf_a = heap.allocate(Node::Leaf(1));
        let pair = heap.allocate(Node::Pair(leaf_a, leaf_a));
        // Point the pair at itself.
        if let Some(Node::Pair(_, second)) = heap.get_mut(pair) {
            *second = pair;
        }

        heap.collect(&[pair]);
        assert_eq!(heap.live_count(), 2);
        assert_eq!(leaf(&heap, leaf_a), 1);
    }

    #[test]
    fn slots_are_reused_after_collection() {
        let mut heap = CheneyHeap::new();
        let keep = heap.allocate(Node::Leaf(1));
        for _ in 0..10 {
            heap.allocate(Node::Leaf(0));
        }
        heap.collect(&[keep]);
        assert_eq!(heap.live_count(), 1);

        // Allocation after a collection reuses the released slots rather than
        // growing the table.
        let slots_before = heap.slots.len();
        for _ in 0..10 {
            heap.allocate(Node::Leaf(0));
        }
        assert_eq!(heap.slots.len(), slots_before);
    }
}
