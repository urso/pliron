use crate::context::{ArenaObj, Context, Ptr};

/// An object that contains a linked list.
pub trait ContainsLinkedList<T: LinkedList> {
    /// Simply get the head of the list.
    fn get_head(&self) -> Option<Ptr<T>>;
    /// Simply get the tail of the list.
    fn get_tail(&self) -> Option<Ptr<T>>;
    /// Simply set the head pointer.
    fn set_head(&mut self, head: Option<Ptr<T>>);
    /// Simply set the tail pointer
    fn set_tail(&mut self, tail: Option<Ptr<T>>);
    /// Get an iterator over the items. Context is borrowed throughout.
    fn iter<'a>(&self, ctx: &'a Context) -> LinkedListIter<'a, T> {
        LinkedListIter {
            cur: self.get_head(),
            ctx,
        }
    }
}

pub struct LinkedListIter<'a, T: LinkedList> {
    cur: Option<Ptr<T>>,
    ctx: &'a Context,
}

impl<'a, T> Iterator for LinkedListIter<'a, T>
where
    T: LinkedList,
{
    type Item = Ptr<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let cur_opt = self.cur;
        match cur_opt {
            Some(cur) => {
                self.cur = cur.deref(self.ctx).get_next();
                cur_opt
            }
            None => None,
        }
    }
}

/// Implements a linked list based on [Ptr]
/// Types implementing this trait must provide simple
/// getters and setters for prev and next fields.
/// Actual linked list operations are implemented for Ptr<T: LinkedList>
pub trait LinkedList: ArenaObj + PartialEq {
    type ContainerType: ContainsLinkedList<Self> + ArenaObj;

    /// Simple getter for the item previous to this in the list.
    fn get_next(&self) -> Option<Ptr<Self>>;
    /// Simple getter for the item previous to this in the list.
    fn get_prev(&self) -> Option<Ptr<Self>>;
    /// Simply set the item next to this in the list.
    fn set_next(&mut self, next: Option<Ptr<Self>>);
    /// Simply set the item previous to this in the list.
    fn set_prev(&mut self, prev: Option<Ptr<Self>>);
    /// Get a reference to the object that contains this linked list.
    fn get_container(&self) -> Option<Ptr<Self::ContainerType>>;
    /// Set the container for this node.
    fn set_container(&mut self, container: Option<Ptr<Self::ContainerType>>);
}

/// Linked list operations on Ptr<T: LinkedList> for convenience and safety.
impl<T> Ptr<T>
where
    T: LinkedList,
{
    /// Insert self after mark.
    pub fn insert_after(&self, ctx: &Context, mark: Ptr<T>) {
        {
            let node = self.deref(ctx);
            debug_assert!(
                node.get_prev().is_none()
                    && node.get_next().is_none()
                    && node.get_container().is_none(),
                "LinkedList node must be unlinked before relinking"
            );
            let mark = mark.deref(ctx);
            debug_assert!(
                mark.get_container().is_some(),
                "insert_after: Mark node itself is unlinked"
            );
        }
        let next;
        let container;
        // If mark == *self, we don't want two deref_mut() on it.
        {
            let mut mark_ref = mark.deref_mut(ctx);
            container = mark_ref.get_container().unwrap();
            next = mark_ref.get_next();
            match next {
                Some(next) => {
                    debug_assert!(next.deref(ctx).get_prev().unwrap() == mark);
                    next.deref_mut(ctx).set_prev(Some(*self));
                }
                None => {
                    debug_assert!(container.deref(ctx).get_tail().unwrap() == mark);
                    container.deref_mut(ctx).set_tail(Some(*self));
                }
            }
            (*mark_ref).set_next(Some(*self));
        }

        let mut node = self.deref_mut(ctx);
        node.set_next(next);
        node.set_prev(Some(mark));
        node.set_container(Some(container));
    }

    /// Insert self before mark.
    pub fn insert_before(&self, ctx: &Context, mark: Ptr<T>) {
        {
            let node = self.deref(ctx);
            debug_assert!(
                node.get_prev().is_none()
                    && node.get_next().is_none()
                    && node.get_container().is_none(),
                "LinkedList node must be unlinked before relinking"
            );
            let mark = mark.deref(ctx);
            debug_assert!(
                mark.get_container().is_some(),
                "insert_before: Mark node itself is unlinked"
            );
        }

        let container;
        let prev;
        // If mark == *self, we don't want two deref_mut() on it.
        {
            let mut mark_ref = mark.deref_mut(ctx);
            container = mark_ref.get_container().unwrap();
            prev = mark_ref.get_prev();
            match prev {
                Some(prev) => {
                    debug_assert!(
                        prev.deref(ctx).get_next().unwrap() == mark.deref(ctx).get_self_ptr(ctx)
                    );
                    prev.deref_mut(ctx).set_next(Some(*self));
                }
                None => {
                    debug_assert!(container.deref(ctx).get_head().unwrap() == mark);
                    container.deref_mut(ctx).set_head(Some(*self));
                }
            }
            (*mark_ref).set_prev(Some(*self));
        }

        let mut node = self.deref_mut(ctx);
        node.set_prev(prev);
        node.set_next(Some(mark));
        node.set_container(Some(container));
    }

    /// Insert self as the head of the list.
    pub fn insert_at_front(&self, container: Ptr<T::ContainerType>, ctx: &Context) {
        let mut node = self.deref_mut(ctx);
        debug_assert!(
            node.get_prev().is_none()
                && node.get_next().is_none()
                && node.get_container().is_none(),
            "LinkedList node must be unlinked before relinking"
        );
        let mut container_ref = container.deref_mut(ctx);
        let head = container_ref.get_head();
        match head {
            Some(head) => {
                debug_assert!(head.deref(ctx).get_prev().is_none());
                head.deref_mut(ctx).set_prev(Some(*self))
            }
            None => {
                container_ref.set_tail(Some(*self));
            }
        }
        node.set_next(head);
        container_ref.set_head(Some(*self));
        node.set_container(Some(container));
    }

    /// Insert self as the tail of the list.
    pub fn insert_at_back(&self, container: Ptr<T::ContainerType>, ctx: &Context) {
        let mut node = self.deref_mut(ctx);
        debug_assert!(
            node.get_prev().is_none()
                && node.get_next().is_none()
                && node.get_container().is_none(),
            "LinkedList node must be unlinked before relinking"
        );
        let mut container_ref = container.deref_mut(ctx);
        let tail = container_ref.get_tail();
        match tail {
            Some(tail) => {
                debug_assert!(tail.deref(ctx).get_next().is_none());
                tail.deref_mut(ctx).set_next(Some(*self));
            }
            None => {
                container_ref.set_head(Some(*self));
            }
        }
        node.set_prev(tail);
        container_ref.set_tail(Some(*self));
        node.set_container(Some(container));
    }

    /// Unlink self from list.
    pub fn remove(&self, ctx: &Context) {
        let container = self.deref(ctx).get_container();
        debug_assert!(
            container.is_some(),
            "LinkedList: Attempt to remove unlinked node"
        );
        let container = container.unwrap();
        match self.deref(ctx).get_next() {
            Some(next) => next.deref_mut(ctx).set_prev(self.deref(ctx).get_prev()),
            None => {
                container
                    .deref_mut(ctx)
                    .set_tail(self.deref(ctx).get_prev());
            }
        }
        match self.deref(ctx).get_prev() {
            Some(prev) => {
                prev.deref_mut(ctx).set_prev(self.deref(ctx).get_next());
            }
            None => {
                container
                    .deref_mut(ctx)
                    .set_head(self.deref(ctx).get_next());
            }
        }

        let mut node = self.deref_mut(ctx);
        node.set_next(None);
        node.set_prev(None);
        node.set_container(None);
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::{ContainsLinkedList, LinkedList};
    use crate::context::{ArenaCell, ArenaObj, Context, Ptr};

    #[derive(PartialEq)]
    struct LLNode {
        data: u64,
        next: Option<Ptr<LLNode>>,
        prev: Option<Ptr<LLNode>>,
        parent: Option<Ptr<LLRoot>>,
        self_ptr: Ptr<LLNode>,
    }
    impl ArenaObj for LLNode {
        fn get_arena(ctx: &Context) -> &ArenaCell<Self> {
            &ctx.linked_list_store.nodes
        }

        fn get_arena_mut(ctx: &mut Context) -> &mut ArenaCell<Self> {
            &mut ctx.linked_list_store.nodes
        }

        fn get_self_ptr(&self, _ctx: &Context) -> Ptr<Self> {
            self.self_ptr
        }

        fn dealloc_sub_objects(_ptr: Ptr<Self>, _ctx: &mut Context) {}

        fn remove_references(_ptr: Ptr<Self>, _ctx: &mut Context) {}
    }

    impl LLNode {
        pub fn new(ctx: &mut Context, data: u64) -> Ptr<LLNode> {
            let f = |self_ptr: Ptr<LLNode>| LLNode {
                self_ptr,
                data,
                next: None,
                prev: None,
                parent: None,
            };
            Self::alloc(ctx, f)
        }
    }

    impl LinkedList for LLNode {
        type ContainerType = LLRoot;

        fn get_next(&self) -> Option<Ptr<Self>> {
            self.next
        }

        fn get_prev(&self) -> Option<Ptr<Self>> {
            self.prev
        }

        fn set_next(&mut self, next: Option<Ptr<Self>>) {
            self.next = next;
        }

        fn set_prev(&mut self, prev: Option<Ptr<Self>>) {
            self.prev = prev;
        }

        fn get_container(&self) -> Option<Ptr<Self::ContainerType>> {
            self.parent
        }

        fn set_container(&mut self, container: Option<Ptr<Self::ContainerType>>) {
            self.parent = container;
        }
    }

    struct LLRoot {
        first: Option<Ptr<LLNode>>,
        last: Option<Ptr<LLNode>>,
        self_ptr: Ptr<LLRoot>,
    }

    impl LLRoot {
        pub fn empty(ctx: &mut Context) -> Ptr<LLRoot> {
            let f = |self_ptr: Ptr<LLRoot>| LLRoot {
                self_ptr,
                first: None,
                last: None,
            };
            Self::alloc(ctx, f)
        }
    }

    impl ArenaObj for LLRoot {
        fn get_arena(ctx: &Context) -> &ArenaCell<Self> {
            &ctx.linked_list_store.containers
        }

        fn get_arena_mut(ctx: &mut Context) -> &mut ArenaCell<Self> {
            &mut ctx.linked_list_store.containers
        }

        fn get_self_ptr(&self, _ctx: &Context) -> Ptr<Self> {
            self.self_ptr
        }

        fn dealloc_sub_objects(_ptr: Ptr<Self>, _ctx: &mut Context) {}

        fn remove_references(_ptr: Ptr<Self>, _ctx: &mut Context) {}
    }

    impl ContainsLinkedList<LLNode> for LLRoot {
        fn get_head(&self) -> Option<Ptr<LLNode>> {
            self.first
        }

        fn get_tail(&self) -> Option<Ptr<LLNode>> {
            self.last
        }

        fn set_head(&mut self, head: Option<Ptr<LLNode>>) {
            self.first = head;
        }

        fn set_tail(&mut self, tail: Option<Ptr<LLNode>>) {
            self.last = tail;
        }
    }

    #[derive(Default)]
    pub(crate) struct LinkedListTestArena {
        nodes: ArenaCell<LLNode>,
        containers: ArenaCell<LLRoot>,
    }

    fn validate_list(ctx: &Context, root: Ptr<LLRoot>, gold: Vec<u64>) {
        let root: Vec<_> = root
            .deref(&ctx)
            .iter(&ctx)
            .map(|n| n.deref(&ctx).data)
            .collect();

        assert!(
            root == gold,
            "\nExpected: {:?}\nvs\nFound: {:?}",
            gold,
            root
        );
    }

    #[test]
    fn success() {
        let mut ctx = &mut Context::default();
        let root = LLRoot::empty(&mut ctx);

        let n1 = LLNode::new(ctx, 1);
        let n2 = LLNode::new(ctx, 2);
        let n3 = LLNode::new(ctx, 3);

        n1.insert_at_front(root, ctx);
        validate_list(ctx, root, vec![1]);

        n2.insert_after(ctx, n1);
        validate_list(ctx, root, vec![1, 2]);

        n1.remove(ctx);
        validate_list(ctx, root, vec![2]);
        n2.remove(ctx);
        validate_list(ctx, root, vec![]);

        n1.insert_at_back(root, ctx);
        validate_list(ctx, root, vec![1]);
        n2.insert_at_back(root, ctx);
        validate_list(ctx, root, vec![1, 2]);

        n1.remove(ctx);
        validate_list(ctx, root, vec![2]);
        n3.insert_before(ctx, n2);
        validate_list(ctx, root, vec![3, 2]);
    }

    #[test]
    #[should_panic(expected = "must be unlinked before relinking")]
    fn reinsert_panic() {
        let mut ctx = &mut Context::default();
        let root = LLRoot::empty(&mut ctx);

        let n1 = LLNode::new(ctx, 1);
        n1.insert_at_front(root, ctx);
        // Reinserting an exiting node must cause panic.
        n1.insert_at_front(root, ctx);
    }

    #[test]
    #[should_panic(expected = "Attempt to remove unlinked node")]
    fn uninserted_remove_panic() {
        let ctx = &mut Context::default();
        let n1 = LLNode::new(ctx, 1);
        // Removing an unlinked node must cause panic.
        n1.remove(ctx);
    }

    #[test]
    #[should_panic(expected = "Attempt to remove unlinked node")]
    fn reremove_panic() {
        let mut ctx = &mut Context::default();
        let root = LLRoot::empty(&mut ctx);

        let n1 = LLNode::new(ctx, 1);
        n1.insert_at_front(root, ctx);
        n1.remove(ctx);
        // Removing an unlinked node must cause panic.
        n1.remove(ctx);
    }

    #[test]
    #[should_panic(expected = " Mark node itself is unlinked")]
    fn insert_after_unlinked_panic() {
        let ctx = &mut Context::default();

        let n1 = LLNode::new(ctx, 1);
        let n2 = LLNode::new(ctx, 2);
        // n1 itself is unlinked, so this is a panic.
        n2.insert_after(ctx, n1);
    }

    #[test]
    #[should_panic(expected = " Mark node itself is unlinked")]
    fn insert_before_unlinked_panic() {
        let ctx = &mut Context::default();

        let n1 = LLNode::new(ctx, 1);
        let n2 = LLNode::new(ctx, 2);
        // n1 itself is unlinked, so this is a panic.
        n2.insert_before(ctx, n1);
    }
}
