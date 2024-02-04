use std::ops::ControlFlow;

use crate::{
    basic_block::BasicBlock, context::Context, linked_list::ContainsLinkedList,
    operation::Operation, region::Region,
};

type WalkResult<B> = ControlFlow<B, WalkCont>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WalkCont {
    Advance, // Advance the walk
    Next,    // Advance to the next sibling
}

#[inline]
pub fn walk_break<T>(v: T) -> WalkResult<T> {
    return WalkResult::Break(v);
}

#[inline]
pub fn walk_advance<T>() -> WalkResult<T> {
    return WalkResult::Continue(WalkCont::Advance);
}

#[inline]
pub fn walk_next<T>() -> WalkResult<T> {
    return WalkResult::Continue(WalkCont::Next);
}

fn walk<B, W, V>(ctx: &Context, root: &Operation, mut v: V) -> WalkResult<B>
where
    V: WalkerVisitor<B>,
    W: Default + Walker<B, V>,
{
    let w = W::default();
    w.walk_op(&mut v, ctx, root)
}

fn walk_with<B, V: Visitor<B>>(
    ctx: &Context,
    root: &Operation,
    order: WalkOrder,
    dir: WalkDirection,
    v: V,
) -> WalkResult<B> {
    match (order, dir) {
        (WalkOrder::PreOrder, WalkDirection::Forward) => {
            walk::<B, ForwardWalker, _>(ctx, root, PreOrderVisitor::build(v))
        }
        (WalkOrder::PreOrder, WalkDirection::Reverse) => {
            walk::<B, ReverseWalker, _>(ctx, root, PreOrderVisitor::build(v))
        }
        (WalkOrder::PostOrder, WalkDirection::Forward) => {
            walk::<B, ForwardWalker, _>(ctx, root, PostOrderVisitor::build(v))
        }
        (WalkOrder::PostOrder, WalkDirection::Reverse) => {
            walk::<B, ReverseWalker, _>(ctx, root, PostOrderVisitor::build(v))
        }
    }
}

enum WalkOrder {
    PreOrder,
    PostOrder,
}

enum WalkDirection {
    Forward,
    Reverse,
}

fn walk_ops<B, F>(
    ctx: &Context,
    root: &Operation,
    order: WalkOrder,
    dir: WalkDirection,
    c: &mut F,
) -> WalkResult<B>
where
    F: FnMut(&Context, &Operation) -> WalkResult<B>,
{
    walk_with(ctx, root, order, dir, Callback::new(c))
}

fn walk_regions<B, F>(
    ctx: &Context,
    root: &Operation,
    order: WalkOrder,
    dir: WalkDirection,
    c: &mut F,
) -> WalkResult<B>
where
    F: FnMut(&Context, &Region) -> WalkResult<B>,
{
    walk_with(ctx, root, order, dir, Callback::new(c))
}

fn walk_blocks<B, F>(
    ctx: &Context,
    root: &Operation,
    order: WalkOrder,
    dir: WalkDirection,
    c: &mut F,
) -> WalkResult<B>
where
    F: FnMut(&Context, &BasicBlock) -> WalkResult<B>,
{
    walk_with(ctx, root, order, dir, Callback::new(c))
}

struct Callback<IR, B, F: FnMut(&Context, &IR) -> WalkResult<B>>(F, std::marker::PhantomData<IR>);

impl<IR, B, F> Callback<IR, B, F>
where
    F: FnMut(&Context, &IR) -> WalkResult<B>,
{
    fn new(f: F) -> Self {
        Callback(f, std::marker::PhantomData)
    }

    #[inline]
    fn exec(&mut self, ctx: &Context, op: &IR) -> WalkResult<B> {
        (self.0)(ctx, op)
    }
}

impl<B, F> Visitor<B> for Callback<Operation, B, F>
where
    F: FnMut(&Context, &Operation) -> WalkResult<B>,
{
    #[inline]
    fn visit_op(&mut self, ctx: &Context, op: &Operation) -> WalkResult<B> {
        self.exec(ctx, op)
    }
}

impl<B, F> Visitor<B> for Callback<Region, B, F>
where
    F: FnMut(&Context, &Region) -> WalkResult<B>,
{
    #[inline]
    fn visit_region(&mut self, ctx: &Context, region: &Region) -> WalkResult<B> {
        self.exec(ctx, region)
    }
}

impl<B, F> Visitor<B> for Callback<BasicBlock, B, F>
where
    F: FnMut(&Context, &BasicBlock) -> WalkResult<B>,
{
    #[inline]
    fn visit_block(&mut self, ctx: &Context, block: &BasicBlock) -> WalkResult<B> {
        self.exec(ctx, block)
    }
}

trait Ordered<T, V: Visitor<T>>: WalkerVisitor<T> {
    fn build(v: V) -> Self;
}

struct PreOrderVisitor<T, V: Visitor<T>>(V, std::marker::PhantomData<T>);

impl<T, V: Visitor<T>> Ordered<T, V> for PreOrderVisitor<T, V> {
    #[inline]
    fn build(v: V) -> Self {
        PreOrderVisitor(v, std::marker::PhantomData)
    }
}

impl<T, V: Visitor<T>> WalkerVisitor<T> for PreOrderVisitor<T, V> {
    #[inline]
    fn op_pre_visit(&mut self, ctx: &Context, op: &Operation) -> WalkResult<T> {
        self.0.visit_op(ctx, op)
    }

    #[inline]
    fn region_pre_visit(&mut self, ctx: &Context, region: &Region) -> WalkResult<T> {
        self.0.visit_region(ctx, region)
    }

    #[inline]
    fn block_pre_visit(&mut self, ctx: &Context, block: &BasicBlock) -> WalkResult<T> {
        self.0.visit_block(ctx, block)
    }
}

struct PostOrderVisitor<T, V: Visitor<T>>(V, std::marker::PhantomData<T>);

impl<T, V: Visitor<T>> Ordered<T, V> for PostOrderVisitor<T, V> {
    #[inline]
    fn build(v: V) -> Self {
        PostOrderVisitor(v, std::marker::PhantomData)
    }
}

impl<T, V: Visitor<T>> WalkerVisitor<T> for PostOrderVisitor<T, V> {
    #[inline]
    fn op_post_visit(&mut self, ctx: &Context, op: &Operation) -> WalkResult<T> {
        self.0.visit_op(ctx, op)
    }

    #[inline]
    fn region_post_visit(&mut self, ctx: &Context, region: &Region) -> WalkResult<T> {
        self.0.visit_region(ctx, region)
    }

    #[inline]
    fn block_post_visit(&mut self, ctx: &Context, block: &BasicBlock) -> WalkResult<T> {
        self.0.visit_block(ctx, block)
    }
}

trait Visitor<T = ()> {
    fn visit_op(&mut self, _ctx: &Context, _op: &Operation) -> WalkResult<T> {
        walk_advance()
    }

    fn visit_region(&mut self, _ctx: &Context, _region: &Region) -> WalkResult<T> {
        walk_advance()
    }

    fn visit_block(&mut self, _ctx: &Context, _block: &BasicBlock) -> WalkResult<T> {
        walk_advance()
    }
}

trait WalkerVisitor<T = ()> {
    #[inline]
    fn op_pre_visit(&mut self, _ctx: &Context, _op: &Operation) -> WalkResult<T> {
        walk_advance()
    }

    #[inline]
    fn op_post_visit(&mut self, _ctx: &Context, _op: &Operation) -> WalkResult<T> {
        walk_advance()
    }

    #[inline]
    fn region_pre_visit(&mut self, _ctx: &Context, _region: &Region) -> WalkResult<T> {
        walk_advance()
    }

    #[inline]
    fn region_post_visit(&mut self, _ctx: &Context, _region: &Region) -> WalkResult<T> {
        walk_advance()
    }

    #[inline]
    fn block_pre_visit(&mut self, _ctx: &Context, _block: &BasicBlock) -> WalkResult<T> {
        walk_advance()
    }

    #[inline]
    fn block_post_visit(&mut self, _ctx: &Context, _block: &BasicBlock) -> WalkResult<T> {
        walk_advance()
    }
}

#[derive(Default)]
struct ForwardWalker;
impl<T, V: WalkerVisitor<T>> Walker<T, V> for ForwardWalker {}

#[derive(Default)]
struct ReverseWalker;
impl<T, V: WalkerVisitor<T>> Walker<T, V> for ReverseWalker {
    #[inline]
    fn walk_regions(&self, v: &mut V, ctx: &Context, op: &Operation) -> WalkResult<T> {
        for ptr in op.regions.iter().rev() {
            self.walk_region(v, ctx, &ptr.deref(ctx))?;
        }
        walk_advance()
    }

    #[inline]
    fn walk_blocks(&self, v: &mut V, ctx: &Context, region: &Region) -> WalkResult<T> {
        for ptr in region.iter(ctx).rev() {
            self.walk_block(v, ctx, &ptr.deref(ctx))?;
        }
        walk_advance()
    }

    #[inline]
    fn walk_ops(&self, v: &mut V, ctx: &Context, block: &BasicBlock) -> WalkResult<T> {
        for ptr in block.iter(ctx).rev() {
            self.walk_op(v, ctx, &ptr.deref(ctx))?;
        }
        walk_advance()
    }
}

trait Walker<T, V: WalkerVisitor<T>> {
    #[inline]
    fn walk_op(&self, v: &mut V, ctx: &Context, op: &Operation) -> WalkResult<T> {
        let c = v.op_pre_visit(ctx, op)?;
        if c == WalkCont::Advance {
            self.walk_regions(v, ctx, op)?;
        }
        v.op_post_visit(ctx, op)?;
        walk_advance()
    }

    #[inline]
    fn walk_regions(&self, v: &mut V, ctx: &Context, op: &Operation) -> WalkResult<T> {
        for ptr in op.regions.iter() {
            self.walk_region(v, ctx, &ptr.deref(ctx))?;
        }
        walk_advance()
    }

    #[inline]
    fn walk_region(&self, v: &mut V, ctx: &Context, region: &Region) -> WalkResult<T> {
        let c = v.region_pre_visit(ctx, &region)?;
        if c == WalkCont::Advance {
            self.walk_blocks(v, ctx, region)?;
        }
        v.region_post_visit(ctx, &region)?;
        walk_advance()
    }

    #[inline]
    fn walk_blocks(&self, v: &mut V, ctx: &Context, region: &Region) -> WalkResult<T> {
        for ptr in region.iter(ctx) {
            self.walk_block(v, ctx, &ptr.deref(ctx))?;
        }
        walk_advance()
    }

    #[inline]
    fn walk_block(&self, v: &mut V, ctx: &Context, block: &BasicBlock) -> WalkResult<T> {
        let c = v.block_pre_visit(ctx, block)?;
        if c == WalkCont::Advance {
            self.walk_ops(v, ctx, block)?;
        }
        v.block_post_visit(ctx, block)
    }

    #[inline]
    fn walk_ops(&self, v: &mut V, ctx: &Context, block: &BasicBlock) -> WalkResult<T> {
        for ptr in block.iter(ctx) {
            self.walk_op(v, ctx, &ptr.deref(ctx))?;
        }
        walk_advance()
    }
}
