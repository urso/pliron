use std::{marker::PhantomData, ops::ControlFlow};

use crate::{
    basic_block::BasicBlock,
    context::{Context, Ptr},
    linked_list::ContainsLinkedList,
    operation::Operation,
    region::Region,
};

pub type WalkResult<B> = ControlFlow<B, WalkCont>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WalkCont {
    Advance, // Advance the walk
    Skip,    // Advance to the next sibling
}

pub trait Visitor<S: Strategy, B> {
    fn visit_op(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        S::walk_op(self, ctx, op)
    }

    fn visit_region(&mut self, ctx: &Context, region: Ptr<Region>) -> WalkResult<B> {
        S::walk_region(self, ctx, region)
    }

    fn visit_block(&mut self, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        S::walk_block(self, ctx, block)
    }
}

pub trait Strategy {
    fn walk_op<V, B>(a: &mut V, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B>
    where
        V: Visitor<Self, B> + ?Sized,
        Self: Sized;

    fn walk_region<V, B>(a: &mut V, ctx: &Context, region: Ptr<Region>) -> WalkResult<B>
    where
        V: Visitor<Self, B> + ?Sized,
        Self: Sized;

    fn walk_block<V, B>(a: &mut V, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B>
    where
        V: Visitor<Self, B> + ?Sized,
        Self: Sized;
}

pub trait VisitorMut<B> {
    fn visit_op_mut(&mut self, ctx: &mut Context, op: Ptr<Operation>) -> WalkResult<B> {
        Forward::walk_op_mut(self, ctx, op)
    }

    fn visit_region_mut(&mut self, ctx: &mut Context, region: Ptr<Region>) -> WalkResult<B> {
        Forward::walk_region_mut(self, ctx, region)
    }

    fn visit_block_mut(&mut self, ctx: &mut Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        Forward::walk_block_mut(self, ctx, block)
    }
}

pub trait StrategyMut {
    fn walk_op_mut<V, B>(a: &mut V, ctx: &mut Context, op: Ptr<Operation>) -> WalkResult<B>
    where
        V: VisitorMut<B> + ?Sized;

    fn walk_region_mut<V, B>(a: &mut V, ctx: &mut Context, region: Ptr<Region>) -> WalkResult<B>
    where
        V: VisitorMut<B> + ?Sized;

    fn walk_block_mut<V, B>(a: &mut V, ctx: &mut Context, block: Ptr<BasicBlock>) -> WalkResult<B>
    where
        V: VisitorMut<B> + ?Sized;
}

struct Forward;

impl Strategy for Forward {
    fn walk_op<V, B>(a: &mut V, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B>
    where
        V: Visitor<Self, B> + ?Sized,
    {
        for ptr in op.deref(ctx).regions().iter().cloned() {
            a.visit_region(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }

    fn walk_region<V, B>(a: &mut V, ctx: &Context, region: Ptr<Region>) -> WalkResult<B>
    where
        V: Visitor<Self, B> + ?Sized,
    {
        for ptr in region.deref(ctx).iter(ctx) {
            a.visit_block(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }

    fn walk_block<V, B>(a: &mut V, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B>
    where
        V: Visitor<Self, B> + ?Sized,
    {
        for ptr in block.deref(ctx).iter(ctx) {
            a.visit_op(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }
}

impl StrategyMut for Forward {
    fn walk_op_mut<V, B>(a: &mut V, ctx: &mut Context, op: Ptr<Operation>) -> WalkResult<B>
    where
        V: VisitorMut<B> + ?Sized,
    {
        let iter = {
            let op = op.deref(ctx);
            op.regions().iter().cloned().collect::<Vec<_>>()
        };
        for ptr in iter {
            a.visit_region_mut(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }

    fn walk_region_mut<V, B>(a: &mut V, ctx: &mut Context, region: Ptr<Region>) -> WalkResult<B>
    where
        V: VisitorMut<B> + ?Sized,
    {
        let iter = {
            let region: &Region = &region.deref(ctx);
            region.iter(ctx).collect::<Vec<_>>()
        };
        for ptr in iter {
            a.visit_block_mut(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }

    fn walk_block_mut<V, B>(a: &mut V, ctx: &mut Context, block: Ptr<BasicBlock>) -> WalkResult<B>
    where
        V: VisitorMut<B> + ?Sized,
    {
        let iter = {
            let block: &BasicBlock = &block.deref(ctx);
            block.iter(ctx).collect::<Vec<_>>()
        };
        for ptr in iter {
            a.visit_op_mut(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }
}

struct Reverse;

impl Strategy for Reverse {
    fn walk_op<V, B>(a: &mut V, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B>
    where
        V: Visitor<Self, B> + ?Sized,
    {
        eprintln!("walk_regions");
        for ptr in op.deref(ctx).regions().iter().cloned().rev() {
            a.visit_region(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }

    fn walk_region<V, B>(a: &mut V, ctx: &Context, region: Ptr<Region>) -> WalkResult<B>
    where
        V: Visitor<Self, B> + ?Sized,
    {
        eprintln!("walk_blocks");
        for ptr in region.deref(ctx).iter(ctx).rev() {
            a.visit_block(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }

    fn walk_block<V, B>(a: &mut V, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B>
    where
        V: Visitor<Self, B> + ?Sized,
    {
        eprintln!("walk_ops");
        for ptr in block.deref(ctx).iter(ctx).rev() {
            eprintln!("visiting op: {}", ptr.deref(ctx).get_opid().to_string());

            a.visit_op(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }
}

impl StrategyMut for Reverse {
    fn walk_op_mut<V, B>(a: &mut V, ctx: &mut Context, op: Ptr<Operation>) -> WalkResult<B>
    where
        V: VisitorMut<B> + ?Sized,
    {
        let regions = {
            let op = op.deref(ctx);
            op.regions().iter().cloned().collect::<Vec<_>>()
        };
        for ptr in regions.into_iter().rev() {
            a.visit_region_mut(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }

    fn walk_region_mut<V, B>(a: &mut V, ctx: &mut Context, region: Ptr<Region>) -> WalkResult<B>
    where
        V: VisitorMut<B> + ?Sized,
    {
        let blocks = {
            let region: &Region = &region.deref(ctx);
            region.iter(ctx).collect::<Vec<_>>()
        };
        for ptr in blocks.into_iter().rev() {
            a.visit_block_mut(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }

    fn walk_block_mut<V, B>(a: &mut V, ctx: &mut Context, block: Ptr<BasicBlock>) -> WalkResult<B>
    where
        V: VisitorMut<B> + ?Sized,
    {
        let ops = {
            let block: &BasicBlock = &block.deref(ctx);
            block.iter(ctx).collect::<Vec<_>>()
        };
        for ptr in ops.into_iter().rev() {
            a.visit_op_mut(ctx, ptr)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }
}

// ------------------------------------------------------------------------------------------------
// Callback based walker functions
//

pub trait Order {}

pub struct PostOrder;
impl Order for PostOrder {}

pub struct PreOrder;
impl Order for PreOrder {}

pub fn walk_op<B, O, S, F>(root_op: Ptr<Operation>, ctx: &Context, callback: F) -> WalkResult<B>
where
    O: Order,
    S: Strategy,
    F: FnMut(&Context, Ptr<Operation>) -> WalkResult<B>,
    Callback<Operation, O, S, F>: Visitor<S, B>,
{
    Callback::<Operation, O, S, _>::new(callback).run(ctx, root_op)
}

pub fn walk_regions<B, O, S, F>(
    root_op: Ptr<Operation>,
    ctx: &Context,
    callback: F,
) -> WalkResult<B>
where
    O: Order,
    S: Strategy,
    F: FnMut(&Context, Ptr<Region>) -> WalkResult<B>,
    Callback<Region, O, S, F>: Visitor<S, B>,
{
    Callback::<Region, O, S, _>::new(callback).run(ctx, root_op)
}

pub fn walk_blocks<B, O, S, F>(root_op: Ptr<Operation>, ctx: &Context, callback: F) -> WalkResult<B>
where
    O: Order,
    S: Strategy,
    F: FnMut(&Context, Ptr<BasicBlock>) -> WalkResult<B>,
    Callback<BasicBlock, O, S, F>: Visitor<S, B>,
{
    Callback::<BasicBlock, O, S, _>::new(callback).run(ctx, root_op)
}

pub struct Callback<IR, O: Order, S: Strategy, F>(F, PhantomData<(IR, O, S)>);

impl<IR, O, S, F> Callback<IR, O, S, F>
where
    O: Order,
    S: Strategy,
{
    pub fn new(f: F) -> Self {
        Self(f, PhantomData)
    }
}

impl<IR, O, S, F, B> Callback<IR, O, S, F>
where
    O: Order,
    S: Strategy,
    F: FnMut(&Context, Ptr<IR>) -> WalkResult<B>,
    Self: Visitor<S, B>,
{
    pub fn run(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        <Self as Visitor<S, B>>::visit_op(self, ctx, op)
    }
}

impl<B, S: Strategy, F> Visitor<S, B> for Callback<Operation, PreOrder, S, F>
where
    F: FnMut(&Context, Ptr<Operation>) -> WalkResult<B>,
{
    fn visit_op(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        if (self.0)(ctx, op)? == WalkCont::Advance {
            S::walk_op(self, ctx, op)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }
}

impl<B, S: Strategy, F> Visitor<S, B> for Callback<Region, PreOrder, S, F>
where
    F: FnMut(&Context, Ptr<Region>) -> WalkResult<B>,
{
    fn visit_region(&mut self, ctx: &Context, r: Ptr<Region>) -> WalkResult<B> {
        if (self.0)(ctx, r)? == WalkCont::Advance {
            S::walk_region(self, ctx, r)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }
}

impl<B, S: Strategy, F> Visitor<S, B> for Callback<BasicBlock, PreOrder, S, F>
where
    F: FnMut(&Context, Ptr<BasicBlock>) -> WalkResult<B>,
{
    fn visit_block(&mut self, ctx: &Context, r: Ptr<BasicBlock>) -> WalkResult<B> {
        if (self.0)(ctx, r)? == WalkCont::Advance {
            S::walk_block(self, ctx, r)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }
}

impl<B, S: Strategy, F> Visitor<S, B> for Callback<Operation, PostOrder, S, F>
where
    F: FnMut(&Context, Ptr<Operation>) -> WalkResult<B>,
{
    fn visit_op(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        S::walk_op(self, ctx, op)?;
        (self.0)(ctx, op)
    }
}

impl<B, S: Strategy, F> Visitor<S, B> for Callback<Region, PostOrder, S, F>
where
    F: FnMut(&Context, Ptr<Region>) -> WalkResult<B>,
{
    fn visit_region(&mut self, ctx: &Context, op: Ptr<Region>) -> WalkResult<B> {
        S::walk_region(self, ctx, op)?;
        (self.0)(ctx, op)
    }
}

impl<B, S: Strategy, F> Visitor<S, B> for Callback<BasicBlock, PostOrder, S, F>
where
    F: FnMut(&Context, Ptr<BasicBlock>) -> WalkResult<B>,
{
    fn visit_block(&mut self, ctx: &Context, op: Ptr<BasicBlock>) -> WalkResult<B> {
        S::walk_block(self, ctx, op)?;
        (self.0)(ctx, op)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VisitOrder {
    PreOrder,
    PostOrder,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    Forward,
    Reverse,
}

pub fn walk_op_with_enums<B>(
    order: VisitOrder,
    direction: Direction,
    root: Ptr<Operation>,
    ctx: &Context,
    callback: impl FnMut(&Context, Ptr<Operation>) -> WalkResult<B>,
) -> WalkResult<B> {
    match (order, direction) {
        (VisitOrder::PreOrder, Direction::Forward) => {
            walk_op::<B, PreOrder, Forward, _>(root, ctx, callback)
        }
        (VisitOrder::PreOrder, Direction::Reverse) => {
            walk_op::<B, PreOrder, Reverse, _>(root, ctx, callback)
        }
        (VisitOrder::PostOrder, Direction::Forward) => {
            walk_op::<B, PostOrder, Forward, _>(root, ctx, callback)
        }
        (VisitOrder::PostOrder, Direction::Reverse) => {
            walk_op::<B, PostOrder, Reverse, _>(root, ctx, callback)
        }
    }
}

#[cfg(test)]
mod tests {
    use apint::ApInt;

    use crate::{
        debug_info::set_operation_result_name,
        dialects::{
            self,
            builtin::{
                attributes::IntegerAttr,
                op_interfaces::OneResultInterface,
                ops::{ConstantOp, FuncOp, ModuleOp},
                types::{FunctionType, IntegerType, Signedness},
            },
            llvm::ops::ReturnOp,
        },
        op::Op,
    };

    use super::*;

    // Create a print a module "bar", with a function "foo"
    // containing a single `return 0`.
    fn create_mod_op(ctx: &mut Context) -> ModuleOp {
        let i64_ty = IntegerType::get(ctx, 64, Signedness::Signed);
        let module = ModuleOp::new(ctx, "bar");
        // Our function is going to have type () -> ().
        let func_ty = FunctionType::get(ctx, vec![], vec![i64_ty]);
        let func = FuncOp::new_unlinked(ctx, "foo", func_ty);
        module.add_operation(ctx, func.get_operation());
        let bb = func.get_entry_block(ctx);

        // Create a `const 0` op and add it to bb.
        let zero_const = IntegerAttr::create(i64_ty, ApInt::from(0));
        let const_op = ConstantOp::new_unlinked(ctx, zero_const);
        const_op.get_operation().insert_at_front(bb, ctx);
        set_operation_result_name(ctx, const_op.get_operation(), 0, "c0".to_string());

        // Return the constant.
        let ret_op = ReturnOp::new_unlinked(ctx, const_op.get_result(ctx));
        ret_op.get_operation().insert_at_back(bb, ctx);

        module
    }

    fn setup_context_dialects() -> Context {
        let mut ctx = Context::new();
        dialects::builtin::register(&mut ctx);
        dialects::llvm::register(&mut ctx);
        ctx
    }

    #[test]
    fn use_walk_ops_preorder() {
        let mut ctx = setup_context_dialects();
        let module_op = create_mod_op(&mut ctx);
        let mut opids: Vec<String> = vec![];

        //let res =
        //    walk_op::<(), PreOrder, Forward, _>(module_op.get_operation(), &ctx, |ctx, op| {
        //        opids.push(op.deref(ctx).get_opid().to_string());
        //        WalkResult::Continue(WalkCont::Advance)
        //    });
        let res = walk_op_with_enums::<()>(
            VisitOrder::PreOrder,
            Direction::Forward,
            module_op.get_operation(),
            &ctx,
            |ctx, op| {
                opids.push(op.deref(ctx).get_opid().to_string());
                WalkResult::Continue(WalkCont::Advance)
            },
        );
        if res.is_break() {
            panic!("unexpected break");
        }

        let want: Vec<&str> = vec![
            "builtin.module",
            "builtin.func",
            "builtin.constant",
            "llvm.return",
        ];
        assert_eq!(opids, want);
    }

    #[test]
    fn use_walk_ops_reverse_postorder() {
        let mut ctx = setup_context_dialects();
        let module_op = create_mod_op(&mut ctx);
        let mut opids: Vec<String> = vec![];

        eprintln!("walk:");
        let res =
            walk_op::<(), PostOrder, Reverse, _>(module_op.get_operation(), &ctx, |ctx, op| {
                opids.push(op.deref(ctx).get_opid().to_string());
                WalkResult::Continue(WalkCont::Advance)
            });
        if res.is_break() {
            panic!("unexpected break");
        }

        let want: Vec<&str> = vec![
            "llvm.return",
            "builtin.constant",
            "builtin.func",
            "builtin.module",
        ];
        assert_eq!(opids, want);
    }
}
