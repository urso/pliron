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

pub trait Visitor<B> {
    fn visit_op(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        walk_op(self, ctx, op)
    }

    fn visit_region(&mut self, ctx: &Context, region: Ptr<Region>) -> WalkResult<B> {
        walk_region(self, ctx, region)
    }

    fn visit_block(&mut self, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        walk_block(self, ctx, block)
    }
}

fn walk_op<V, B>(a: &mut V, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B>
where
    V: Visitor<B> + ?Sized,
{
    for ptr in op.deref(ctx).regions().iter().cloned() {
        a.visit_region(ctx, ptr)?;
    }
    WalkResult::Continue(WalkCont::Advance)
}

fn walk_region<V, B>(a: &mut V, ctx: &Context, region: Ptr<Region>) -> WalkResult<B>
where
    V: Visitor<B> + ?Sized,
{
    for ptr in region.deref(ctx).iter(ctx) {
        a.visit_block(ctx, ptr)?;
    }
    WalkResult::Continue(WalkCont::Advance)
}

fn walk_block<V, B>(a: &mut V, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B>
where
    V: Visitor<B> + ?Sized,
{
    for ptr in block.deref(ctx).iter(ctx) {
        a.visit_op(ctx, ptr)?;
    }
    WalkResult::Continue(WalkCont::Advance)
}

fn walk_op_rev<V, B>(a: &mut V, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B>
where
    V: Visitor<B> + ?Sized,
{
    for ptr in op.deref(ctx).regions().iter().rev().cloned() {
        a.visit_region(ctx, ptr)?;
    }
    WalkResult::Continue(WalkCont::Advance)
}

fn walk_region_rev<V, B>(a: &mut V, ctx: &Context, region: Ptr<Region>) -> WalkResult<B>
where
    V: Visitor<B> + ?Sized,
{
    for ptr in region.deref(ctx).iter(ctx).rev() {
        a.visit_block(ctx, ptr)?;
    }
    WalkResult::Continue(WalkCont::Advance)
}

fn walk_block_rev<V, B>(a: &mut V, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B>
where
    V: Visitor<B> + ?Sized,
{
    for ptr in block.deref(ctx).iter(ctx).rev() {
        a.visit_op(ctx, ptr)?;
    }
    WalkResult::Continue(WalkCont::Advance)
}

// ------------------------------------------------------------------------------------------------
// Callback based walker functions
//

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Order {
    PreOrder,
    PostOrder,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    Forward,
    Reverse,
}

struct Callback<IR, F> {
    f: F,
    order: Order,
    direction: Direction,
    marker: PhantomData<IR>,
}

impl<IR, F> Callback<IR, F> {
    pub fn new(f: F, order: Order, direction: Direction) -> Self {
        Self {
            f,
            order,
            direction,
            marker: PhantomData,
        }
    }
}

pub fn walk_ops<B>(
    order: Order,
    direction: Direction,
    root: Ptr<Operation>,
    ctx: &Context,
    callback: impl FnMut(&Context, Ptr<Operation>) -> WalkResult<B>,
) -> WalkResult<B> {
    let mut cb = Callback::new(callback, order, direction);
    cb.visit_op(ctx, root)
}

/* Todo: would require more variants of the Callback implementation

pub fn walk_regions<B>(
    order: Order,
    direction: Direction,
    root: Ptr<Operation>,
    ctx: &Context,
    callback: impl FnMut(&Context, Ptr<Region>) -> WalkResult<B>,
) -> WalkResult<B> {
    let mut cb = Callback::new(callback, order, direction);
    cb.visit_op(ctx, root)
}

pub fn walk_blocks<B>(
    order: Order,
    direction: Direction,
    root: Ptr<Operation>,
    ctx: &Context,
    callback: impl FnMut(&Context, Ptr<BasicBlock>) -> WalkResult<B>,
) -> WalkResult<B> {
    let mut cb = Callback::new(callback, order, direction);
    cb.visit_op(ctx, root)
}
*/

impl<B, F> Visitor<B> for Callback<Operation, F>
where
    F: FnMut(&Context, Ptr<Operation>) -> WalkResult<B>,
{
    fn visit_op(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        if self.order == Order::PreOrder {
            let cont = (self.f)(ctx, op)?;
            if cont == WalkCont::Skip {
                // go to next sibling Op, but ignore regions and blocks within this Op
                return WalkResult::Continue(WalkCont::Advance);
            }
        }

        match self.direction {
            Direction::Forward => walk_op(self, ctx, op)?,
            Direction::Reverse => walk_op_rev(self, ctx, op)?,
        };

        if self.order == Order::PostOrder {
            (self.f)(ctx, op)?;
        }
        WalkResult::Continue(WalkCont::Advance)
    }

    fn visit_region(&mut self, ctx: &Context, region: Ptr<Region>) -> WalkResult<B> {
        match self.direction {
            Direction::Forward => walk_region(self, ctx, region),
            Direction::Reverse => walk_region_rev(self, ctx, region),
        }
    }

    fn visit_block(&mut self, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        match self.direction {
            Direction::Forward => walk_block(self, ctx, block),
            Direction::Reverse => walk_block_rev(self, ctx, block),
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

        eprintln!("walk:");
        let res = walk_ops::<()>(
            Order::PreOrder,
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
        let res = walk_ops::<()>(
            Order::PostOrder,
            Direction::Reverse,
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
            "llvm.return",
            "builtin.constant",
            "builtin.func",
            "builtin.module",
        ];
        assert_eq!(opids, want);
    }
}
