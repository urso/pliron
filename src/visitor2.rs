use std::ops::ControlFlow;

use crate::{
    basic_block::BasicBlock,
    context::{Context, Ptr},
    linked_list::ContainsLinkedList,
    operation::Operation,
    region::Region,
};

type WalkResult<B> = ControlFlow<B, WalkCont>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WalkCont {
    Advance, // Advance the walk
    Skip,    // Advance to the next sibling
}

#[inline]
pub fn skip<B>() -> WalkResult<B> {
    return WalkResult::Continue(WalkCont::Skip);
}

#[inline]
pub fn cont<B>() -> WalkResult<B> {
    return WalkResult::Continue(WalkCont::Advance);
}

#[inline]
pub fn break_<B>(v: B) -> WalkResult<B> {
    return WalkResult::Break(v);
}

trait WalkerMut<B> {
    fn walk_op_mut(&mut self, ctx: &mut Context, op: Ptr<Operation>) -> WalkResult<B> {
        self.walk_regions_mut(ctx, op)
    }

    fn walk_regions_mut(&mut self, ctx: &mut Context, op: Ptr<Operation>) -> WalkResult<B> {
        let iter = {
            let op = op.deref(ctx);
            op.regions().iter().cloned().collect::<Vec<_>>()
        };
        for ptr in iter {
            self.walk_region_mut(ctx, ptr)?;
        }
        cont()
    }

    fn walk_region_mut(&mut self, ctx: &mut Context, region: Ptr<Region>) -> WalkResult<B> {
        self.walk_blocks_mut(ctx, region)
    }

    fn walk_blocks_mut(&mut self, ctx: &mut Context, region: Ptr<Region>) -> WalkResult<B> {
        let iter = {
            let region: &Region = &region.deref(ctx);
            region.iter(ctx).collect::<Vec<_>>()
        };
        for ptr in iter {
            self.walk_block_mut(ctx, ptr)?;
        }
        cont()
    }

    fn walk_block_mut(&mut self, ctx: &mut Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        self.walk_ops_mut(ctx, block)
    }

    fn walk_ops_mut(&mut self, ctx: &mut Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        let iter = {
            let block: &BasicBlock = &block.deref(ctx);
            block.iter(ctx).collect::<Vec<_>>()
        };
        for ptr in iter {
            self.walk_op_mut(ctx, ptr)?;
        }
        cont()
    }
}

trait Walker<B> {
    fn walk_op(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        self.walk_regions(ctx, op)
    }

    fn walk_regions(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        for ptr in op.deref(ctx).regions().iter().cloned() {
            self.walk_region(ctx, ptr)?;
        }
        cont()
    }

    fn walk_region(&mut self, ctx: &Context, region: Ptr<Region>) -> WalkResult<B> {
        self.walk_blocks(ctx, region)
    }

    fn walk_blocks(&mut self, ctx: &Context, region: Ptr<Region>) -> WalkResult<B> {
        for ptr in region.deref(ctx).iter(ctx) {
            self.walk_block(ctx, ptr)?;
        }
        cont()
    }

    fn walk_block(&mut self, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        self.walk_ops(ctx, block)
    }

    fn walk_ops(&mut self, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        for ptr in block.deref(ctx).iter(ctx) {
            self.walk_op(ctx, ptr)?;
        }
        cont()
    }
}

trait Visitor<B> {
    fn visit_op(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B>;

    fn visit_region(&mut self, ctx: &Context, region: Ptr<Region>) -> WalkResult<B>;

    fn visit_block(&mut self, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B>;
}

trait VisitorMut<B> {
    fn visit_op(&mut self, ctx: &mut Context, op: Ptr<Operation>) -> WalkResult<B>;

    fn visit_region(&mut self, ctx: &mut Context, region: Ptr<Region>) -> WalkResult<B>;

    fn visit_block(&mut self, ctx: &mut Context, block: Ptr<BasicBlock>) -> WalkResult<B>;
}

struct PreOrder<B, V> {
    visitor: V,
    _marker: std::marker::PhantomData<B>,
}

impl<B, V: Visitor<B>> Walker<B> for PreOrder<B, V> {
    fn walk_op(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        let c = self.visitor.visit_op(ctx, op)?;
        if c == WalkCont::Advance {
            self.walk_regions(ctx, op)?;
        }
        cont()
    }

    fn walk_region(&mut self, ctx: &Context, region: Ptr<Region>) -> WalkResult<B> {
        let c = self.visitor.visit_region(ctx, region)?;
        if c == WalkCont::Advance {
            self.walk_blocks(ctx, region)?;
        }
        cont()
    }

    fn walk_block(&mut self, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        let c = self.visitor.visit_block(ctx, block)?;
        if c == WalkCont::Advance {
            self.walk_ops(ctx, block)?;
        }
        cont()
    }
}

impl<B, V: VisitorMut<B>> WalkerMut<B> for PreOrder<B, V> {
    fn walk_op_mut(&mut self, ctx: &mut Context, op: Ptr<Operation>) -> WalkResult<B> {
        let c = self.visitor.visit_op(ctx, op)?;
        if c == WalkCont::Advance {
            self.walk_regions_mut(ctx, op)?;
        }
        cont()
    }

    fn walk_region_mut(&mut self, ctx: &mut Context, region: Ptr<Region>) -> WalkResult<B> {
        let c = self.visitor.visit_region(ctx, region)?;
        if c == WalkCont::Advance {
            self.walk_blocks_mut(ctx, region)?;
        }
        cont()
    }

    fn walk_block_mut(&mut self, ctx: &mut Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        let c = self.visitor.visit_block(ctx, block)?;
        if c == WalkCont::Advance {
            self.walk_ops_mut(ctx, block)?;
        }
        cont()
    }
}

struct PostOrder<B, V> {
    visitor: V,
    _marker: std::marker::PhantomData<B>,
}

impl<B, V: Visitor<B>> Walker<B> for PostOrder<B, V> {
    fn walk_op(&mut self, ctx: &Context, op: Ptr<Operation>) -> WalkResult<B> {
        self.walk_regions(ctx, op)?;
        self.visitor.visit_op(ctx, op)
    }

    fn walk_region(&mut self, ctx: &Context, region: Ptr<Region>) -> WalkResult<B> {
        self.walk_blocks(ctx, region)?;
        self.visitor.visit_region(ctx, region)
    }

    fn walk_block(&mut self, ctx: &Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        self.walk_ops(ctx, block)?;
        self.visitor.visit_block(ctx, block)
    }
}

impl<B, V: VisitorMut<B>> WalkerMut<B> for PostOrder<B, V> {
    fn walk_op_mut(&mut self, ctx: &mut Context, op: Ptr<Operation>) -> WalkResult<B> {
        self.walk_regions_mut(ctx, op)?;
        self.visitor.visit_op(ctx, op)
    }

    fn walk_region_mut(&mut self, ctx: &mut Context, region: Ptr<Region>) -> WalkResult<B> {
        self.walk_blocks_mut(ctx, region)?;
        self.visitor.visit_region(ctx, region)
    }

    fn walk_block_mut(&mut self, ctx: &mut Context, block: Ptr<BasicBlock>) -> WalkResult<B> {
        self.walk_ops_mut(ctx, block)?;
        self.visitor.visit_block(ctx, block)
    }
}
