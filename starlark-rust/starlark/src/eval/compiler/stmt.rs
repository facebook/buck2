/*
 * Copyright 2018 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Evaluation environment, provide converters from Ast* element to value.
//!
//! # <a name="build_file"></a>Starlark and BUILD dialect
//!
//! All evaluation function can evaluate the full Starlark language (i.e.
//! Bazel's .bzl files) or the BUILD file dialect (i.e. used to interpret
//! Bazel's BUILD file). The BUILD dialect does not allow `def` statements.

use std::mem;

use gazebo::prelude::*;
use thiserror::Error;

use crate::{
    codemap::{FileSpanRef, Span, Spanned},
    environment::{slots::ModuleSlotId, FrozenModuleRef},
    eval::{
        compiler::{
            expr::ExprCompiled,
            expr_bool::ExprCompiledBool,
            known::list_to_tuple,
            scope::{Captured, CstAssign, CstExpr, CstStmt, Slot},
            small_vec_1::SmallVec1,
            span::IrSpanned,
            Compiler,
        },
        runtime::{
            call_stack::FrozenFileSpan,
            evaluator::{Evaluator, GC_THRESHOLD},
            slots::LocalSlotId,
        },
    },
    syntax::ast::{AssignOp, AssignP, StmtP},
    values::{dict::Dict, list::List, FrozenHeap, FrozenValue, Heap, Value, ValueError},
};

#[derive(Clone, Debug)]
pub(crate) enum AssignModifyLhs {
    Dot(IrSpanned<ExprCompiled>, String),
    Array(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>),
    Local(IrSpanned<(LocalSlotId, Captured)>),
    Module(IrSpanned<ModuleSlotId>),
}

#[derive(Clone, Debug)]
pub(crate) enum StmtCompiled {
    PossibleGc,
    Return(IrSpanned<ExprCompiled>),
    Expr(IrSpanned<ExprCompiled>),
    Assign(IrSpanned<AssignCompiledValue>, IrSpanned<ExprCompiled>),
    AssignModify(AssignModifyLhs, AssignOp, IrSpanned<ExprCompiled>),
    If(Box<(IrSpanned<ExprCompiled>, StmtsCompiled, StmtsCompiled)>),
    For(
        Box<(
            IrSpanned<AssignCompiledValue>,
            IrSpanned<ExprCompiled>,
            StmtsCompiled,
        )>,
    ),
    Break,
    Continue,
}

#[derive(Debug, Default)]
pub(crate) struct StmtCompileContext {
    pub(crate) has_before_stmt: bool,
    pub(crate) bc_profile: bool,
}

pub(crate) struct OptimizeOnFreezeContext<'a> {
    pub(crate) module: &'a FrozenModuleRef,
    /// Nothing useful should be left in the heap after the freeze,
    /// but having a heap is useful to allocate objects temporarily
    /// (when invoking operations which require heap).
    pub(crate) heap: &'a Heap,
    pub(crate) frozen_heap: &'a FrozenHeap,
}

impl AssignModifyLhs {
    fn optimize_on_freeze(&self, ctx: &OptimizeOnFreezeContext) -> AssignModifyLhs {
        match self {
            AssignModifyLhs::Dot(expr, name) => {
                AssignModifyLhs::Dot(expr.optimize_on_freeze(ctx), name.clone())
            }
            AssignModifyLhs::Array(expr, index) => {
                AssignModifyLhs::Array(expr.optimize_on_freeze(ctx), index.optimize_on_freeze(ctx))
            }
            AssignModifyLhs::Local(slot) => AssignModifyLhs::Local(*slot),
            AssignModifyLhs::Module(slot) => AssignModifyLhs::Module(*slot),
        }
    }
}

impl IrSpanned<StmtCompiled> {
    fn optimize_on_freeze(&self, ctx: &OptimizeOnFreezeContext) -> StmtsCompiled {
        let span = self.span;
        match self.node {
            StmtCompiled::Return(ref e) => StmtsCompiled::one(IrSpanned {
                span,
                node: StmtCompiled::Return(e.optimize_on_freeze(ctx)),
            }),
            StmtCompiled::Expr(ref expr) => {
                let expr = expr.optimize_on_freeze(ctx);
                StmtsCompiled::expr(expr)
            }
            StmtCompiled::Assign(ref lhs, ref rhs) => {
                let lhs = lhs.optimize_on_freeze(ctx);
                let rhs = rhs.optimize_on_freeze(ctx);
                StmtsCompiled::one(IrSpanned {
                    span,
                    node: StmtCompiled::Assign(lhs, rhs),
                })
            }
            StmtCompiled::If(box (ref cond, ref t, ref f)) => {
                let cond = cond.optimize_on_freeze(ctx);
                let t = t.optimize_on_freeze(ctx);
                let f = f.optimize_on_freeze(ctx);
                StmtsCompiled::if_stmt(span, cond, t, f)
            }
            StmtCompiled::For(box (ref var, ref over, ref body)) => {
                let var = var.optimize_on_freeze(ctx);
                let over = over.optimize_on_freeze(ctx);
                let body = body.optimize_on_freeze(ctx);
                StmtsCompiled::for_stmt(span, var, over, body)
            }
            ref s @ (StmtCompiled::PossibleGc | StmtCompiled::Break | StmtCompiled::Continue) => {
                StmtsCompiled::one(IrSpanned {
                    span,
                    node: s.clone(),
                })
            }
            StmtCompiled::AssignModify(ref lhs, op, ref rhs) => StmtsCompiled::one(IrSpanned {
                span,
                node: StmtCompiled::AssignModify(
                    lhs.optimize_on_freeze(ctx),
                    op,
                    rhs.optimize_on_freeze(ctx),
                ),
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct StmtsCompiled(SmallVec1<IrSpanned<StmtCompiled>>);

impl StmtsCompiled {
    pub(crate) fn empty() -> StmtsCompiled {
        StmtsCompiled(SmallVec1::Empty)
    }

    pub(crate) fn one(stmt: IrSpanned<StmtCompiled>) -> StmtsCompiled {
        StmtsCompiled(SmallVec1::One(stmt))
    }

    pub(crate) fn is_empty(&self) -> bool {
        match &self.0 {
            SmallVec1::Empty => true,
            SmallVec1::One(_) => false,
            SmallVec1::Many(stmts) => {
                debug_assert!(stmts.len() > 1);
                false
            }
        }
    }

    pub(crate) fn stmts(&self) -> &[IrSpanned<StmtCompiled>] {
        self.0.as_slice()
    }

    /// Last statement in this block is `break`, `continue` or `return`.
    fn is_terminal(&self) -> bool {
        if let Some(stmt) = self.last() {
            match &stmt.node {
                StmtCompiled::Break | StmtCompiled::Continue | StmtCompiled::Return(..) => true,
                _ => false,
            }
        } else {
            false
        }
    }

    pub(crate) fn extend(&mut self, right: StmtsCompiled) {
        // Do not add any code after `break`, `continue` or `return`.
        if self.is_terminal() {
            return;
        }
        self.0.extend(right.0);
    }

    pub(crate) fn optimize_on_freeze(&self, ctx: &OptimizeOnFreezeContext) -> StmtsCompiled {
        let mut stmts = StmtsCompiled::empty();
        match &self.0 {
            SmallVec1::Empty => {}
            SmallVec1::One(s) => stmts.extend(s.optimize_on_freeze(ctx)),
            SmallVec1::Many(ss) => {
                for s in ss {
                    if stmts.is_terminal() {
                        break;
                    }
                    stmts.extend(s.optimize_on_freeze(ctx));
                }
            }
        }
        stmts
    }

    pub(crate) fn first(&self) -> Option<&IrSpanned<StmtCompiled>> {
        match &self.0 {
            SmallVec1::Empty => None,
            SmallVec1::One(s) => Some(s),
            SmallVec1::Many(ss) => ss.first(),
        }
    }

    pub(crate) fn last(&self) -> Option<&IrSpanned<StmtCompiled>> {
        match &self.0 {
            SmallVec1::Empty => None,
            SmallVec1::One(s) => Some(s),
            SmallVec1::Many(ss) => ss.last(),
        }
    }

    fn expr(expr: IrSpanned<ExprCompiled>) -> StmtsCompiled {
        let span = expr.span;
        match expr.node {
            expr if expr.is_pure_infallible() => StmtsCompiled::empty(),
            ExprCompiled::List(xs) | ExprCompiled::Tuple(xs) => {
                let mut stmts = StmtsCompiled::empty();
                for x in xs {
                    stmts.extend(Self::expr(x));
                }
                stmts
            }
            // Unwrap infallible expressions.
            ExprCompiled::Type(x) | ExprCompiled::TypeIs(x, _) | ExprCompiled::Not(x) => {
                Self::expr(*x)
            }
            // "And" and "or" for effect are equivalent to `if`.
            ExprCompiled::And(box (x, y)) => {
                Self::if_stmt(expr.span, x, Self::expr(y), StmtsCompiled::empty())
            }
            ExprCompiled::Or(box (x, y)) => {
                Self::if_stmt(expr.span, x, StmtsCompiled::empty(), Self::expr(y))
            }
            expr => StmtsCompiled::one(IrSpanned {
                span,
                node: StmtCompiled::Expr(IrSpanned { span, node: expr }),
            }),
        }
    }

    fn if_stmt(
        span: FrozenFileSpan,
        cond: IrSpanned<ExprCompiled>,
        t: StmtsCompiled,
        f: StmtsCompiled,
    ) -> StmtsCompiled {
        let cond = ExprCompiledBool::new(cond);
        match cond.node {
            ExprCompiledBool::Const(true) => t,
            ExprCompiledBool::Const(false) => f,
            ExprCompiledBool::Expr(cond) => match cond {
                ExprCompiled::Not(box cond) => Self::if_stmt(span, cond, f, t),
                ExprCompiled::Seq(box (x, cond)) => {
                    let mut stmt = StmtsCompiled::empty();
                    stmt.extend(Self::expr(x));
                    stmt.extend(Self::if_stmt(span, cond, t, f));
                    stmt
                }
                cond => {
                    let cond = IrSpanned { span, node: cond };
                    if t.is_empty() && f.is_empty() {
                        Self::expr(cond)
                    } else {
                        StmtsCompiled::one(IrSpanned {
                            span,
                            node: StmtCompiled::If(box (cond, t, f)),
                        })
                    }
                }
            },
        }
    }

    fn for_stmt(
        span: FrozenFileSpan,
        var: IrSpanned<AssignCompiledValue>,
        over: IrSpanned<ExprCompiled>,
        body: StmtsCompiled,
    ) -> StmtsCompiled {
        if over.is_iterable_empty() {
            return StmtsCompiled::empty();
        }
        StmtsCompiled::one(IrSpanned {
            span,
            node: StmtCompiled::For(box (var, over, body)),
        })
    }
}

#[derive(Debug, Error)]
pub(crate) enum AssignError {
    // Incorrect number of value to unpack (expected, got)
    #[error("Unpacked {1} values but expected {0}")]
    IncorrectNumberOfValueToUnpack(i32, i32),
}

#[derive(Clone, Debug)]
pub(crate) enum AssignCompiledValue {
    Dot(IrSpanned<ExprCompiled>, String),
    ArrayIndirection(IrSpanned<ExprCompiled>, IrSpanned<ExprCompiled>),
    Tuple(Vec<IrSpanned<AssignCompiledValue>>),
    Local(LocalSlotId, Captured),
    Module(ModuleSlotId, String),
}

impl IrSpanned<AssignCompiledValue> {
    pub(crate) fn optimize_on_freeze(
        &self,
        ctx: &OptimizeOnFreezeContext,
    ) -> IrSpanned<AssignCompiledValue> {
        let span = self.span;
        let assign = match self.node {
            AssignCompiledValue::Dot(ref object, ref field) => {
                let object = object.optimize_on_freeze(ctx);
                let field = field.clone();
                AssignCompiledValue::Dot(object, field)
            }
            AssignCompiledValue::ArrayIndirection(ref array, ref index) => {
                let array = array.optimize_on_freeze(ctx);
                let index = index.optimize_on_freeze(ctx);
                AssignCompiledValue::ArrayIndirection(array, index)
            }
            AssignCompiledValue::Tuple(ref xs) => {
                let xs = xs.map(|x| x.optimize_on_freeze(ctx));
                AssignCompiledValue::Tuple(xs)
            }
            ref e @ (AssignCompiledValue::Local(..) | AssignCompiledValue::Module(..)) => e.clone(),
        };
        IrSpanned { node: assign, span }
    }
}

impl Compiler<'_, '_, '_> {
    pub fn assign(&mut self, expr: CstAssign) -> IrSpanned<AssignCompiledValue> {
        let span = FrozenFileSpan::new(self.codemap, expr.span);
        let assign = match expr.node {
            AssignP::Dot(e, s) => {
                let e = self.expr(*e);
                let s = s.node;
                AssignCompiledValue::Dot(e, s)
            }
            AssignP::ArrayIndirection(box (e, idx)) => {
                let e = self.expr(e);
                let idx = self.expr(idx);
                AssignCompiledValue::ArrayIndirection(e, idx)
            }
            AssignP::Tuple(v) => {
                let v = v.into_map(|x| self.assign(x));
                AssignCompiledValue::Tuple(v)
            }
            AssignP::Identifier(ident) => {
                let name = ident.node.0;
                let binding_id = ident
                    .node
                    .1
                    .unwrap_or_else(|| panic!("unresolved binding: `{}`", name));
                let binding = self.scope_data.get_binding(binding_id);
                let slot = binding
                    .slot
                    .unwrap_or_else(|| panic!("unresolved binding: `{}`", name));
                match (slot, binding.captured) {
                    (Slot::Local(slot), captured) => AssignCompiledValue::Local(slot, captured),
                    (Slot::Module(slot), _) => AssignCompiledValue::Module(slot, name),
                }
            }
        };
        IrSpanned { node: assign, span }
    }

    fn assign_modify(
        &mut self,
        span_stmt: Span,
        lhs: CstAssign,
        rhs: IrSpanned<ExprCompiled>,
        op: AssignOp,
    ) -> StmtsCompiled {
        let span_stmt = FrozenFileSpan::new(self.codemap, span_stmt);
        let span_lhs = FrozenFileSpan::new(self.codemap, lhs.span);
        match lhs.node {
            AssignP::Dot(e, s) => {
                let e = self.expr(*e);
                StmtsCompiled::one(IrSpanned {
                    span: span_stmt,
                    node: StmtCompiled::AssignModify(AssignModifyLhs::Dot(e, s.node), op, rhs),
                })
            }
            AssignP::ArrayIndirection(box (e, idx)) => {
                let e = self.expr(e);
                let idx = self.expr(idx);
                StmtsCompiled::one(IrSpanned {
                    span: span_stmt,
                    node: StmtCompiled::AssignModify(AssignModifyLhs::Array(e, idx), op, rhs),
                })
            }
            AssignP::Identifier(ident) => {
                let (slot, captured) = self.scope_data.get_assign_ident_slot(&ident);
                match slot {
                    Slot::Local(slot) => {
                        let lhs = IrSpanned {
                            node: (slot, captured),
                            span: span_lhs,
                        };
                        StmtsCompiled::one(IrSpanned {
                            span: span_stmt,
                            node: StmtCompiled::AssignModify(AssignModifyLhs::Local(lhs), op, rhs),
                        })
                    }
                    Slot::Module(slot) => {
                        let lhs = IrSpanned {
                            node: slot,
                            span: span_lhs,
                        };
                        StmtsCompiled::one(IrSpanned {
                            span: span_stmt,
                            node: StmtCompiled::AssignModify(AssignModifyLhs::Module(lhs), op, rhs),
                        })
                    }
                }
            }
            AssignP::Tuple(_) => {
                unreachable!("Assign modify validates that the LHS is never a tuple")
            }
        }
    }
}

// This function should be called before every meaningful statement.
// The purposes are GC, profiling and debugging.
//
// This function is called only if `before_stmt` is set before compilation start.
pub(crate) fn before_stmt(span: FrozenFileSpan, eval: &mut Evaluator) {
    assert!(
        eval.before_stmt.enabled(),
        "this code should not be called if `before_stmt` is set"
    );
    let fs = mem::take(&mut eval.before_stmt.before_stmt);
    for f in &fs {
        f(
            FileSpanRef {
                span: span.span(),
                file: &span.file(),
            },
            eval,
        )
    }
    let added = mem::replace(&mut eval.before_stmt.before_stmt, fs);
    assert!(
        added.is_empty(),
        "`before_stmt` cannot be modified during evaluation"
    );
}

// There are two requirements to perform a GC:
//
// 1. We can't be profiling, since profiling relies on the redundant heap
//    entries. When profiling we set disable_gc.
// 2. We must be able to access all roots.
//
// We track as many roots as possible, and eventually aim to track them all, but
// for the moment we're only sure we have all roots when we are in the module
// evaluation eval. There are three roots we don't yet know about:
//
// 1. When evaluating an expression which has multiple subexpressions, e.g. List
//    we can't GC during that, as we can't see the root of the first list.
// 2. When evaluating inside a native function, especially if that native
//    function calls back to a non-native function, e.g. sort with a comparison
//    function.
// 3. When iterating we freeze the iteration variable, which means it
//    can't be moved by a GC. A special type of root.
//
// The first issue can be solved by moving to a bytecode interpreter and
// evaluation stack. The second issue can be solved by disabling GC while in
// such functions (it's probably rare). The third issue could be solved by
// making the freeze for iteration a separate flag to the RefCell, at the cost
// of an extra word in ValueMem. Or we could disable GC while iterating.
//
// For the moment we only GC when executing a statement at the root of the
// module, which we know is safe with respect to all three conditions.
//
// We also require that `extra_v` is None, since otherwise the user might have
// additional values stashed somewhere.
pub(crate) fn possible_gc(eval: &mut Evaluator) {
    if !eval.disable_gc
        && eval.heap().allocated_bytes() >= eval.next_gc_level
        && eval.extra_v.is_none()
    {
        // When we are at a module scope (as checked above) the eval contains
        // references to all values, so walking covers everything and the unsafe
        // is satisfied.
        unsafe { eval.garbage_collect() }
        eval.next_gc_level = eval.heap().allocated_bytes() + GC_THRESHOLD;
    }
}

/// Implement lhs |= rhs, which is special in Starlark, because dicts are mutated,
/// while all other types are not.
pub(crate) fn bit_or_assign<'v>(
    lhs: Value<'v>,
    rhs: Value<'v>,
    heap: &'v Heap,
) -> anyhow::Result<Value<'v>> {
    // The Starlark spec says dict |= mutates, while nothing else does.
    // When mutating, be careful if they alias, so we don't have `lhs`
    // mutably borrowed when we iterate over `rhs`, as they might alias.

    let lhs_aref = lhs.get_ref();
    let lhs_ty = lhs_aref.static_type_of_value();

    if Dict::is_dict_type(lhs_ty) {
        let mut dict = Dict::from_value_mut(lhs)?;
        if lhs.ptr_eq(rhs) {
            // Nothing to do as union is idempotent
        } else {
            let rhs = Dict::from_value(rhs).map_or_else(
                || ValueError::unsupported_owned(lhs_aref.get_type(), "|=", Some(rhs.get_type())),
                Ok,
            )?;
            for (k, v) in rhs.iter_hashed() {
                dict.insert_hashed(k, v);
            }
        }
        Ok(lhs)
    } else {
        lhs_aref.bit_or(rhs, heap)
    }
}

/// Implement lhs += rhs, which is special in Starlark, because lists are mutated,
/// while all other types are not.
pub(crate) fn add_assign<'v>(
    lhs: Value<'v>,
    rhs: Value<'v>,
    heap: &'v Heap,
) -> anyhow::Result<Value<'v>> {
    // Checking whether a value is an integer or a string is cheap (no virtual call),
    // and `Value::add` has optimizations for these types, so check them first
    // and delegate to `Value::add`.
    if lhs.unpack_int().is_some() || lhs.is_str() {
        return lhs.add(rhs, heap);
    }

    // The Starlark spec says list += mutates, while nothing else does.
    // When mutating, be careful if they alias, so we don't have `lhs`
    // mutably borrowed when we iterate over `rhs`, as they might alias.

    // In practice, select is the only thing that implements radd.
    // If the users does x += select(...) we don't want an error,
    // we really want to x = x + select, so check radd first.
    let lhs_aref = lhs.get_ref();
    let lhs_ty = lhs_aref.static_type_of_value();

    if List::is_list_type(lhs_ty) {
        if let Some(v) = rhs.get_ref().radd(lhs, heap) {
            v
        } else {
            let list = List::from_value_mut(lhs)?;
            if lhs.ptr_eq(rhs) {
                list.double(heap);
            } else {
                // TODO: if RHS is list, consider calling `List::extend_from_slice`.
                rhs.with_iterator(heap, |it| list.extend(it, heap))?;
            }
            Ok(lhs)
        }
    } else {
        lhs.add(rhs, heap)
    }
}

impl Compiler<'_, '_, '_> {
    pub(crate) fn compile_context(&self) -> StmtCompileContext {
        StmtCompileContext {
            has_before_stmt: self.has_before_stmt,
            bc_profile: self.bc_profile,
        }
    }

    pub(crate) fn stmt(&mut self, stmt: CstStmt, allow_gc: bool) -> StmtsCompiled {
        let span = FrozenFileSpan::new(self.codemap, stmt.span);
        let is_statements = matches!(&stmt.node, StmtP::Statements(_));
        let res = self.stmt_direct(stmt, allow_gc);
        // No point inserting a GC point around statements, since they will contain inner statements we can do
        if allow_gc && !is_statements {
            // We could do this more efficiently by fusing the possible_gc
            // into the inner closure, but no real need - we insert allow_gc fairly rarely
            let mut with_gc = StmtsCompiled::one(IrSpanned {
                span,
                node: StmtCompiled::PossibleGc,
            });
            with_gc.extend(res);
            with_gc
        } else {
            res
        }
    }

    pub(crate) fn module_top_level_stmt(&mut self, stmt: CstStmt) -> StmtsCompiled {
        match stmt.node {
            StmtP::Statements(..) => {
                unreachable!("top level statement lists are handled by outer loop")
            }
            StmtP::Expression(expr) => {
                let stmt = Spanned {
                    span: expr.span,
                    // When top level statement is an expression, compile it as return.
                    // This is used to obtain the result of evaluation
                    // of the last statement-expression in module.
                    node: StmtP::Return(Some(expr)),
                };
                self.stmt(stmt, true)
            }
            _ => self.stmt(stmt, true),
        }
    }

    fn stmt_if(
        &mut self,
        span: FrozenFileSpan,
        cond: CstExpr,
        then_block: CstStmt,
        allow_gc: bool,
    ) -> StmtsCompiled {
        let cond = self.expr(cond);
        let then_block = self.stmt(then_block, allow_gc);
        StmtsCompiled::if_stmt(span, cond, then_block, StmtsCompiled::empty())
    }

    fn stmt_if_else(
        &mut self,
        span: FrozenFileSpan,
        cond: CstExpr,
        then_block: CstStmt,
        else_block: CstStmt,
        allow_gc: bool,
    ) -> StmtsCompiled {
        let cond = self.expr(cond);
        let then_block = self.stmt(then_block, allow_gc);
        let else_block = self.stmt(else_block, allow_gc);
        StmtsCompiled::if_stmt(span, cond, then_block, else_block)
    }

    fn stmt_expr(&mut self, expr: CstExpr) -> StmtsCompiled {
        let expr = self.expr(expr);
        StmtsCompiled::expr(expr)
    }

    fn stmt_direct(&mut self, stmt: CstStmt, allow_gc: bool) -> StmtsCompiled {
        let span = FrozenFileSpan::new(self.codemap, stmt.span);
        match stmt.node {
            StmtP::Def(name, params, return_type, suite, scope_id) => {
                let rhs = IrSpanned {
                    node: self.function(&name.0, scope_id, params, return_type, *suite),
                    span,
                };
                let lhs = self.assign(Spanned {
                    span: name.span,
                    node: AssignP::Identifier(name),
                });
                StmtsCompiled::one(IrSpanned {
                    span,
                    node: StmtCompiled::Assign(lhs, rhs),
                })
            }
            StmtP::For(var, box (over, body)) => {
                let over = list_to_tuple(over);
                let var = self.assign(var);
                let over = self.expr(over);
                let st = self.stmt(body, false);
                StmtsCompiled::for_stmt(span, var, over, st)
            }
            StmtP::Return(None) => StmtsCompiled::one(IrSpanned {
                node: StmtCompiled::Return(IrSpanned {
                    span,
                    node: ExprCompiled::Value(FrozenValue::new_none()),
                }),
                span,
            }),
            StmtP::Return(Some(e)) => StmtsCompiled::one(IrSpanned {
                node: StmtCompiled::Return(self.expr(e)),
                span,
            }),
            StmtP::If(cond, box then_block) => self.stmt_if(span, cond, then_block, allow_gc),
            StmtP::IfElse(cond, box (then_block, else_block)) => {
                self.stmt_if_else(span, cond, then_block, else_block, allow_gc)
            }
            StmtP::Statements(stmts) => {
                let mut r = StmtsCompiled::empty();
                for stmt in stmts {
                    if r.is_terminal() {
                        break;
                    }
                    r.extend(self.stmt(stmt, allow_gc));
                }
                r
            }
            StmtP::Expression(e) => self.stmt_expr(e),
            StmtP::Assign(lhs, rhs) => {
                let rhs = self.expr(*rhs);
                let lhs = self.assign(lhs);
                StmtsCompiled::one(IrSpanned {
                    span,
                    node: StmtCompiled::Assign(lhs, rhs),
                })
            }
            StmtP::AssignModify(lhs, op, rhs) => {
                let rhs = self.expr(*rhs);
                self.assign_modify(span.span(), lhs, rhs, op)
            }
            StmtP::Load(..) => unreachable!(),
            StmtP::Pass => StmtsCompiled::empty(),
            StmtP::Break => StmtsCompiled::one(IrSpanned {
                span,
                node: StmtCompiled::Break,
            }),
            StmtP::Continue => StmtsCompiled::one(IrSpanned {
                span,
                node: StmtCompiled::Continue,
            }),
        }
    }
}
