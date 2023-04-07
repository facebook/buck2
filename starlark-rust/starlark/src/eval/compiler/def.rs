/*
 * Copyright 2019 The Starlark in Rust Authors.
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

//! Implementation of `def`.

use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::fmt::Write;
use std::ptr;
use std::time::Instant;

use allocative::Allocative;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use gazebo::prelude::VecExt;
use once_cell::sync::Lazy;
use starlark_derive::NoSerialize;
use starlark_derive::VisitSpanMut;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::codemap::CodeMap;
use crate::collections::Hashed;
use crate::const_frozen_string;
use crate::docs;
use crate::docs::DocItem;
use crate::docs::DocString;
use crate::docs::DocStringKind;
use crate::environment::FrozenModuleData;
use crate::environment::Globals;
use crate::eval::bc::bytecode::Bc;
use crate::eval::bc::frame::alloca_frame;
use crate::eval::compiler::def_inline::inline_def_body;
use crate::eval::compiler::def_inline::InlineDefBody;
use crate::eval::compiler::expr::ExprCompiled;
use crate::eval::compiler::opt_ctx::OptCtx;
use crate::eval::compiler::scope::Captured;
use crate::eval::compiler::scope::CstAssignIdent;
use crate::eval::compiler::scope::CstExpr;
use crate::eval::compiler::scope::CstParameter;
use crate::eval::compiler::scope::CstStmt;
use crate::eval::compiler::scope::ScopeId;
use crate::eval::compiler::span::IrSpanned;
use crate::eval::compiler::stmt::OptimizeOnFreezeContext;
use crate::eval::compiler::stmt::StmtCompileContext;
use crate::eval::compiler::stmt::StmtsCompiled;
use crate::eval::compiler::Compiler;
use crate::eval::compiler::EvalException;
use crate::eval::runtime::arguments::ArgumentsImpl;
use crate::eval::runtime::arguments::ResolvedArgName;
use crate::eval::runtime::evaluator::Evaluator;
use crate::eval::runtime::frame_span::FrameSpan;
use crate::eval::runtime::frozen_file_span::FrozenFileSpan;
use crate::eval::runtime::params::ParametersSpec;
use crate::eval::runtime::slots::LocalSlotId;
use crate::eval::runtime::slots::LocalSlotIdCapturedOrNot;
use crate::eval::Arguments;
use crate::syntax::ast::ParameterP;
use crate::values::frozen_ref::AtomicFrozenRefOption;
use crate::values::function::FUNCTION_TYPE;
use crate::values::typing::TypeCompiled;
use crate::values::Freeze;
use crate::values::Freezer;
use crate::values::FrozenHeap;
use crate::values::FrozenRef;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Tracer;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(thiserror::Error, Debug)]
enum DefError {
    #[error("Function has no type, while function was compiled with return type (internal error)")]
    CheckReturnTypeNoType,
}

/// Store frozen `StmtCompiled`.
/// This is initialized in `post_freeze`.
struct StmtCompiledCell {
    cell: UnsafeCell<Bc>,
}

unsafe impl<'v> Trace<'v> for StmtCompiledCell {
    fn trace(&mut self, _: &Tracer<'v>) {
        // Bytecode contains only frozen values.
    }
}

unsafe impl Sync for StmtCompiledCell {}
unsafe impl Send for StmtCompiledCell {}

impl StmtCompiledCell {
    fn new() -> StmtCompiledCell {
        StmtCompiledCell {
            cell: UnsafeCell::new(Bc::default()),
        }
    }

    /// This function is unsafe if other thread is executing the stmt.
    unsafe fn set(&self, value: Bc) {
        ptr::drop_in_place(self.cell.get());
        ptr::write(self.cell.get(), value);
    }

    fn get(&self) -> &Bc {
        unsafe { &*self.cell.get() }
    }
}

#[derive(Clone, Debug, VisitSpanMut)]
pub(crate) struct ParameterName {
    pub(crate) name: String,
    captured: Captured,
}

#[derive(Clone, Debug, VisitSpanMut)]
pub(crate) enum ParameterCompiled<T> {
    Normal(ParameterName, Option<T>),
    WithDefaultValue(ParameterName, Option<T>, T),
    Args(ParameterName, Option<T>),
    KwArgs(ParameterName, Option<T>),
}

impl<T> ParameterCompiled<T> {
    pub(crate) fn map_expr<U>(&self, mut f: impl FnMut(&T) -> U) -> ParameterCompiled<U> {
        match self {
            ParameterCompiled::Normal(n, o) => {
                ParameterCompiled::Normal(n.clone(), o.as_ref().map(f))
            }
            ParameterCompiled::WithDefaultValue(n, o, t) => {
                ParameterCompiled::WithDefaultValue(n.clone(), o.as_ref().map(&mut f), f(t))
            }
            ParameterCompiled::Args(n, o) => ParameterCompiled::Args(n.clone(), o.as_ref().map(f)),
            ParameterCompiled::KwArgs(n, o) => {
                ParameterCompiled::KwArgs(n.clone(), o.as_ref().map(f))
            }
        }
    }

    pub(crate) fn accepts_positional(&self) -> bool {
        match self {
            ParameterCompiled::Normal(_, _) => true,
            ParameterCompiled::WithDefaultValue(_, _, _) => true,
            _ => false,
        }
    }

    pub(crate) fn captured(&self) -> Captured {
        self.name_ty().0.captured
    }

    pub(crate) fn name_ty(&self) -> (&ParameterName, Option<&T>) {
        match self {
            Self::Normal(n, t) => (n, t.as_ref()),
            Self::WithDefaultValue(n, t, _) => (n, t.as_ref()),
            Self::Args(n, t) => (n, t.as_ref()),
            Self::KwArgs(n, t) => (n, t.as_ref()),
        }
    }

    pub(crate) fn has_type(&self) -> bool {
        match self.name_ty() {
            (_, Some(_)) => true,
            _ => false,
        }
    }

    pub(crate) fn is_star_or_star_star(&self) -> bool {
        matches!(
            self,
            ParameterCompiled::Args(_, _) | ParameterCompiled::KwArgs(_, _)
        )
    }
}

#[derive(Debug, Clone, VisitSpanMut)]
pub(crate) struct ParametersCompiled<T> {
    pub(crate) params: Vec<IrSpanned<ParameterCompiled<T>>>,
    /// Number of parameters which can be filled positionally.
    /// That is, number of parameters before first `*`, `*args` or `**kwargs`.
    pub(crate) num_positional: u32,
}

impl<T> ParametersCompiled<T> {
    /// How many expressions this parameters references (default values and types).
    pub(crate) fn count_exprs(&self) -> u32 {
        let mut count = 0;
        for p in &self.params {
            p.map_expr(|_e| count += 1);
        }
        count
    }

    /// How many parameter variables?
    pub(crate) fn count_param_variables(&self) -> u32 {
        self.params.len().try_into().unwrap()
    }

    /// Any parameter has type annotation?
    pub(crate) fn has_types(&self) -> bool {
        self.params.iter().any(|p| p.has_type())
    }

    /// Has `*args` or `*kwargs` parameter? `*` is fine.
    pub(crate) fn has_args_or_kwargs(&self) -> bool {
        self.params.iter().any(|p| {
            matches!(
                p.node,
                ParameterCompiled::Args(..) | ParameterCompiled::KwArgs(..)
            )
        })
    }

    pub(crate) fn parameter_captures(&self) -> Vec<LocalSlotId> {
        self.params
            .iter()
            .enumerate()
            .filter_map(|(i, p)| {
                if p.captured() == Captured::Yes {
                    Some(LocalSlotId(i as u32))
                } else {
                    None
                }
            })
            .collect()
    }
}

/// Copy local variable slot to nested function.
#[derive(Debug, Clone, Dupe)]
pub(crate) struct CopySlotFromParent {
    /// Slot in the outer function.
    pub(crate) parent: LocalSlotIdCapturedOrNot,
    /// Slot in the nested function.
    pub(crate) child: LocalSlotIdCapturedOrNot,
}

/// Static info for `def`, `lambda` or module.
#[derive(Derivative, Display)]
#[derivative(Debug)]
#[display(fmt = "DefInfo")]
pub(crate) struct DefInfo {
    pub(crate) name: FrozenStringValue,
    /// Span of function signature.
    pub(crate) signature_span: FrozenFileSpan,
    /// Indices of parameters, which are captured in nested defs.
    parameter_captures: FrozenRef<'static, [LocalSlotId]>,
    /// Codemap of the file where the function is declared.
    pub(crate) codemap: FrozenRef<'static, CodeMap>,
    /// The raw docstring pulled out of the AST.
    pub(crate) docstring: Option<String>,
    /// Slots this scope uses, including for parameters and `parent`.
    /// Indexed by [`LocalSlotId`], values are variable names.
    pub(crate) used: FrozenRef<'static, [FrozenStringValue]>,
    /// Slots to copy from the parent.
    /// Module-level identifiers are not copied over, to avoid excess copying.
    pub(crate) parent: FrozenRef<'static, [CopySlotFromParent]>,
    /// Statement compiled for non-frozen def.
    #[derivative(Debug = "ignore")]
    stmt_compiled: Bc,
    // The compiled expression for the body of this definition, to be run
    // after the parameters are evaluated.
    #[derivative(Debug = "ignore")]
    body_stmts: StmtsCompiled,
    /// How to compile the statement on freeze.
    stmt_compile_context: StmtCompileContext,
    /// Function can be inlined.
    pub(crate) inline_def_body: Option<InlineDefBody>,
    /// Globals captured during function or module creation.
    /// Only needed for debugger evaluation.
    pub(crate) globals: FrozenRef<'static, Globals>,
}

impl DefInfo {
    pub(crate) fn empty() -> FrozenRef<'static, DefInfo> {
        static EMPTY: Lazy<DefInfo> = Lazy::new(|| DefInfo {
            name: const_frozen_string!("<empty>"),
            signature_span: FrozenFileSpan::default(),
            parameter_captures: FrozenRef::new(&[]),
            codemap: FrozenRef::new(CodeMap::empty_static()),
            docstring: None,
            used: FrozenRef::new(&[]),
            parent: FrozenRef::new(&[]),
            stmt_compiled: Bc::default(),
            body_stmts: StmtsCompiled::empty(),
            stmt_compile_context: StmtCompileContext::default(),
            inline_def_body: None,
            globals: FrozenRef::new(Globals::empty()),
        });
        FrozenRef::new(&EMPTY)
    }

    pub(crate) fn for_module(
        codemap: FrozenRef<'static, CodeMap>,
        local_names: FrozenRef<'static, [FrozenStringValue]>,
        parent: FrozenRef<'static, [CopySlotFromParent]>,
        globals: FrozenRef<'static, Globals>,
    ) -> DefInfo {
        DefInfo {
            name: const_frozen_string!("<module>"),
            signature_span: FrozenFileSpan::default(),
            parameter_captures: FrozenRef::new(&[]),
            codemap,
            docstring: None,
            used: local_names,
            parent,
            stmt_compiled: Bc::default(),
            body_stmts: StmtsCompiled::empty(),
            stmt_compile_context: StmtCompileContext::default(),
            inline_def_body: None,
            globals,
        }
    }
}

#[derive(Clone, Debug, VisitSpanMut)]
pub(crate) struct DefCompiled {
    pub(crate) function_name: String,
    pub(crate) params: ParametersCompiled<IrSpanned<ExprCompiled>>,
    pub(crate) return_type: Option<Box<IrSpanned<ExprCompiled>>>,
    pub(crate) info: FrozenRef<'static, DefInfo>,
}

impl Compiler<'_, '_, '_> {
    fn parameter_name(&mut self, ident: CstAssignIdent) -> ParameterName {
        let binding_id = ident.1.expect("no binding for parameter");
        let binding = self.scope_data.get_binding(binding_id);
        ParameterName {
            name: ident.node.0,
            captured: binding.captured,
        }
    }

    /// Compile expression when it is expected to be interpreted as type.
    pub(crate) fn expr_for_type(
        &mut self,
        expr: Option<Box<CstExpr>>,
    ) -> Option<IrSpanned<ExprCompiled>> {
        if !self.check_types {
            return None;
        }
        let expr = self.expr_opt(expr)?;
        if let Some(value) = expr.as_value() {
            if TypeCompiled::is_wildcard_value(value.to_value()) {
                // When type is anything, skip type check.
                return None;
            }
        }
        Some(expr)
    }

    /// Compile a parameter. Return `None` for `*` pseudo parameter.
    fn parameter(
        &mut self,
        x: CstParameter,
    ) -> Option<IrSpanned<ParameterCompiled<IrSpanned<ExprCompiled>>>> {
        let span = FrameSpan::new(FrozenFileSpan::new(self.codemap, x.span));
        Some(IrSpanned {
            span,
            node: match x.node {
                ParameterP::Normal(x, t) => {
                    ParameterCompiled::Normal(self.parameter_name(x), self.expr_for_type(t))
                }
                ParameterP::WithDefaultValue(x, t, v) => ParameterCompiled::WithDefaultValue(
                    self.parameter_name(x),
                    self.expr_opt(t),
                    self.expr(*v),
                ),
                ParameterP::NoArgs => return None,
                ParameterP::Args(x, t) => {
                    ParameterCompiled::Args(self.parameter_name(x), self.expr_for_type(t))
                }
                ParameterP::KwArgs(x, t) => {
                    ParameterCompiled::KwArgs(self.parameter_name(x), self.expr_for_type(t))
                }
            },
        })
    }

    pub fn function(
        &mut self,
        name: &str,
        signature_span: FrozenFileSpan,
        scope_id: ScopeId,
        params: Vec<CstParameter>,
        return_type: Option<Box<CstExpr>>,
        suite: CstStmt,
    ) -> ExprCompiled {
        let file = self.codemap.file_span(suite.span);
        let function_name = format!("{}.{}", file.file.filename(), name);
        let name = self.eval.frozen_heap().alloc_str(name);

        // The parameters run in the scope of the parent, so compile them with the outer
        // scope
        let num_positional = params
            .iter()
            .position(|x| {
                matches!(
                    &x.node,
                    ParameterP::NoArgs | ParameterP::Args(..) | ParameterP::KwArgs(..)
                )
            })
            .unwrap_or(params.len())
            .try_into()
            .unwrap();
        let params = params
            .into_iter()
            .filter_map(|x| self.parameter(x))
            .collect();
        let params = ParametersCompiled {
            params,
            num_positional,
        };
        let return_type = self.expr_for_type(return_type).map(Box::new);

        self.enter_scope(scope_id);

        let docstring = DocString::extract_raw_starlark_docstring(&suite);
        let body = self.stmt(suite, false);
        let scope_id = self.exit_scope();
        let scope_names = self.scope_data.get_scope(scope_id);

        let has_types = return_type.is_some() || params.has_types();

        let inline_def_body = if has_types {
            // It is harder to inline if a function declares parameter types or return type.
            None
        } else {
            inline_def_body(&params, &body)
        };

        let param_count = params.count_param_variables();

        let used = self
            .eval
            .frozen_heap()
            .alloc_any_slice_display_from_debug(&scope_names.used);
        let info = self.eval.module_env.frozen_heap().alloc_any(DefInfo {
            name,
            signature_span,
            parameter_captures: self
                .eval
                .frozen_heap()
                .alloc_any_slice_display_from_debug(&params.parameter_captures()),
            codemap: self.codemap,
            docstring,
            used,
            parent: self
                .eval
                .frozen_heap()
                .alloc_any_slice_display_from_debug(&scope_names.parent),
            stmt_compiled: body.as_bc(
                &self.compile_context(return_type.is_some()),
                used,
                param_count,
                self.eval.module_env.frozen_heap(),
            ),
            body_stmts: body,
            inline_def_body,
            stmt_compile_context: self.compile_context(return_type.is_some()),
            globals: self.globals,
        });

        ExprCompiled::Def(DefCompiled {
            function_name,
            params,
            return_type,
            info,
        })
    }
}

/// Starlark function internal representation and implementation of
/// [`StarlarkValue`].
#[derive(Derivative, NoSerialize, ProvidesStaticType, Trace, Allocative)]
#[derivative(Debug)]
pub(crate) struct DefGen<V> {
    pub(crate) parameters: ParametersSpec<V>, // The parameters, **kwargs etc including defaults (which are evaluated afresh each time)
    /// Indices of parameters, which are captured in nested defs.
    /// This is a copy of `DefInfo.parameter_captures`.
    parameter_captures: FrozenRef<'static, [LocalSlotId]>,
    // The types of the parameters.
    // (Sparse indexed array, (0, argm T) implies parameter 0 named arg must have type T).
    parameter_types: Vec<(LocalSlotId, String, V, TypeCompiled)>,
    pub(crate) return_type: Option<(V, TypeCompiled)>, // The return type annotation for the function
    /// Data created during function compilation but before function instantiation.
    /// `DefInfo` can be shared by multiple `def` instances, for example,
    /// `lambda` functions can be instantiated multiple times.
    pub(crate) def_info: FrozenRef<'static, DefInfo>,
    /// Any variables captured from the outer scope (nested def/lambda).
    /// Values are either [`Value`] or [`FrozenValu`] pointing respectively to
    /// [`ValueCaptured`] or [`FrozenValueCaptured`].
    captured: Vec<V>,
    // Important to ignore these field as it probably references DefGen in a cycle
    #[derivative(Debug = "ignore")]
    /// A reference to the module where the function is defined after the module has been frozen.
    /// When the module is not frozen yet, this field contains `None`, and function's module
    /// can be accessed from evaluator's module.
    #[allocative(skip)]
    module: AtomicFrozenRefOption<FrozenModuleData>,
    /// This field is only used in `FrozenDef`. It is populated in `post_freeze`.
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    optimized_on_freeze_stmt: StmtCompiledCell,
}

impl<V> Display for DefGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.parameters.signature())
    }
}

pub(crate) type Def<'v> = DefGen<Value<'v>>;
pub(crate) type FrozenDef = DefGen<FrozenValue>;

starlark_complex_values!(Def);

impl<'v> Def<'v> {
    pub(crate) fn new(
        parameters: ParametersSpec<Value<'v>>,
        parameter_types: Vec<(LocalSlotId, String, Value<'v>, TypeCompiled)>,
        return_type: Option<(Value<'v>, TypeCompiled)>,
        stmt: FrozenRef<'static, DefInfo>,
        eval: &mut Evaluator<'v, '_>,
    ) -> Value<'v> {
        let captured = stmt
            .parent
            .as_ref()
            .map(|copy| eval.clone_slot_capture(copy, &stmt));
        eval.heap().alloc(Self {
            parameters,
            parameter_captures: stmt.parameter_captures,
            parameter_types,
            return_type,
            captured,
            module: AtomicFrozenRefOption::new(eval.module_variables),
            optimized_on_freeze_stmt: StmtCompiledCell::new(),
            def_info: stmt,
        })
    }
}

impl<'v, T1: ValueLike<'v>> DefGen<T1> {
    fn docs(&self) -> Option<DocItem> {
        let parameter_types: HashMap<usize, docs::Type> = self
            .parameter_types
            .iter()
            .map(|(idx, _, v, _)| {
                (
                    idx.0 as usize,
                    docs::Type {
                        raw_type: v.to_value().to_repr(),
                    },
                )
            })
            .collect();

        let return_type = self.return_type.as_ref().map(|r| docs::Type {
            raw_type: r.0.to_value().to_repr(),
        });

        let function_docs = docs::Function::from_docstring(
            DocStringKind::Starlark,
            self.parameters
                .documentation(parameter_types, HashMap::new()),
            return_type,
            self.def_info.docstring.as_ref().map(String::as_ref),
        );

        Some(DocItem::Function(function_docs))
    }
}

impl<'v> Freeze for Def<'v> {
    type Frozen = FrozenDef;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let parameters = self.parameters.freeze(freezer)?;
        let parameter_types = self
            .parameter_types
            .into_try_map(|(i, s, v, t)| anyhow::Ok((i, s, v.freeze(freezer)?, t)))?;
        let return_type = self.return_type.freeze(freezer)?;
        let captured = self.captured.try_map(|x| x.freeze(freezer))?;
        let module = AtomicFrozenRefOption::new(self.module.load_relaxed());
        Ok(FrozenDef {
            parameters,
            parameter_captures: self.parameter_captures,
            parameter_types,
            return_type,
            def_info: self.def_info,
            captured,
            module,
            optimized_on_freeze_stmt: self.optimized_on_freeze_stmt,
        })
    }
}

pub(crate) trait DefLike<'v> {
    const FROZEN: bool;
}

impl<'v> DefLike<'v> for DefGen<Value<'v>> {
    const FROZEN: bool = false;
}

impl<'v> DefLike<'v> for DefGen<FrozenValue> {
    const FROZEN: bool = true;
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for DefGen<V>
where
    Self: ProvidesStaticType + DefLike<'v>,
{
    starlark_type!(FUNCTION_TYPE);

    fn name_for_call_stack(&self, _me: Value<'v>) -> String {
        self.def_info.name.as_str().to_owned()
    }

    fn invoke(
        &self,
        _me: Value<'v>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.invoke_impl(&args.0, eval)
    }

    fn documentation(&self) -> Option<DocItem> {
        self.docs()
    }
}

impl<'v, V: ValueLike<'v>> DefGen<V>
where
    Self: DefLike<'v>,
{
    pub(crate) fn bc(&self) -> &Bc {
        if Self::FROZEN {
            self.optimized_on_freeze_stmt.get()
        } else {
            &self.def_info.stmt_compiled
        }
    }

    fn check_parameter_types(&self, eval: &mut Evaluator<'v, '_>) -> anyhow::Result<()> {
        let start = if eval.typecheck_profile.enabled {
            Some(Instant::now())
        } else {
            None
        };
        for (i, arg_name, ty, ty2) in &self.parameter_types {
            match eval.current_frame.get_slot(i.to_captured_or_not()) {
                None => {
                    panic!("Not allowed optional unassigned with type annotations on them")
                }
                Some(v) => v.check_type_compiled(ty.to_value(), ty2, Some(arg_name))?,
            }
        }
        if let Some(start) = start {
            eval.typecheck_profile
                .add(self.def_info.name, start.elapsed());
        }
        Ok(())
    }

    pub(crate) fn check_return_type(
        &self,
        ret: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<()> {
        let (return_type_value, return_type_ty): &(V, TypeCompiled) = self
            .return_type
            .as_ref()
            .ok_or(DefError::CheckReturnTypeNoType)?;
        let start = if eval.typecheck_profile.enabled {
            Some(Instant::now())
        } else {
            None
        };
        ret.check_type_compiled(return_type_value.to_value(), return_type_ty, None)?;
        if let Some(start) = start {
            eval.typecheck_profile
                .add(self.def_info.name, start.elapsed());
        }
        Ok(())
    }

    #[inline(always)]
    fn invoke_impl<'a, A: ArgumentsImpl<'v, 'a>>(
        &self,
        args: &A,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>>
    where
        'v: 'a,
    {
        let bc = self.bc();
        alloca_frame(eval, bc.local_count, bc.max_stack_size, |eval| {
            let slots = eval.current_frame.locals();
            self.parameters.collect_inline(args, slots, eval.heap())?;
            self.invoke_raw(eval)
        })
    }

    pub(crate) fn invoke_with_args<'a, A: ArgumentsImpl<'v, 'a>>(
        &self,
        args: &A,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>>
    where
        'v: 'a,
    {
        // This is trivial function which delegates to `invoke_impl`.
        // `invoke_impl` is called from two places,
        // giving this function different name makes this function easier to see in profiler.
        self.invoke_impl(args, eval)
    }

    /// Invoke the function, assuming that:
    /// * the frame has been allocated and stored in `eval.current_frame`
    /// * the arguments have been collected into the frame
    #[inline(always)]
    fn invoke_raw(&self, eval: &mut Evaluator<'v, '_>) -> anyhow::Result<Value<'v>> {
        // println!("invoking {}", self.def.stmt.name.node);

        if !self.parameter_types.is_empty() {
            self.check_parameter_types(eval)?;
        }

        // Parameters are collected into local slots without captures
        // (to avoid even more branches in parameter capture),
        // and this loop wraps captured parameters.
        for &captured in &*self.parameter_captures {
            eval.wrap_local_slot_captured(captured);
        }

        // Copy over the parent slots.
        // Explicitly check `self.captured` is not empty to avoid accessing
        // self.def_info.scope_names which is two indirections.
        if !self.captured.is_empty() {
            for (copy, captured) in self.def_info.parent.iter().zip(self.captured.iter()) {
                eval.current_frame.set_slot(copy.child, captured.to_value());
            }
        }

        if Self::FROZEN {
            debug_assert!(self.module.load_relaxed().is_some());
        }
        let res =
            eval.with_function_context(self.module.load_relaxed(), |eval| self.bc().run(eval));

        res.map_err(|EvalException(e)| e)
    }

    pub(crate) fn resolve_arg_name(&self, name: Hashed<&str>) -> ResolvedArgName {
        self.parameters.resolve_name(name)
    }

    pub(crate) fn dump_debug(&self) -> String {
        let mut w = String::new();
        writeln!(w, "Bytecode:").unwrap();
        self.bc()
            .dump_debug()
            .lines()
            .for_each(|l| writeln!(w, "  {}", l).unwrap());
        w
    }
}

impl FrozenDef {
    pub(crate) fn post_freeze(
        &self,
        module: FrozenRef<FrozenModuleData>,
        heap: &Heap,
        frozen_heap: &FrozenHeap,
    ) {
        // Module passed to this function is not always module where the function is declared:
        // A function can be created in a frozen module and frozen later in another module.
        // `def_module` variable contains a module where this `def` is declared.
        let def_module = match self.module.load_relaxed() {
            None => {
                self.module.store_relaxed(module);
                module
            }
            Some(module) => module,
        };

        // Now perform the optimization of function body with fully frozen module:
        // all module variables are frozen, so we can inline more aggressively.
        let body_optimized = self
            .def_info
            .body_stmts
            .optimize(&mut OptCtx::new(
                &mut OptimizeOnFreezeContext {
                    module: def_module.as_ref(),
                    heap,
                    frozen_heap,
                },
                self.parameters.len().try_into().unwrap(),
            ))
            .as_bc(
                &self.def_info.stmt_compile_context,
                self.def_info.used,
                self.parameters.len() as u32,
                frozen_heap,
            );

        // Store the optimized body.
        // This is (relatively) safe because we know that during freeze
        // nobody has a reference to stmt: nobody is executing this `def`.
        unsafe {
            self.optimized_on_freeze_stmt.set(body_optimized);
        }
    }
}
