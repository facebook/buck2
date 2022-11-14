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

use std::iter;
use std::mem;

use gazebo::dupe::Dupe;
use starlark_map::small_map;
use starlark_map::small_map::SmallMap;

use crate::codemap::CodeMap;
use crate::environment::names::MutableNames;
use crate::environment::slots::ModuleSlotId;
use crate::environment::EnvironmentError;
use crate::environment::Globals;
use crate::environment::Module;
use crate::errors::did_you_mean::did_you_mean;
use crate::errors::Diagnostic;
use crate::eval::runtime::slots::LocalSlotIdCapturedOrNot;
use crate::syntax::ast::Assign;
use crate::syntax::ast::AssignIdent;
use crate::syntax::ast::AstArgumentP;
use crate::syntax::ast::AstAssignIdentP;
use crate::syntax::ast::AstAssignP;
use crate::syntax::ast::AstExprP;
use crate::syntax::ast::AstLoadP;
use crate::syntax::ast::AstNoPayload;
use crate::syntax::ast::AstParameterP;
use crate::syntax::ast::AstPayload;
use crate::syntax::ast::AstStmtP;
use crate::syntax::ast::AstString;
use crate::syntax::ast::ClauseP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForClauseP;
use crate::syntax::ast::ParameterP;
use crate::syntax::ast::Stmt;
use crate::syntax::ast::StmtP;
use crate::syntax::ast::Visibility;
use crate::syntax::payload_map::AstPayloadFunction;
use crate::syntax::uniplate::VisitMut;
use crate::syntax::Dialect;
use crate::values::FrozenHeap;
use crate::values::FrozenRef;
use crate::values::FrozenStringValue;
use crate::values::FrozenValue;

pub(crate) struct Scope<'a> {
    pub(crate) scope_data: ScopeData,
    module: &'a MutableNames,
    frozen_heap: &'a FrozenHeap,
    pub(crate) module_bindings: SmallMap<FrozenStringValue, BindingId>,
    // The first scope is a module-level scope (including comprehensions in module scope).
    // The rest are scopes for functions (which include their comprehensions).
    locals: Vec<ScopeId>,
    unscopes: Vec<Unscope>,
    codemap: FrozenRef<'static, CodeMap>,
    globals: FrozenRef<'static, Globals>,
    pub(crate) errors: Vec<anyhow::Error>,
}

struct UnscopeBinding {
    /// Variable mappings in local scope are overwritten by comprehension variables.
    ///
    /// When we enter comprehension, we replace local scope variable slots with comprehension
    /// scope slots. This field stores the original slot in the local scope,
    /// or `None` if there was no mapping for the variable.
    ///
    /// When we pop the comprehension scope, we restore the mapping from this value.
    undo: Option<(LocalSlotIdCapturedOrNot, BindingId)>,
}

#[derive(Default)]
struct Unscope(SmallMap<FrozenStringValue, UnscopeBinding>);

#[derive(Default, Debug)]
pub(crate) struct ScopeNames {
    /// `Some` when scope is initialized.
    /// For module scope, the value is zero.
    pub param_count: Option<u32>,
    /// Slots this scope uses, including for parameters and `parent`.
    /// Indexed by [`LocalSlotId`], values are variable names.
    pub used: Vec<FrozenStringValue>,
    /// The names that are in this scope
    pub mp: SmallMap<FrozenStringValue, (LocalSlotIdCapturedOrNot, BindingId)>,
    /// Slots to copy from the parent. (index in parent, index in child).
    /// Module-level identifiers are not copied over, to avoid excess copying.
    pub parent: Vec<(LocalSlotIdCapturedOrNot, LocalSlotIdCapturedOrNot)>,
}

impl ScopeNames {
    fn set_param_count(&mut self, param_count: u32) {
        assert!(self.param_count.is_none());
        self.param_count = Some(param_count);
    }

    pub(crate) fn param_count(&self) -> u32 {
        self.param_count
            .expect("param_count must be set during analysis")
    }

    fn copy_parent(
        &mut self,
        parent_slot: LocalSlotIdCapturedOrNot,
        binding_id: BindingId,
        name: FrozenStringValue,
    ) -> LocalSlotIdCapturedOrNot {
        assert!(self.get_name(name).is_none()); // Or we'll be overwriting our variable
        let res = self.add_name(name, binding_id);
        self.parent.push((parent_slot, res));
        res
    }

    fn next_slot(&mut self, name: FrozenStringValue) -> LocalSlotIdCapturedOrNot {
        let res = LocalSlotIdCapturedOrNot(self.used.len().try_into().unwrap());
        self.used.push(name);
        res
    }

    fn add_name(
        &mut self,
        name: FrozenStringValue,
        binding_id: BindingId,
    ) -> LocalSlotIdCapturedOrNot {
        let slot = self.next_slot(name);
        let old = self.mp.insert_hashed(name.get_hashed(), (slot, binding_id));
        assert!(old.is_none());
        slot
    }

    fn add_scoped(
        &mut self,
        name: FrozenStringValue,
        binding_id: BindingId,
        unscope: &mut Unscope,
    ) -> LocalSlotIdCapturedOrNot {
        let slot = self.next_slot(name);
        let undo = match self.mp.get_mut_hashed(name.get_hashed().borrow()) {
            Some(v) => {
                let old = *v;
                *v = (slot, binding_id);
                Some(old)
            }
            None => {
                self.mp.insert_hashed(name.get_hashed(), (slot, binding_id));
                None
            }
        };
        assert!(
            unscope
                .0
                .insert_hashed(name.get_hashed(), UnscopeBinding { undo })
                .is_none()
        );
        slot
    }

    fn unscope(&mut self, unscope: Unscope) {
        for (name, UnscopeBinding { undo }) in unscope.0 {
            match undo {
                None => {
                    self.mp.remove(&name);
                }
                Some(v) => *self.mp.get_mut(&name).unwrap() = v,
            }
        }
    }

    fn get_name(&self, name: FrozenStringValue) -> Option<(LocalSlotIdCapturedOrNot, BindingId)> {
        self.mp.get_hashed(name.get_hashed().borrow()).copied()
    }
}

#[derive(Copy, Clone, Dupe, Debug)]
pub(crate) enum Slot {
    /// Top-level module scope.
    Module(ModuleSlotId),
    /// Local scope, always mutable.
    Local(LocalSlotIdCapturedOrNot),
}

impl<'a> Scope<'a> {
    fn top_scope_id(&self) -> ScopeId {
        *self.locals.last().unwrap()
    }

    fn scope_at_level(&self, level: usize) -> &ScopeNames {
        let scope_id = self.locals[level];
        self.scope_data.get_scope(scope_id)
    }

    fn scope_at_level_mut(&mut self, level: usize) -> &mut ScopeNames {
        let scope_id = self.locals[level];
        self.scope_data.mut_scope(scope_id)
    }

    pub fn enter_module(
        module: &'a MutableNames,
        frozen_heap: &'a FrozenHeap,
        scope_id: ScopeId,
        mut scope_data: ScopeData,
        code: &mut CstStmt,
        globals: FrozenRef<'static, Globals>,
        codemap: FrozenRef<'static, CodeMap>,
        dialect: &Dialect,
    ) -> Self {
        // Not really important, sanity check
        assert_eq!(scope_id, ScopeId::module());

        scope_data.mut_scope(scope_id).set_param_count(0);

        let mut locals: SmallMap<FrozenStringValue, _> = SmallMap::new();

        let existing_module_names_and_visibilites = module.all_names_and_visibilities();
        for (name, vis) in existing_module_names_and_visibilites.iter() {
            let (binding_id, _binding) = scope_data.new_binding(*vis, AssignCount::AtMostOnce);
            locals.insert_hashed(name.get_hashed(), binding_id);
        }

        Stmt::collect_defines(
            code,
            InLoop::No,
            &mut scope_data,
            frozen_heap,
            &mut locals,
            dialect,
        );

        let mut module_bindings = SmallMap::new();
        for (x, binding_id) in locals {
            let binding = scope_data.mut_binding(binding_id);
            let slot = module.add_name_visibility(x, binding.vis);
            let old_slot = mem::replace(&mut binding.slot, Some(Slot::Module(slot)));
            assert!(old_slot.is_none());
            let old_binding = module_bindings.insert_hashed(x.get_hashed(), binding_id);
            assert!(old_binding.is_none());
        }

        // Here we traverse the AST second time to collect scopes of defs
        Self::collect_defines_recursively(&mut scope_data, code, frozen_heap, dialect);
        let mut scope = Self {
            scope_data,
            frozen_heap,
            module,
            module_bindings,
            locals: vec![scope_id],
            unscopes: Vec::new(),
            codemap,
            globals,
            errors: Vec::new(),
        };
        scope.resolve_idents(code);
        scope
    }

    // Number of module slots I need, and a struct holding all scopes.
    pub fn exit_module(mut self) -> (u32, ScopeData) {
        assert!(self.locals.len() == 1);
        assert!(self.unscopes.is_empty());
        let scope_id = self.locals.pop().unwrap();
        assert!(scope_id == ScopeId::module());
        let scope = self.scope_data.get_scope(scope_id);
        assert!(scope.parent.is_empty());
        (self.module.slot_count(), self.scope_data)
    }

    fn collect_defines_in_def(
        scope_data: &mut ScopeData,
        scope_id: ScopeId,
        params: &mut [CstParameter],
        body: Option<&mut CstStmt>,
        frozen_heap: &FrozenHeap,
        dialect: &Dialect,
    ) {
        let params = params
            .iter_mut()
            .filter_map(|p| match &mut p.node {
                ParameterP::Normal(n, ..) => Some(n),
                ParameterP::WithDefaultValue(n, ..) => Some(n),
                ParameterP::NoArgs => None,
                ParameterP::Args(n, ..) => Some(n),
                ParameterP::KwArgs(n, ..) => Some(n),
            })
            .collect::<Vec<_>>();
        scope_data
            .mut_scope(scope_id)
            .set_param_count(params.len().try_into().unwrap());
        let mut locals: SmallMap<FrozenStringValue, _> = SmallMap::new();
        for p in params {
            // Subtle invariant: the slots for the params must be ordered and at the
            // beginning
            let binding_id = scope_data
                .new_binding(Visibility::Public, AssignCount::AtMostOnce)
                .0;
            p.1 = Some(binding_id);
            let old_local =
                locals.insert_hashed(frozen_heap.alloc_str_intern(&p.0).get_hashed(), binding_id);
            assert!(old_local.is_none());
        }
        if let Some(code) = body {
            Stmt::collect_defines(
                code,
                InLoop::No,
                scope_data,
                frozen_heap,
                &mut locals,
                dialect,
            );
        }
        for (name, binding_id) in locals.into_iter() {
            let slot = scope_data.mut_scope(scope_id).add_name(name, binding_id);
            let binding = scope_data.mut_binding(binding_id);
            let old_slot = mem::replace(&mut binding.slot, Some(Slot::Local(slot)));
            assert!(old_slot.is_none());
        }
    }

    fn collect_defines_recursively(
        scope_data: &mut ScopeData,
        code: &mut CstStmt,
        frozen_heap: &FrozenHeap,
        dialect: &Dialect,
    ) {
        if let StmtP::Def(_name, params, _ret, suite, scope_id) = &mut code.node {
            // Here we traverse the AST twice: once for this def scope,
            // second time below for nested defs.
            Self::collect_defines_in_def(
                scope_data,
                *scope_id,
                params,
                Some(suite),
                frozen_heap,
                dialect,
            );
        }

        code.visit_children_mut(&mut |visit| match visit {
            VisitMut::Expr(e) => {
                Self::collect_defines_recursively_in_expr(scope_data, e, frozen_heap, dialect)
            }
            VisitMut::Stmt(s) => {
                Self::collect_defines_recursively(scope_data, s, frozen_heap, dialect)
            }
        });
    }

    fn collect_defines_recursively_in_expr(
        scope_data: &mut ScopeData,
        code: &mut CstExpr,
        frozen_heap: &FrozenHeap,
        dialect: &Dialect,
    ) {
        if let ExprP::Lambda(params, _expr, scope_id) = &mut code.node {
            Self::collect_defines_in_def(scope_data, *scope_id, params, None, frozen_heap, dialect);
        }

        code.visit_expr_mut(|e| {
            Self::collect_defines_recursively_in_expr(scope_data, e, frozen_heap, dialect)
        });
    }

    fn resolve_idents(&mut self, code: &mut CstStmt) {
        match &mut code.node {
            StmtP::Def(_name, params, ret, body, scope_id) => self.resolve_idents_in_def(
                *scope_id,
                params,
                ret.as_mut().map(|r| &mut **r),
                Some(body),
                None,
            ),
            _ => code.visit_children_mut(|visit| match visit {
                VisitMut::Stmt(stmt) => self.resolve_idents(stmt),
                VisitMut::Expr(expr) => self.resolve_idents_in_expr(expr),
            }),
        }
    }

    fn resolve_idents_in_assign(&mut self, assign: &mut CstAssign) {
        assign.visit_expr_mut(|expr| self.resolve_idents_in_expr(expr));
    }

    fn resolve_idents_in_def(
        &mut self,
        scope_id: ScopeId,
        params: &mut [CstParameter],
        ret: Option<&mut CstExpr>,
        body_stmt: Option<&mut CstStmt>,
        body_expr: Option<&mut CstExpr>,
    ) {
        for param in params {
            param.visit_expr_mut(|expr| self.resolve_idents_in_expr(expr));
        }
        if let Some(ret) = ret {
            self.resolve_idents_in_expr(ret);
        }

        self.enter_def(scope_id);
        if let Some(body_stmt) = body_stmt {
            self.resolve_idents(body_stmt);
        }
        if let Some(body_expr) = body_expr {
            self.resolve_idents_in_expr(body_expr);
        }
        self.exit_def();
    }

    fn resolve_idents_in_expr(&mut self, expr: &mut CstExpr) {
        match &mut expr.node {
            ExprP::Identifier(ident, slot) => self.resolve_ident(ident, slot),
            ExprP::Lambda(params, body, scope_id) => {
                self.resolve_idents_in_def(*scope_id, params, None, None, Some(body))
            }
            ExprP::ListComprehension(expr, first_for, clauses) => {
                self.resolve_idents_in_compr(&mut [expr], first_for, clauses)
            }
            ExprP::DictComprehension(k_v, first_for, clauses) => {
                let (k, v) = &mut **k_v;
                self.resolve_idents_in_compr(&mut [k, v], first_for, clauses)
            }
            _ => expr.visit_expr_mut(|expr| self.resolve_idents_in_expr(expr)),
        }
    }

    fn current_scope_all_visible_names_for_did_you_mean(&self) -> Vec<FrozenStringValue> {
        // It is OK to return non-unique identifiers
        let mut r: Vec<FrozenStringValue> = Vec::new();
        for &scope_id in self.locals.iter().rev() {
            let scope = self.scope_data.get_scope(scope_id);
            r.extend(scope.mp.keys().copied());
        }
        r.extend(self.module_bindings.keys().copied());
        r.extend(self.globals.names());
        r
    }

    fn variable_not_found_err(&self, ident: &AstString) -> anyhow::Error {
        let variants = self.current_scope_all_visible_names_for_did_you_mean();
        let better = did_you_mean(ident, variants.iter().map(|s| s.as_str()));
        Diagnostic::new(
            match better {
                Some(better) => EnvironmentError::VariableNotFoundDidYouMean(
                    ident.node.clone(),
                    better.to_owned(),
                ),
                None => EnvironmentError::VariableNotFound(ident.node.clone()),
            },
            ident.span,
            &self.codemap,
        )
    }

    fn resolve_ident(&mut self, ident: &AstString, resolved_ident: &mut Option<ResolvedIdent>) {
        assert!(resolved_ident.is_none());
        *resolved_ident = Some(
            match self.get_name(self.frozen_heap.alloc_str_intern(ident)) {
                None => {
                    // Must be a global, since we know all variables
                    match self.globals.get_frozen(ident) {
                        None => {
                            self.errors.push(self.variable_not_found_err(ident));
                            return;
                        }
                        Some(v) => ResolvedIdent::Global(v),
                    }
                }
                Some(slot) => ResolvedIdent::Slot(slot),
            },
        );
    }

    fn resolve_idents_in_compr(
        &mut self,
        exprs: &mut [&mut CstExpr],
        first_for: &mut ForClauseP<CstPayload>,
        clauses: &mut [ClauseP<CstPayload>],
    ) {
        // First for is resolved in outer scope
        self.resolve_idents_in_for_clause(first_for);

        self.enter_compr();

        // Add identifiers to compr scope

        self.add_compr(
            iter::once(&mut first_for.var).chain(clauses.iter_mut().filter_map(
                |clause| match clause {
                    ClauseP::For(for_clause) => Some(&mut for_clause.var),
                    ClauseP::If(..) => None,
                },
            )),
        );

        // Now resolve idents in compr scope

        for clause in clauses.iter_mut() {
            match clause {
                ClauseP::For(for_clause) => self.resolve_idents_in_for_clause(for_clause),
                ClauseP::If(cond) => self.resolve_idents_in_expr(cond),
            }
        }

        // Finally, resolve the item expression

        for expr in exprs {
            self.resolve_idents_in_expr(expr);
        }

        self.exit_compr();
    }

    fn resolve_idents_in_for_clause(&mut self, for_clause: &mut ForClauseP<CstPayload>) {
        self.resolve_idents_in_expr(&mut for_clause.over);
        self.resolve_idents_in_assign(&mut for_clause.var);
    }

    pub fn enter_def(&mut self, scope_id: ScopeId) {
        assert!(scope_id != ScopeId::module());
        self.locals.push(scope_id);
    }

    // Which slots to grab from the current scope to the parent scope, size of your
    // self scope Future state: Should return the slots to use from the parent
    // scope
    pub fn exit_def(&mut self) -> &mut ScopeNames {
        let scope_id = self.locals.pop().unwrap();
        self.scope_data.mut_scope(scope_id)
    }

    fn enter_compr(&mut self) {
        self.unscopes.push(Unscope::default());
    }

    fn add_compr<'x>(&mut self, var: impl IntoIterator<Item = &'x mut CstAssign>) {
        let scope_id = self.top_scope_id();
        let mut locals = SmallMap::new();
        for var in var {
            Assign::collect_defines_lvalue(
                var,
                InLoop::Yes,
                &mut self.scope_data,
                self.frozen_heap,
                &mut locals,
            );
        }
        for (name, binding_id) in locals.into_iter() {
            let slot = self.scope_data.mut_scope(scope_id).add_scoped(
                name,
                binding_id,
                self.unscopes.last_mut().unwrap(),
            );
            let binding = self.scope_data.mut_binding(binding_id);
            assert!(mem::replace(&mut binding.slot, Some(Slot::Local(slot))).is_none());
        }
    }

    fn exit_compr(&mut self) {
        self.scope_data
            .mut_scope(self.top_scope_id())
            .unscope(self.unscopes.pop().unwrap());
    }

    fn get_name(&mut self, name: FrozenStringValue) -> Option<(Slot, BindingId)> {
        // look upwards to find the first place the variable occurs
        // then copy that variable downwards
        for i in (0..self.locals.len()).rev() {
            if let Some((mut v, binding_id)) = self.scope_at_level(i).get_name(name) {
                if i + 1 != self.locals.len() {
                    self.scope_data.mut_binding(binding_id).captured = Captured::Yes;
                }
                for j in (i + 1)..self.locals.len() {
                    v = self.scope_at_level_mut(j).copy_parent(v, binding_id, name);
                }
                return Some((Slot::Local(v), binding_id));
            }
        }
        let binding_id = self
            .module_bindings
            .get_hashed(name.get_hashed().borrow())
            .copied();
        match binding_id {
            Some(binding_id) => {
                let binding = self.scope_data.mut_binding(binding_id);
                if self.locals.len() > 1 {
                    binding.captured = Captured::Yes;
                }
                Some((binding.slot.unwrap(), binding_id))
            }
            None => None,
        }
    }
}

/// While performing analysis.
#[derive(Copy, Clone, Dupe)]
enum InLoop {
    /// Current statement has an enclosing loop in the current scope.
    Yes,
    /// Current statement has no enclosing loop in the current scope.
    No,
}

impl Stmt {
    // Collect all the variables that are defined in this scope
    fn collect_defines<'a>(
        stmt: &'a mut CstStmt,
        in_loop: InLoop,
        scope_data: &mut ScopeData,
        frozen_heap: &FrozenHeap,
        result: &mut SmallMap<FrozenStringValue, BindingId>,
        dialect: &Dialect,
    ) {
        match &mut stmt.node {
            StmtP::Assign(dest, _) | StmtP::AssignModify(dest, _, _) => {
                Assign::collect_defines_lvalue(dest, in_loop, scope_data, frozen_heap, result);
            }
            StmtP::For(dest, over_body) => {
                let (_over, body) = &mut **over_body;
                Assign::collect_defines_lvalue(dest, InLoop::Yes, scope_data, frozen_heap, result);
                StmtP::collect_defines(body, InLoop::Yes, scope_data, frozen_heap, result, dialect);
            }
            StmtP::Def(name, ..) => AssignIdent::collect_assign_ident(
                name,
                in_loop,
                Visibility::Public,
                scope_data,
                frozen_heap,
                result,
            ),
            StmtP::Load(load) => {
                let vis = dialect.load_visibility();
                for (name, _) in &mut load.node.args {
                    let mut vis = vis;
                    if Module::default_visibility(&name.0) == Visibility::Private {
                        vis = Visibility::Private;
                    }
                    AssignIdent::collect_assign_ident(
                        name,
                        in_loop,
                        vis,
                        scope_data,
                        frozen_heap,
                        result,
                    );
                }
            }
            stmt => stmt.visit_stmt_mut(|x| {
                Stmt::collect_defines(x, in_loop, scope_data, frozen_heap, result, dialect)
            }),
        }
    }
}

impl AssignIdent {
    fn collect_assign_ident<'a>(
        assign: &'a mut CstAssignIdent,
        in_loop: InLoop,
        vis: Visibility,
        scope_data: &mut ScopeData,
        frozen_heap: &FrozenHeap,
        result: &mut SmallMap<FrozenStringValue, BindingId>,
    ) {
        // Helper function to untangle lifetimes: we read and modify `assign` fields.
        fn assign_ident_impl<'b>(
            name: FrozenStringValue,
            binding: &'b mut Option<BindingId>,
            in_loop: InLoop,
            mut vis: Visibility,
            scope_data: &mut ScopeData,
            result: &mut SmallMap<FrozenStringValue, BindingId>,
        ) {
            assert!(
                binding.is_none(),
                "binding can be assigned only once: `{}`",
                name.as_str()
            );
            if vis == Visibility::Public {
                vis = Module::default_visibility(&name);
            }
            match result.entry_hashed(name.get_hashed()) {
                small_map::Entry::Occupied(e) => {
                    let prev_binding_id = *e.get();
                    let prev_binding = scope_data.mut_binding(prev_binding_id);
                    // If we are in the map as Public and Private, then Public wins.
                    // Everything but Load is definitely Public.
                    // So only insert if it wasn't already there.
                    if vis == Visibility::Public {
                        prev_binding.vis = Visibility::Public;
                    }
                    prev_binding.assign_count = AssignCount::Any;
                    *binding = Some(prev_binding_id);
                }
                small_map::Entry::Vacant(e) => {
                    let assign_count = match in_loop {
                        InLoop::Yes => AssignCount::Any,
                        InLoop::No => AssignCount::AtMostOnce,
                    };
                    let (new_binding_id, _) = scope_data.new_binding(vis, assign_count);
                    e.insert(new_binding_id);
                    *binding = Some(new_binding_id);
                }
            };
        }
        assign_ident_impl(
            frozen_heap.alloc_str_intern(&assign.node.0),
            &mut assign.node.1,
            in_loop,
            vis,
            scope_data,
            result,
        );
    }
}

impl Assign {
    // Collect variables defined in an expression on the LHS of an assignment (or
    // for variable etc)
    fn collect_defines_lvalue<'a>(
        expr: &'a mut CstAssign,
        in_loop: InLoop,
        scope_data: &mut ScopeData,
        frozen_heap: &FrozenHeap,
        result: &mut SmallMap<FrozenStringValue, BindingId>,
    ) {
        expr.node.visit_lvalue_mut(|x| {
            AssignIdent::collect_assign_ident(
                x,
                in_loop,
                Visibility::Public,
                scope_data,
                frozen_heap,
                result,
            )
        });
    }
}

/// Storage of objects referenced by AST.
#[derive(Default)]
pub(crate) struct ScopeData {
    bindings: Vec<Binding>,
    scopes: Vec<ScopeNames>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum AssignCount {
    /// Variable is assigned at most once during the execution of the scope.
    AtMostOnce,
    /// Variable may be assigned more than once during execution of the scope.
    Any,
}

/// Was a binding captured by nested def or lambda scopes?
#[derive(Debug, Copy, Clone, Dupe, VisitSpanMut)]
pub(crate) enum Captured {
    Yes,
    No,
}

/// Binding defines a place for a variable.
///
/// For example, in code `x = 1; x = 2`, there's one binding for name `x`.
///
/// In code `x = 1; def f(): x = 2`, there are two bindings for name `x`.
#[derive(Debug)]
pub(crate) struct Binding {
    pub(crate) vis: Visibility,
    /// `slot` is `None` when it is not initialized yet.
    /// When analysis is completed, `slot` is always `Some`.
    pub(crate) slot: Option<Slot>,
    pub(crate) assign_count: AssignCount,
    // Whether a variable defined in a scope gets captured in nested def or lambda scope.
    // (Comprehension scopes do not count, because they are considered
    // local by the runtime and do not allocate a frame).
    pub(crate) captured: Captured,
}

impl Binding {
    fn new(vis: Visibility, assign_count: AssignCount) -> Binding {
        Binding {
            vis,
            slot: None,
            assign_count,
            captured: Captured::No,
        }
    }
}

/// If of a binding within current module.
#[derive(Copy, Clone, Dupe, Debug, Hash, PartialEq, Eq)]
pub(crate) struct BindingId(usize);

/// Id of a scope within current module.
#[derive(Copy, Clone, Dupe, Debug, Eq, PartialEq)]
pub(crate) struct ScopeId(usize);

impl ScopeId {
    pub(crate) fn module() -> ScopeId {
        ScopeId(0)
    }
}

impl ScopeData {
    pub(crate) fn new() -> ScopeData {
        ScopeData::default()
    }

    pub(crate) fn get_binding(&self, BindingId(id): BindingId) -> &Binding {
        &self.bindings[id]
    }

    fn mut_binding(&mut self, BindingId(id): BindingId) -> &mut Binding {
        &mut self.bindings[id]
    }

    fn new_binding(
        &mut self,
        vis: Visibility,
        assigned_count: AssignCount,
    ) -> (BindingId, &mut Binding) {
        let binding_id = BindingId(self.bindings.len());
        self.bindings.push(Binding::new(vis, assigned_count));
        (binding_id, self.bindings.last_mut().unwrap())
    }

    pub(crate) fn get_scope(&self, ScopeId(id): ScopeId) -> &ScopeNames {
        &self.scopes[id]
    }

    pub(crate) fn mut_scope(&mut self, ScopeId(id): ScopeId) -> &mut ScopeNames {
        &mut self.scopes[id]
    }

    pub(crate) fn new_scope(&mut self) -> (ScopeId, &mut ScopeNames) {
        let scope_id = ScopeId(self.scopes.len());
        self.scopes.push(ScopeNames::default());
        (scope_id, self.scopes.last_mut().unwrap())
    }

    /// Get resolved slot for assigning identifier.
    pub(crate) fn get_assign_ident_slot(&self, ident: &CstAssignIdent) -> (Slot, Captured) {
        let binding_id = ident.1.expect("binding not assigned for ident");
        let binding = self.get_binding(binding_id);
        let slot = binding.slot;
        (
            slot.expect("binding slot is not initialized"),
            binding.captured,
        )
    }
}

#[derive(Debug)]
pub(crate) enum ResolvedIdent {
    Slot((Slot, BindingId)),
    Global(FrozenValue),
}

// We use CST as acronym for compiler-specific AST.

/// Compiler-specific AST payload.
#[derive(Debug)]
pub(crate) struct CstPayload;
impl AstPayload for CstPayload {
    /// Information about how identifier binding is resolved.
    ///
    /// This is `None` when CST is created.
    /// All payload objects are filled with binding ids for all assign idents
    /// during analysis.
    ///
    /// When compilation starts, all payloads are `Some`.
    type IdentPayload = Option<ResolvedIdent>;
    /// Binding for an identifier in assignment position.
    ///
    /// This is `None` when CST is created.
    /// All payload objects are filled with binding ids for all assign idents
    /// during analysis.
    ///
    /// When compilation starts, all payloads are `Some`.
    type IdentAssignPayload = Option<BindingId>;
    type DefPayload = ScopeId;
}

pub(crate) struct CompilerAstMap<'a>(pub(crate) &'a mut ScopeData);
impl AstPayloadFunction<AstNoPayload, CstPayload> for CompilerAstMap<'_> {
    fn map_ident(&mut self, (): ()) -> Option<ResolvedIdent> {
        None
    }

    fn map_ident_assign(&mut self, (): ()) -> Option<BindingId> {
        None
    }

    fn map_def(&mut self, (): ()) -> ScopeId {
        self.0.new_scope().0
    }
}

pub(crate) type CstExpr = AstExprP<CstPayload>;
pub(crate) type CstAssign = AstAssignP<CstPayload>;
pub(crate) type CstAssignIdent = AstAssignIdentP<CstPayload>;
pub(crate) type CstArgument = AstArgumentP<CstPayload>;
pub(crate) type CstParameter = AstParameterP<CstPayload>;
pub(crate) type CstStmt = AstStmtP<CstPayload>;
pub(crate) type CstLoad = AstLoadP<CstPayload>;

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use gazebo::dupe::Dupe;

    use crate::environment::names::MutableNames;
    use crate::environment::Globals;
    use crate::eval::compiler::scope::AssignCount;
    use crate::eval::compiler::scope::Captured;
    use crate::eval::compiler::scope::CompilerAstMap;
    use crate::eval::compiler::scope::CstAssign;
    use crate::eval::compiler::scope::CstAssignIdent;
    use crate::eval::compiler::scope::CstExpr;
    use crate::eval::compiler::scope::CstStmt;
    use crate::eval::compiler::scope::ResolvedIdent;
    use crate::eval::compiler::scope::Scope;
    use crate::eval::compiler::scope::ScopeData;
    use crate::eval::compiler::scope::Slot;
    use crate::syntax::ast::ExprP;
    use crate::syntax::ast::StmtP;
    use crate::syntax::uniplate::Visit;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;
    use crate::values::FrozenHeap;
    use crate::values::FrozenRef;

    fn test_with_module(program: &str, expected: &str, module: &MutableNames) {
        let ast = AstModule::parse("t.star", program.to_owned(), &Dialect::Extended).unwrap();
        let mut scope_data = ScopeData::new();
        let root_scope_id = scope_data.new_scope().0;
        let mut cst = ast
            .statement
            .into_map_payload(&mut CompilerAstMap(&mut scope_data));
        let frozen_heap = FrozenHeap::new();
        let codemap = frozen_heap.alloc_any_display_from_debug(ast.codemap.dupe());
        let scope = Scope::enter_module(
            module,
            &frozen_heap,
            root_scope_id,
            scope_data,
            &mut cst,
            FrozenRef::new(Globals::empty()),
            codemap,
            &Dialect::Extended,
        );
        assert!(scope.errors.is_empty());
        let (.., scope_data) = scope.exit_module();
        let mut r = String::new();
        for (i, binding) in scope_data.bindings.iter().enumerate() {
            if i != 0 {
                r.push(' ');
            }
            let slot = match binding.slot.unwrap() {
                Slot::Module(slot) => format!("m={}", slot.0),
                Slot::Local(slot) => format!("l={}", slot.0),
            };
            let assign_count = match binding.assign_count {
                AssignCount::AtMostOnce => "",
                AssignCount::Any => "+",
            };
            let captured = match binding.captured {
                Captured::Yes => "&",
                Captured::No => "",
            };
            write!(r, "{}:{}{}{}", i, slot, assign_count, captured).unwrap();
        }

        write!(r, " |").unwrap();

        struct Visitor<'a> {
            r: &'a mut String,
        }

        impl Visitor<'_> {
            fn visit_expr(&mut self, expr: &CstExpr) {
                if let ExprP::Identifier(ident, resolved) = &expr.node {
                    let resolved = match resolved.as_ref().unwrap() {
                        ResolvedIdent::Slot((_slot, binding_id)) => binding_id.0.to_string(),
                        ResolvedIdent::Global(_) => "G".to_owned(),
                    };
                    write!(&mut self.r, " {}:{}", ident.node, resolved).unwrap();
                }

                expr.visit_expr(|expr| self.visit_expr(expr));
            }

            fn visit_exprs<'a>(&mut self, exprs: impl IntoIterator<Item = &'a CstExpr>) {
                for expr in exprs {
                    self.visit_expr(expr);
                }
            }

            fn visit_lvalue(&mut self, ident: &CstAssignIdent) {
                write!(&mut self.r, " {}:{}", ident.0, ident.1.unwrap().0).unwrap();
            }

            fn visit_stmt_children(&mut self, stmt: &CstStmt) {
                stmt.visit_children(|visit| match visit {
                    Visit::Stmt(stmt) => self.visit_stmt(stmt),
                    Visit::Expr(expr) => self.visit_expr(expr),
                });
            }

            fn visit_assign(&mut self, assign: &CstAssign) {
                assign.visit_lvalue(|ident| self.visit_lvalue(ident));
            }

            fn visit_stmt(&mut self, stmt: &CstStmt) {
                match &stmt.node {
                    StmtP::Assign(lhs, _rhs) => self.visit_assign(lhs),
                    StmtP::Def(name, params, ..) => {
                        self.visit_lvalue(name);
                        for param in params {
                            let (name, def, typ) = param.split();
                            if let Some(name) = name {
                                self.visit_lvalue(name);
                            }
                            self.visit_exprs(def);
                            self.visit_exprs(typ);
                        }
                    }
                    StmtP::For(assign, ..) => self.visit_assign(assign),
                    _ => {}
                }

                self.visit_stmt_children(stmt);
            }
        }

        Visitor { r: &mut r }.visit_stmt(&cst);

        assert_eq!(expected, &r);
    }

    fn t(program: &str, expected: &str) {
        let module = MutableNames::new();
        test_with_module(program, expected, &module);
    }

    // Expected test output (second parameter to `t` function) is:
    // * list of bindings in format like `1:l=2` means binding id = 1, local slot 2
    // * list of variables with references to binding ids

    #[test]
    fn basic() {
        t("x = 1; y = 2", "0:m=0 1:m=1 | x:0 y:1");
    }

    #[test]
    fn module_reassignment() {
        t("x = 1; x = 2", "0:m=0+ | x:0 x:0");
    }

    #[test]
    fn reassignment_in_loop() {
        t("for x in []: y = x", "0:m=0+ 1:m=1+ | x:0 y:1 x:0");
    }

    #[test]
    fn def_capture() {
        t("x = 1\ndef f(): x", "0:m=0& 1:m=1 | x:0 f:1 x:0");
    }

    #[test]
    fn def_shadow() {
        t("x = 1\ndef f(): x = 2", "0:m=0 1:m=1 2:l=0 | x:0 f:1 x:2");
    }

    #[test]
    fn def_param_bindings() {
        t("def f(x): return x", "0:m=0 1:l=0 | f:0 x:1 x:1");
    }

    #[test]
    fn nested_def_capture() {
        t(
            "\
def f():
    x = 1
    def g(): return x",
            "0:m=0 1:l=0& 2:l=1 | f:0 x:1 g:2 x:1",
        )
    }

    #[test]
    fn existing_module_with_names() {
        let frozen_heap = FrozenHeap::new();
        let module = MutableNames::new();
        module.add_name(frozen_heap.alloc_str_intern("x"));
        module.add_name(frozen_heap.alloc_str_intern("y"));
        test_with_module("x = y", "0:m=0+ 1:m=1 | x:0 y:1", &module);
    }
}
