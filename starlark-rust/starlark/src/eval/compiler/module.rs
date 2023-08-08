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

//! Compile and evaluate module top-level statements.

use std::slice;

use crate::codemap::Spanned;
use crate::const_frozen_string;
use crate::eval::bc::frame::alloca_frame;
use crate::eval::compiler::add_span_to_expr_error;
use crate::eval::compiler::expr_throw;
use crate::eval::compiler::scope::payload::CstPayload;
use crate::eval::compiler::scope::payload::CstStmt;
use crate::eval::compiler::scope::ScopeId;
use crate::eval::compiler::scope::Slot;
use crate::eval::compiler::scope::TopLevelStmtIndex;
use crate::eval::compiler::Compiler;
use crate::eval::compiler::EvalException;
use crate::eval::runtime::frame_span::FrameSpan;
use crate::eval::runtime::frozen_file_span::FrozenFileSpan;
use crate::syntax::ast::LoadP;
use crate::syntax::ast::StmtP;
use crate::typing::bindings::BindingsCollect;
use crate::typing::error::InternalError;
use crate::typing::mode::TypecheckMode;
use crate::typing::oracle::traits::OracleAny;
use crate::typing::typecheck::solve_bindings;
use crate::values::FrozenRef;
use crate::values::FrozenStringValue;
use crate::values::Value;

#[derive(Debug, thiserror::Error)]
enum ModuleError {
    #[error("No imports are available, you tried `{0}` (no call to `Evaluator.set_loader`)")]
    NoImportsAvailable(String),
    #[error("Unexpected statement (internal error)")]
    UnexpectedStatement,
    #[error("Top level stmt count mismatch (internal error)")]
    TopLevelStmtCountMismatch,
}

impl<'v> Compiler<'v, '_, '_> {
    fn eval_load(&mut self, load: Spanned<&LoadP<CstPayload>>) -> Result<(), EvalException> {
        let name = &load.node.module.node;

        let span = FrameSpan::new(FrozenFileSpan::new(self.codemap, load.span));

        let loadenv = match self.eval.loader.as_ref() {
            None => {
                return Err(add_span_to_expr_error(
                    ModuleError::NoImportsAvailable(name.to_owned()).into(),
                    span,
                    self.eval,
                ));
            }
            Some(loader) => expr_throw(loader.load(name), span, self.eval)?,
        };

        for (our_name, their_name) in &load.node.args {
            let (slot, _captured) = self
                .scope_data
                .get_assign_ident_slot(our_name, &self.codemap);
            let slot = match slot {
                Slot::Local(..) => unreachable!("symbol need to be resolved to module"),
                Slot::Module(slot) => slot,
            };
            let value = expr_throw(
                self.eval.module_env.load_symbol(&loadenv, &their_name.node),
                FrameSpan::new(FrozenFileSpan::new(
                    self.codemap,
                    our_name.span.merge(their_name.span),
                )),
                self.eval,
            )?;
            self.eval.set_slot_module(slot, value)
        }

        Ok(())
    }

    /// Compile and evaluate regular statement.
    /// Regular statement is a statement which is not `load` or a sequence of statements.
    fn eval_regular_top_level_stmt(
        &mut self,
        stmt: &mut CstStmt,
        local_names: FrozenRef<'static, [FrozenStringValue]>,
    ) -> Result<Value<'v>, EvalException> {
        if matches!(stmt.node, StmtP::Statements(_) | StmtP::Load(_)) {
            return Err(EvalException::new(
                ModuleError::UnexpectedStatement.into(),
                stmt.span,
                &self.codemap,
            ));
        }

        let stmt = self.module_top_level_stmt(stmt);
        let bc = stmt.as_bc(
            &self.compile_context(false),
            local_names,
            0,
            self.eval.module_env.frozen_heap(),
        );
        // We don't preserve locals between top level statements.
        // That is OK for now: the only locals used in module evaluation
        // are comprehension bindings.
        let local_count = local_names.len().try_into().unwrap();
        alloca_frame(
            self.eval,
            local_count,
            bc.max_stack_size,
            bc.max_loop_depth,
            |eval| eval.eval_bc(const_frozen_string!("module").to_value(), &bc),
        )
    }

    fn eval_top_level_stmt(
        &mut self,
        stmt: &mut CstStmt,
        local_names: FrozenRef<'static, [FrozenStringValue]>,
    ) -> Result<Value<'v>, EvalException> {
        let stmts: &mut [CstStmt] = match &mut stmt.node {
            StmtP::Statements(stmts) => stmts,
            _ => slice::from_mut(stmt),
        };

        if stmts.len() != self.top_level_stmt_count {
            return Err(EvalException::new(
                ModuleError::TopLevelStmtCountMismatch.into(),
                stmt.span,
                &self.codemap,
            ));
        }

        if self.last_stmt_defining_type.is_none() {
            self.typecheck(stmts)?;
        }

        let mut last = Value::new_none();
        for i in 0..stmts.len() {
            self.populate_types_in_stmts(stmts, TopLevelStmtIndex(i + 1))?;

            let stmt = &mut stmts[i];

            match &mut stmt.node {
                StmtP::Load(load) => {
                    self.eval_load(Spanned {
                        node: load,
                        span: stmt.span,
                    })?;
                    last = Value::new_none();
                }
                _ => last = self.eval_regular_top_level_stmt(stmt, local_names)?,
            }

            if Some(TopLevelStmtIndex(i)) == self.last_stmt_defining_type {
                self.typecheck(stmts)?;
            }
        }
        Ok(last)
    }

    fn typecheck(&mut self, stmts: &mut [CstStmt]) -> Result<(), EvalException> {
        if !self.eval.static_typechecking {
            return Ok(());
        }

        self.populate_types_in_stmts(stmts, TopLevelStmtIndex(stmts.len()))?;

        let BindingsCollect { bindings, .. } =
            BindingsCollect::collect(stmts, TypecheckMode::Compiler, &self.codemap)
                .map_err(InternalError::into_eval_exception)?;

        let (errors, ..) = match solve_bindings(&OracleAny, &self.globals, bindings, &self.codemap)
        {
            Ok(x) => x,
            Err(e) => return Err(e.into_eval_exception()),
        };

        if let Some(error) = errors.into_iter().next() {
            return Err(error.into_eval_exception());
        }

        Ok(())
    }

    pub(crate) fn eval_module(
        &mut self,
        mut stmt: CstStmt,
        local_names: FrozenRef<'static, [FrozenStringValue]>,
    ) -> Result<Value<'v>, EvalException> {
        self.enter_scope(ScopeId::module());
        let value = self.eval_top_level_stmt(&mut stmt, local_names)?;
        self.exit_scope();
        assert!(self.locals.is_empty());
        Ok(value)
    }
}
