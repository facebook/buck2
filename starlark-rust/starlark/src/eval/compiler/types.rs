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

use crate::eval::compiler::expr::ExprCompiled;
use crate::eval::compiler::scope::CstTypeExpr;
use crate::eval::compiler::span::IrSpanned;
use crate::eval::compiler::Compiler;
use crate::values::typing::TypeCompiled;

impl Compiler<'_, '_, '_> {
    /// Compile expression when it is expected to be interpreted as type.
    pub(crate) fn expr_for_type(
        &mut self,
        expr: Option<Box<CstTypeExpr>>,
    ) -> Option<IrSpanned<ExprCompiled>> {
        if !self.check_types {
            return None;
        }
        let expr = self.expr(expr?.node.expr);
        if let Some(value) = expr.as_value() {
            if TypeCompiled::is_wildcard_value(value.to_value()) {
                // When type is anything, skip type check.
                return None;
            }
        }
        Some(expr)
    }
}
