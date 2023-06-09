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

use derivative::Derivative;

use crate::codemap::CodeMap;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::StmtP;
use crate::syntax::Dialect;

/// A representation of a Starlark module abstract syntax tree.
///
/// Created with either [`parse`](AstModule::parse) or [`parse_file`](AstModule::parse_file),
/// and evaluated with [`eval_module`](crate::eval::Evaluator::eval_module).
///
/// The internal details (statements/expressions) are deliberately omitted, as they change
/// more regularly. A few methods to obtain information about the AST are provided.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct AstModule {
    #[derivative(Debug = "ignore")]
    pub(crate) codemap: CodeMap,
    pub(crate) statement: AstStmt,
    pub(crate) dialect: Dialect,
}

impl AstModule {
    /// List the top-level statements in the AST.
    pub(crate) fn top_level_statements(&self) -> Vec<&AstStmt> {
        fn f<'a>(ast: &'a AstStmt, res: &mut Vec<&'a AstStmt>) {
            match &**ast {
                StmtP::Statements(xs) => {
                    for x in xs {
                        f(x, res);
                    }
                }
                _ => res.push(ast),
            }
        }

        let mut res = Vec::new();
        f(&self.statement, &mut res);
        res
    }
}
