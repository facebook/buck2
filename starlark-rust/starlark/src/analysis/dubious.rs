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

use std::collections::HashMap;

use gazebo::variants::VariantName;
use num_bigint::BigInt;
use thiserror::Error;

use crate::{
    analysis::types::{LintT, LintWarning},
    codemap::{CodeMap, FileSpan, Span},
    syntax::{
        ast::{AstExpr, AstLiteral, Expr},
        lexer::TokenInt,
        AstModule,
    },
    values::num::Num,
};

#[derive(Error, Debug, VariantName)]
pub(crate) enum Dubious {
    #[error("Duplicate dictionary key `{0}`, also used at {1}")]
    DuplicateKey(String, FileSpan),
}

impl LintWarning for Dubious {
    fn is_serious(&self) -> bool {
        true
    }
}

// Go implementation of Starlark disallows duplicate top-level assignments,
// it's likely that will become Starlark standard sooner or later, so check now.
// The one place we allow it is to export something you grabbed with load.
fn duplicate_dictionary_key(module: &AstModule, res: &mut Vec<LintT<Dubious>>) {
    #[derive(PartialEq, Eq, Hash)]
    enum Key<'a> {
        Int(i32),
        BigInt(&'a BigInt),
        Float(u64),
        String(&'a str),
        Identifier(&'a str),
    }

    fn to_key<'a>(x: &'a AstExpr) -> Option<(Key<'a>, Span)> {
        match &**x {
            Expr::Literal(x) => match x {
                AstLiteral::Int(x) => match &x.node {
                    TokenInt::I32(i) => Some((Key::Int(*i), x.span)),
                    TokenInt::BigInt(i) => Some((Key::BigInt(i), x.span)),
                },
                AstLiteral::Float(x) => {
                    let n = Num::from(x.node);
                    if let Some(i) = n.as_int() {
                        // make an integer float always collide with other ints
                        Some((Key::Int(i), x.span))
                    } else {
                        // use bits representation of float to be able to always compare them for equality
                        // First normalise -0.0
                        let v = if x.node == 0.0 { 0.0 } else { x.node };
                        Some((Key::Float(v.to_bits()), x.span))
                    }
                }
                AstLiteral::String(x) => Some((Key::String(&x.node), x.span)),
            },
            Expr::Identifier(x, ()) => Some((Key::Identifier(&x.node), x.span)),
            _ => None,
        }
    }

    fn expr<'a>(x: &'a AstExpr, codemap: &CodeMap, res: &mut Vec<LintT<Dubious>>) {
        match &**x {
            Expr::Dict(args) => {
                let mut seen = HashMap::new();
                for (key, _) in args {
                    if let Some((key_id, pos)) = to_key(key) {
                        if let Some(old) = seen.insert(key_id, pos) {
                            res.push(LintT::new(
                                codemap,
                                old,
                                Dubious::DuplicateKey(key.to_string(), codemap.file_span(pos)),
                            ))
                        }
                    }
                }
            }
            _ => {}
        }
        x.visit_expr(|x| expr(x, codemap, res));
    }

    module
        .statement
        .visit_expr(|x| expr(x, &module.codemap, res))
}

pub(crate) fn dubious(module: &AstModule) -> Vec<LintT<Dubious>> {
    let mut res = Vec::new();
    duplicate_dictionary_key(module, &mut res);
    res
}

#[cfg(test)]
mod tests {
    use gazebo::prelude::*;

    use super::*;
    use crate::syntax::Dialect;

    fn module(x: &str) -> AstModule {
        AstModule::parse("X", x.to_owned(), &Dialect::Extended).unwrap()
    }

    impl Dubious {
        fn about(&self) -> &String {
            match self {
                Dubious::DuplicateKey(x, _) => x,
            }
        }
    }

    #[test]
    fn test_lint_duplicate_keys() {
        let m = module(
            r#"
{'no1': 1, 'no1': 2}
{42: 1, 78: 9, 'no2': 100, 42: 6, 'no2': 8}
{123.0: "f", 123: "i"}
{0.25: "frac", 25e-2: "exp"}

# Variables can't change as a result of expression evaluation,
# so it's always an error if you see the same expression
{no3: 1, no4: 2, yes: 3, no3: 1, no3: 3, no4: 8}

# Functions can change each time round, so don't lint on them.
{f(): 1, f(): 2}
"#,
        );
        let mut res = Vec::new();
        duplicate_dictionary_key(&m, &mut res);
        assert_eq!(
            res.map(|x| x.problem.about()),
            &[
                "\"no1\"", "42", "\"no2\"", "123", "0.25", "no3", "no3", "no4"
            ]
        );
    }
}
