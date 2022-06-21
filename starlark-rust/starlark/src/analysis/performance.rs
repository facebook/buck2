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

use gazebo::variants::VariantName;
use thiserror::Error;

use crate::analysis::types::LintT;
use crate::analysis::types::LintWarning;
use crate::codemap::CodeMap;
use crate::syntax::ast::Argument;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::Expr;
use crate::syntax::AstModule;

#[derive(Error, Debug, VariantName)]
pub(crate) enum Performance {
    #[error("Dict copy `{0}` is more efficient as `{1}`")]
    DictWithoutStarStar(String, String),
}

impl LintWarning for Performance {
    fn is_serious(&self) -> bool {
        true
    }
}

fn match_dict_copy(codemap: &CodeMap, x: &AstExpr, res: &mut Vec<LintT<Performance>>) {
    // If we see `dict(**x)` suggest `dict(x)`
    match &**x {
        Expr::Call(fun, args) if args.len() == 1 => match (&***fun, &*args[0]) {
            (Expr::Identifier(f, _), Argument::KwArgs(arg)) if f.node == "dict" => {
                res.push(LintT::new(
                    codemap,
                    x.span,
                    Performance::DictWithoutStarStar(x.to_string(), format!("dict({})", arg.node)),
                ))
            }
            _ => {}
        },
        _ => {}
    }
}

fn dict_copy(module: &AstModule, res: &mut Vec<LintT<Performance>>) {
    fn check(codemap: &CodeMap, x: &AstExpr, res: &mut Vec<LintT<Performance>>) {
        match_dict_copy(codemap, x, res);
        x.visit_expr(|x| check(codemap, x, res));
    }
    module
        .statement
        .visit_expr(|x| check(&module.codemap, x, res));
}

pub(crate) fn performance(module: &AstModule) -> Vec<LintT<Performance>> {
    let mut res = Vec::new();
    dict_copy(module, &mut res);
    res
}

#[cfg(test)]
mod tests {
    use gazebo::prelude::*;

    use super::*;
    use crate::syntax::Dialect;

    fn module(x: &str) -> AstModule {
        AstModule::parse("bad.bzl", x.to_owned(), &Dialect::Extended).unwrap()
    }

    #[test]
    fn test_lint_performance() {
        let mut res = Vec::new();
        dict_copy(
            &module(
                r#"
def foo(extra, **kwargs):
    x = dict(**kwargs)
    y = dict(extra)
    return (x,y)
"#,
            ),
            &mut res,
        );
        assert_eq!(
            res.map(|x| x.to_string()),
            &["bad.bzl:3:9-23: Dict copy `dict(**kwargs)` is more efficient as `dict(kwargs)`"]
        );
    }
}
