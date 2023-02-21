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

use crate::analysis::bind;
use crate::analysis::bind::Bind;
use crate::analysis::bind::Scope;
use crate::analysis::types::LintT;
use crate::analysis::types::LintWarning;
use crate::codemap::CodeMap;
use crate::syntax::ast::Assign;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::DefP;
use crate::syntax::ast::Expr;
use crate::syntax::ast::Stmt;
use crate::syntax::AstModule;

#[derive(Error, Debug, VariantName)]
pub(crate) enum UnderscoreWarning {
    #[error("Underscore definitions should be simple `{0}`")]
    UnderscoreDefinition(String),
    #[error("Used ignored variable `{0}`")]
    UsingIgnored(String),
}

impl LintWarning for UnderscoreWarning {
    fn is_serious(&self) -> bool {
        false
    }
}

pub(crate) fn lint(module: &AstModule) -> Vec<LintT<UnderscoreWarning>> {
    let mut res = Vec::new();
    let scope = bind::scope(module);
    inappropriate_underscore(&module.codemap, &module.statement, true, &mut res);
    use_ignored(&module.codemap, &scope, None, &mut res);
    res
}

// There's no reason to make a def or lambda and give it an underscore name not at the top level
fn inappropriate_underscore(
    codemap: &CodeMap,
    x: &AstStmt,
    top: bool,
    res: &mut Vec<LintT<UnderscoreWarning>>,
) {
    // Is this value allowed as an assignment to a boring identifier - just tuple of vars and var
    fn is_allowed(x: &AstExpr) -> bool {
        match &**x {
            Expr::Tuple(xs) if !xs.is_empty() => {
                xs.iter().all(|x| matches!(&**x, Expr::Identifier(..)))
            }
            Expr::Identifier(..) => true,
            _ => false,
        }
    }

    match &**x {
        Stmt::Def(DefP { name, body, .. }) => {
            if !top && name.0.starts_with('_') {
                res.push(LintT::new(
                    codemap,
                    name.span,
                    UnderscoreWarning::UnderscoreDefinition(name.0.clone()),
                ))
            }
            inappropriate_underscore(codemap, body, false, res)
        }
        Stmt::Assign(lhs, type_rhs) if !top => {
            if let Assign::Identifier(name) = &**lhs {
                if name.0.starts_with('_') && !is_allowed(&type_rhs.1) {
                    res.push(LintT::new(
                        codemap,
                        name.span,
                        UnderscoreWarning::UnderscoreDefinition(name.node.0.clone()),
                    ))
                }
            }
        }
        _ => x.visit_stmt(|x| inappropriate_underscore(codemap, x, top, res)),
    }
}

// Don't want to use a variable that has been defined to be ignored
fn use_ignored(
    codemap: &CodeMap,
    scope: &Scope,
    root: Option<&Scope>,
    res: &mut Vec<LintT<UnderscoreWarning>>,
) {
    match root {
        None => {
            // Things at the top level can be ignored and used
            for x in &scope.inner {
                if let Bind::Scope(x) = x {
                    use_ignored(codemap, x, Some(scope), res)
                }
            }
        }
        Some(root) => {
            // You can only use _ variables which are defined at the root,
            // and thus must be free in this scope.
            // If you use _foo, but foo is already in this scope, you may have been avoiding shadowing.
            for x in &scope.inner {
                match x {
                    Bind::Get(x) if x.starts_with('_') => {
                        // There are two permissible reasons to use an underscore variable
                        let defined_at_root =
                            || root.bound.contains_key(&x.node) && scope.free.contains_key(&x.node);
                        let shadows = || {
                            let suffix = &x[1..];
                            scope.free.contains_key(suffix) || scope.bound.contains_key(suffix)
                        };
                        if !defined_at_root() && !shadows() {
                            res.push(LintT::new(
                                codemap,
                                x.span,
                                UnderscoreWarning::UsingIgnored(x.node.clone()),
                            ))
                        }
                    }
                    Bind::Scope(x) => use_ignored(codemap, x, Some(root), res),
                    _ => {}
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use gazebo::prelude::*;

    use super::*;
    use crate::syntax::Dialect;

    impl UnderscoreWarning {
        fn about(&self) -> &String {
            match self {
                UnderscoreWarning::UnderscoreDefinition(x) => x,
                UnderscoreWarning::UsingIgnored(x) => x,
            }
        }
    }

    fn module(x: &str) -> AstModule {
        AstModule::parse("X", x.to_owned(), &Dialect::Extended).unwrap()
    }

    #[test]
    fn test_lint_inappropriate_underscore() {
        let m = module(
            r#"
def _ok():
    def _no1(foo):
        _no2 = lambda x: x
        _no3 = 8
        _unused = foo
"#,
        );
        let mut res = Vec::new();
        inappropriate_underscore(&m.codemap, &m.statement, true, &mut res);
        let mut res = res.map(|x| x.problem.about());
        res.sort();
        assert_eq!(res, &["_no1", "_no2", "_no3"])
    }

    #[test]
    fn test_lint_use_ignored() {
        let m = module(
            r#"
def _foo(): pass
_no4 = 1
def bar():
    def _no1(): pass
    _foo()
    _no1()
    _no2 = 1
    print(_no2)
    _no3 = 1
    _no4 = 1
    print(_no4)
    def deeper():
        print(_no3)
        _foo()
def foo():
    x = 1
    for _x in range(1, 100):
        print(_x)
"#,
        );
        let mut res = Vec::new();
        let scope = bind::scope(&m);
        use_ignored(&m.codemap, &scope, None, &mut res);
        let mut res = res.map(|x| x.problem.about());
        res.sort();
        assert_eq!(res, &["_no1", "_no2", "_no3", "_no4"])
    }
}
