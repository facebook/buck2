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

use std::collections::{HashMap, HashSet};

use gazebo::{prelude::*, variants::VariantName};
use thiserror::Error;

use crate::{
    analysis::{
        bind,
        bind::{Assigner, Bind, Scope},
        types::{LintT, LintWarning},
    },
    codemap::{CodeMap, Span},
    syntax::{
        ast::{Assign, AstStmt, Expr, Stmt},
        AstModule,
    },
};

#[derive(Error, Debug, VariantName)]
pub(crate) enum NameWarning {
    #[error("Unused `load` of `{0}`")]
    UnusedLoad(String),
    #[error("Unused assignment of `{0}`")]
    UnusedAssign(String),
    #[error("Unused argument `{0}`")]
    UnusedArgument(String),
    #[error("Use of unassigned variable `{0}`")]
    UsingUnassigned(String),
    #[error("Use of undefined variable `{0}`")]
    UsingUndefined(String),
    #[error("Underscore-prefixed nested function name `{0}`")]
    UnderscoreFunction(String),
    #[error("Used ignored variable `{0}`")]
    UsingIgnored(String),
}

impl LintWarning for NameWarning {
    fn is_serious(&self) -> bool {
        match self {
            Self::UsingUnassigned(..) => true,
            _ => false,
        }
    }
}

impl NameWarning {
    fn unused(typ: Assigner, codemap: &CodeMap, span: Span, name: String) -> LintT<Self> {
        LintT::new(
            codemap,
            span,
            match typ {
                Assigner::Load { .. } => NameWarning::UnusedLoad(name),
                Assigner::Argument => NameWarning::UnusedArgument(name),
                Assigner::Assign => NameWarning::UnusedAssign(name),
            },
        )
    }
}

pub(crate) fn name_warnings(
    module: &AstModule,
    globals: Option<&[&str]>,
) -> Vec<LintT<NameWarning>> {
    let mut res = Vec::new();
    let scope = bind::scope(module);
    unused_variable(&module.codemap, &scope, true, &mut res);
    duplicate_assign(&module.codemap, &scope, true, &mut res);
    unassigned_variable(&module.codemap, &scope, &mut res);
    if let Some(globals) = globals {
        undefined_variable(&module.codemap, &scope, globals, &mut res);
    }
    inappropriate_underscore(&module.codemap, &module.statement, true, &mut res);
    use_ignored(&module.codemap, &scope, None, &mut res);
    res
}

fn undefined_variable(
    codemap: &CodeMap,
    scope: &Scope,
    globals: &[&str],
    res: &mut Vec<LintT<NameWarning>>,
) {
    let globals: HashSet<&str> = globals.iter().copied().collect();
    for (name, span) in &scope.free {
        if !globals.contains(name.as_str()) {
            res.push(LintT::new(
                codemap,
                *span,
                NameWarning::UsingUndefined(name.clone()),
            ))
        }
    }
}

fn duplicate_assign(
    codemap: &CodeMap,
    scope: &Scope,
    top: bool,
    res: &mut Vec<LintT<NameWarning>>,
) {
    // If I see two set's, without any intervening flow or get, the first one was pointless
    let mut warnings: HashMap<&str, _> = HashMap::new();
    let mut captured: HashSet<&str> = HashSet::new(); // those captured by child scopes
    for x in &scope.inner {
        match x {
            Bind::Set(reason, x) => {
                let ignored = !top && x.0.starts_with('_');
                if !ignored && !captured.contains(x.0.as_str()) {
                    if let Some((span, typ)) = warnings.insert(&x.node.0, (x.span, reason)) {
                        res.push(NameWarning::unused(typ.clone(), codemap, span, x.0.clone()))
                    }
                }
            }
            Bind::Get(x) => {
                warnings.remove(x.node.as_str());
            }
            Bind::Scope(scope) => {
                duplicate_assign(codemap, scope, false, res);
                for x in scope.free.keys() {
                    warnings.remove(x.as_str());
                    captured.insert(x.as_str());
                }
            }
            Bind::Flow => warnings.clear(),
        }
    }
}

fn unused_variable(codemap: &CodeMap, scope: &Scope, top: bool, res: &mut Vec<LintT<NameWarning>>) {
    let mut warnings = HashMap::new();
    for (x, (typ, span)) in &scope.bound {
        let exported = top && *typ == Assigner::Assign && !x.starts_with('_');
        let ignored = !top && x.starts_with('_');

        // We don't want to warn about exported things or ignored things
        if !exported && !ignored {
            warnings.insert(x, (typ.clone(), *span));
        }
    }

    for x in &scope.inner {
        match x {
            Bind::Set(..) => {}
            Bind::Get(x) => {
                warnings.remove(&x.node);
            }
            Bind::Scope(scope) => {
                unused_variable(codemap, scope, false, res);
                for x in scope.free.keys() {
                    warnings.remove(x);
                }
            }
            Bind::Flow => {}
        }
    }

    for (name, (typ, span)) in warnings {
        res.push(NameWarning::unused(typ, codemap, span, name.clone()))
    }
}

fn unassigned_variable(codemap: &CodeMap, scope: &Scope, res: &mut Vec<LintT<NameWarning>>) {
    // We only look for variables that are assigned in this scope, but haven't yet been assigned
    let mut assigned: HashSet<&str> = HashSet::new();
    for x in &scope.inner {
        match x {
            Bind::Get(x)
                if scope.bound.get(&x.node).is_some() && !assigned.contains(x.as_str()) =>
            {
                res.push(LintT::new(
                    codemap,
                    x.span,
                    NameWarning::UsingUnassigned(x.node.clone()),
                ))
            }
            Bind::Set(_, x) => {
                assigned.insert(x.0.as_str());
            }
            Bind::Scope(scope) => unassigned_variable(codemap, scope, res),
            _ => {}
        }
    }
}

// There's no reason to make a def or lambda and give it an underscore name not at the top level
fn inappropriate_underscore(
    codemap: &CodeMap,
    x: &AstStmt,
    top: bool,
    res: &mut Vec<LintT<NameWarning>>,
) {
    match &**x {
        Stmt::Def(name, _, _, x, _payload) => {
            if !top && name.0.starts_with('_') {
                res.push(LintT::new(
                    codemap,
                    name.span,
                    NameWarning::UnderscoreFunction(name.0.clone()),
                ))
            }
            inappropriate_underscore(codemap, x, false, res)
        }
        Stmt::Assign(lhs, rhs) | Stmt::AssignModify(lhs, _, rhs) if !top => {
            match (&**lhs, &***rhs) {
                (Assign::Identifier(name), Expr::Lambda(..)) if name.0.starts_with('_') => res
                    .push(LintT::new(
                        codemap,
                        name.span,
                        NameWarning::UnderscoreFunction(name.node.0.clone()),
                    )),
                _ => {}
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
    res: &mut Vec<LintT<NameWarning>>,
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
                            let suffix = x.trim_start_match('_');
                            scope.free.contains_key(suffix) || scope.bound.contains_key(suffix)
                        };
                        if !defined_at_root() && !shadows() {
                            res.push(LintT::new(
                                codemap,
                                x.span,
                                NameWarning::UsingIgnored(x.node.clone()),
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
    use super::*;
    use crate::syntax::Dialect;

    impl NameWarning {
        fn about(&self) -> &String {
            match self {
                NameWarning::UnusedLoad(x) => x,
                NameWarning::UnusedAssign(x) => x,
                NameWarning::UnusedArgument(x) => x,
                NameWarning::UsingUnassigned(x) => x,
                NameWarning::UsingUndefined(x) => x,
                NameWarning::UnderscoreFunction(x) => x,
                NameWarning::UsingIgnored(x) => x,
            }
        }
    }

    fn module(x: &str) -> AstModule {
        AstModule::parse("X", x.to_owned(), &Dialect::Extended).unwrap()
    }

    #[test]
    fn test_lint_unused() {
        let m = module(
            r#"
load("test", "no1", "a")
b = a
_c = 18
_no2 = 17
d = _c
_e = 9
_no4 = 1
def magic(no3, f, _allowed):
    _ignore = f
    _no4 = 9 # shadows out _no4, which is thus unused
    no5 = 10
    (a, _) = f()
    (a2, _ignored) = f()
    f.go(_e, a, a2, _no4)
def uses_h():
    _h
_h = []
def _no6(): pass
def foo():
    array = [1,2,3]
    x = 1
    array[x] = 9
    return array
"#,
        );
        let mut res = Vec::new();
        let scope = bind::scope(&m);
        unused_variable(&m.codemap, &scope, true, &mut res);
        let mut res = res.map(|x| x.problem.about());
        res.sort();
        assert_eq!(res, &["_no2", "_no4", "_no6", "no1", "no3", "no5"]);
    }

    #[test]
    fn test_lint_duplicate_assign() {
        let m = module(
            r#"
load("test", "no1", "a")
no1 = 12
b = 8
no2 = 8
no2 = 9
c = 8
c += 1
def foo():
    no3 = 1
    no3 = 1
    _ignore = 1
    _ignore = 2
    if foo2:
        d = 1
    else:
        d = 2
no2 = 10
_no4 = 1
_no4 = 2
export = _no4

def capture_e(): print(e)
e = 8
capture_e()
e = 10
capture_e()
"#,
        );
        let mut res = Vec::new();
        let scope = bind::scope(&m);
        duplicate_assign(&m.codemap, &scope, true, &mut res);
        let mut res = res.map(|x| x.problem.about());
        res.sort();
        assert_eq!(res, &["_no4", "no1", "no2", "no2", "no3"]);
    }

    #[test]
    fn test_lint_unassigned() {
        let m = module(
            r#"
a = no1
no1 = 9
test = no3()
def foo():
    b = no2 + undefined_variable
    for x in xs:
        no2 = 18
def no3(): pass
def uses_h():
    _h
_h = []
"#,
        );
        let mut res = Vec::new();
        let scope = bind::scope(&m);
        unassigned_variable(&m.codemap, &scope, &mut res);
        let mut res = res.map(|x| x.problem.about());
        res.sort();
        assert_eq!(res, &["no1", "no2", "no3"]);
    }

    #[test]
    fn test_lint_undefined() {
        let m = module(
            r#"
load("test", imported = "more")
a = True + imported + no1
def foo():
    fail("test") + no2(t = 3)
"#,
        );
        let mut res = Vec::new();
        let scope = bind::scope(&m);
        undefined_variable(&m.codemap, &scope, &["True", "fail"], &mut res);
        let mut res = res.map(|x| x.problem.about());
        res.sort();
        assert_eq!(res, &["no1", "no2"])
    }

    #[test]
    fn test_lint_inappropriate_underscore() {
        let m = module(
            r#"
def _ok():
    def _no1():
        _no2 = lambda x: x
        _ignore = 8
"#,
        );
        let mut res = Vec::new();
        inappropriate_underscore(&m.codemap, &m.statement, true, &mut res);
        let mut res = res.map(|x| x.problem.about());
        res.sort();
        assert_eq!(res, &["_no1", "_no2"])
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
