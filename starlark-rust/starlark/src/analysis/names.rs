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
use std::collections::HashSet;

use thiserror::Error;

use crate::analysis::bind;
use crate::analysis::bind::Assigner;
use crate::analysis::bind::Bind;
use crate::analysis::bind::Scope;
use crate::analysis::types::LintT;
use crate::analysis::types::LintWarning;
use crate::codemap::CodeMap;
use crate::codemap::Span;
use crate::syntax::AstModule;

#[derive(Error, Debug)]
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
}

impl LintWarning for NameWarning {
    fn is_serious(&self) -> bool {
        match self {
            Self::UsingUnassigned(..) => true,
            _ => false,
        }
    }

    fn short_name(&self) -> &'static str {
        match self {
            Self::UnusedLoad(..) => "unused-load",
            Self::UnusedAssign(..) => "unused-assign",
            Self::UnusedArgument(..) => "unused-argument",
            Self::UsingUnassigned(..) => "using-unassigned",
            Self::UsingUndefined(..) => "using-undefined",
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

pub(crate) fn lint(
    module: &AstModule,
    globals: Option<&HashSet<String>>,
) -> Vec<LintT<NameWarning>> {
    let mut res = Vec::new();
    let scope = bind::scope(module);
    unused_variable(&module.codemap, &scope, true, &mut res);
    duplicate_assign(&module.codemap, &scope, true, &mut res);
    unassigned_variable(&module.codemap, &scope, &mut res);
    if let Some(globals) = globals {
        undefined_variable(&module.codemap, &scope, globals, &mut res);
    }
    res
}

fn undefined_variable(
    codemap: &CodeMap,
    scope: &Scope,
    globals: &HashSet<String>,
    res: &mut Vec<LintT<NameWarning>>,
) {
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
            Bind::GetDotted(x) => {
                warnings.remove(x.variable.node.as_str());
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
            Bind::GetDotted(x) => {
                warnings.remove(&x.variable.node);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::slice_vec_ext::SliceExt;
    use crate::syntax::Dialect;

    impl NameWarning {
        fn about(&self) -> &String {
            match self {
                NameWarning::UnusedLoad(x) => x,
                NameWarning::UnusedAssign(x) => x,
                NameWarning::UnusedArgument(x) => x,
                NameWarning::UsingUnassigned(x) => x,
                NameWarning::UsingUndefined(x) => x,
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
def dots(f, g):
    (d_a, d_b, d_c, d_d, d_e, d_f, d_g, d_h, d_i, d_j) = (20, 21, 22, 23, 24, 25, 26, 27, 28, 29)
    # Make sure that expressions on dotted accesses are accounted for.
    # Ignore that the integers referenced aren't functions. They're just names.
    f.foo[d_a].bar
    f.foo[d_b].bar[d_c]
    f.baz(g(["ignore", d_d]))
    f.foobar.baz(d_e, [d_f], d_g([d_h]))
    d_i(d_j).blah
    pass
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
        let globals = HashSet::from(["True".to_owned(), "fail".to_owned()]);

        let m = module(
            r#"
load("test", imported = "more")
a = True + imported + no1
def foo():
    fail("test") + no2(t = 3)
def bar(ctx):
    ctx.attrs.dep[fail.index].default_outputs[0]
    ctx[0].default_outputs
"#,
        );
        let mut res = Vec::new();
        let scope = bind::scope(&m);
        undefined_variable(&m.codemap, &scope, &globals, &mut res);
        let mut res = res.map(|x| x.problem.about());
        res.sort();
        assert_eq!(res, &["no1", "no2"])
    }
}
