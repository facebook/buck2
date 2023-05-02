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

use crate::codemap::FileSpan;
use crate::collections::SmallMap;
use crate::syntax::ast::AstAssignIdent;
use crate::syntax::ast::DefP;
use crate::syntax::ast::Expr;
use crate::syntax::ast::Stmt;
use crate::syntax::AstModule;

/// The type of an exported symbol.
/// If unknown, will use `Variable`.
#[derive(Debug)]
pub enum ExportedSymbolKind {
    /// Any kind of symbol.
    Any,
    /// The symbol represents something that can be called, for example
    /// a `def` or a variable assigned to a `lambda`.
    Function,
}

/// An exported symbol. Returned from [`AstModule::exported_symbols`].
#[derive(Debug)]
pub struct ExportedSymbol<'a> {
    /// The name of the symbol.
    pub name: &'a str,
    /// The location of its definition.
    pub span: FileSpan,
    /// The type of symbol it represents.
    pub kind: ExportedSymbolKind,
}

impl AstModule {
    /// Which symbols are exported by this module. These are the top-level assignments,
    /// including function definitions. Any symbols that start with `_` are not exported.
    pub fn exported_symbols<'a>(&'a self) -> Vec<ExportedSymbol<'a>> {
        // Map since we only want to store the first of each export
        // IndexMap since we want the order to match the order they were defined in
        let mut result: SmallMap<&'a str, _> = SmallMap::new();

        fn add<'a>(
            me: &AstModule,
            result: &mut SmallMap<&'a str, ExportedSymbol<'a>>,
            name: &'a AstAssignIdent,
            kind: ExportedSymbolKind,
        ) {
            if !name.0.starts_with('_') {
                result.entry(&name.0).or_insert(ExportedSymbol {
                    name: &name.0,
                    span: me.file_span(name.span),
                    kind,
                });
            }
        }

        for x in self.top_level_statements() {
            match &**x {
                Stmt::Assign(dest, rhs) => {
                    dest.visit_lvalue(|name| {
                        let kind = match &*rhs.1 {
                            Expr::Lambda(..) => ExportedSymbolKind::Function,
                            _ => ExportedSymbolKind::Any,
                        };
                        add(self, &mut result, name, kind);
                    });
                }
                Stmt::AssignModify(dest, _, _) => {
                    dest.visit_lvalue(|name| {
                        add(self, &mut result, name, ExportedSymbolKind::Any);
                    });
                }
                Stmt::Def(DefP { name, .. }) => {
                    add(self, &mut result, name, ExportedSymbolKind::Function);
                }
                _ => {}
            }
        }
        result.into_values().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::slice_vec_ext::SliceExt;
    use crate::syntax::Dialect;

    fn module(x: &str) -> AstModule {
        AstModule::parse("X", x.to_owned(), &Dialect::Extended).unwrap()
    }

    #[test]
    fn test_lint_exported() {
        let modu = module(
            r#"
load("test", "a")
def b(): pass
d = 1
def _e(): pass
d = 2
"#,
        );
        let res = modu.exported_symbols();
        assert_eq!(
            res.map(|symbol| format!("{} {}", symbol.span, symbol.name)),
            &["X:3:5-6 b", "X:4:1-2 d"]
        );
    }
}
