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

//! Find which symbols are in scope at a particular point.

use starlark_map::small_map::SmallMap;

use crate::analysis::exported::SymbolKind;
use crate::codemap::CodeMap;
use crate::codemap::LineCol;
use crate::syntax::ast::AstAssignIdent;
use crate::syntax::ast::AstStmt;
use crate::syntax::ast::StmtP;
use crate::syntax::AstModule;

#[derive(Debug, PartialEq)]
pub(crate) struct SymbolWithDetails<'a> {
    /// The symbol itself.
    pub(crate) name: &'a str,
    /// The type of symbol it represents.
    pub(crate) kind: SymbolKind,
    /// The file where the symbol was loaded, if it was loaded.
    pub(crate) loaded_from: Option<&'a str>,
}

/// Find the symbols that are available in scope at a particular point that might be interesting
/// for autocomplete.
///
/// * Currently does not look into variables bound in list/dict comprehensions (should be fixed one day).
/// * Does not return local variables that start with an underscore (since they )
#[allow(dead_code)] // Used shortly by the LSP
pub(crate) fn find_symbols_at_position<'a>(
    module: &'a AstModule,
    position: LineCol,
) -> Vec<SymbolWithDetails<'a>> {
    fn walk<'a>(
        codemap: &CodeMap,
        position: LineCol,
        stmt: &'a AstStmt,
        top_level: bool,
        symbols: &mut SmallMap<&'a str, SymbolWithDetails<'a>>,
    ) {
        fn add<'a>(
            symbols: &mut SmallMap<&'a str, SymbolWithDetails<'a>>,
            top_level: bool,
            name: &'a AstAssignIdent,
            kind: SymbolKind,
            loaded_from: Option<&'a str>,
        ) {
            // Local variables which start with an underscore are ignored by convention,
            // so don't consider them to be variables we are interested in reporting.
            // They are more sinks to ignore warnings than variables.
            if top_level || !name.0.starts_with('_') {
                symbols.entry(&name.0).or_insert(SymbolWithDetails {
                    name: &name.0,
                    kind,
                    loaded_from,
                });
            }
        }

        match &**stmt {
            StmtP::Assign(dest, rhs) => dest
                .visit_lvalue(|x| add(symbols, top_level, x, SymbolKind::from_expr(&rhs.1), None)),
            StmtP::AssignModify(dest, _, _) => {
                dest.visit_lvalue(|x| add(symbols, top_level, x, SymbolKind::Any, None))
            }
            StmtP::For(dest, over_body) => {
                let (_, body) = &**over_body;
                dest.visit_lvalue(|x| add(symbols, top_level, x, SymbolKind::Any, None));
                walk(codemap, position, body, top_level, symbols);
            }
            StmtP::Def(def) => {
                add(symbols, top_level, &def.name, SymbolKind::Function, None);

                // Only recurse into method if the cursor is in it.
                if codemap.resolve_span(def.body.span).contains(position) {
                    for param in &def.params {
                        if let Some(name) = param.split().0 {
                            add(symbols, false, name, SymbolKind::Any, None)
                        }
                    }
                    walk(codemap, position, &def.body, false, symbols);
                }
            }
            StmtP::Load(load) => {
                for (name, _) in &load.args {
                    add(
                        symbols,
                        top_level,
                        name,
                        SymbolKind::Any,
                        Some(&**load.module),
                    )
                }
            }
            stmt => stmt.visit_stmt(|x| walk(codemap, position, x, top_level, symbols)),
        }
    }

    let mut symbols = SmallMap::new();
    walk(
        &module.codemap,
        position,
        &module.statement,
        true,
        &mut symbols,
    );
    symbols.into_values().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::Dialect;

    #[test]
    fn test_symbols_at_location() {
        let modu = AstModule::parse(
            "t.star",
            r#"
load("foo.star", "exported_a", renamed = "exported_b")
def _method(param):
    pass
    _local = 7
    x = lambda _: 7
my_var = True
        "#
            .to_owned(),
            &Dialect::Standard,
        )
        .unwrap();

        let at_root = find_symbols_at_position(&modu, LineCol { line: 0, column: 0 });
        let mut inside_method = find_symbols_at_position(&modu, LineCol { line: 3, column: 6 });
        assert_eq!(inside_method.len(), 6);
        inside_method.retain(|x| !at_root.contains(x));

        let sym = |name, kind, loaded_from| SymbolWithDetails {
            name,
            kind,
            loaded_from,
        };

        assert_eq!(
            at_root,
            vec![
                sym("exported_a", SymbolKind::Any, Some("foo.star")),
                sym("renamed", SymbolKind::Any, Some("foo.star")),
                sym("_method", SymbolKind::Function, None),
                sym("my_var", SymbolKind::Any, None),
            ]
        );

        assert_eq!(
            inside_method,
            vec![
                sym("param", SymbolKind::Any, None),
                sym("x", SymbolKind::Function, None),
            ]
        );
    }
}
