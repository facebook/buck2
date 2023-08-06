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

use std::collections::HashMap;

use crate::codemap::CodeMap;
use crate::codemap::LineCol;
use crate::docs::DocItem;
use crate::docs::DocParam;
use crate::lsp::docs::get_doc_item_for_def;
use crate::syntax::ast::AssignP;
use crate::syntax::ast::AstPayload;
use crate::syntax::ast::AstStmtP;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::ForP;
use crate::syntax::ast::ParameterP;
use crate::syntax::ast::StmtP;

#[derive(Debug, PartialEq)]
pub(crate) enum SymbolKind {
    Method,
    Variable,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Symbol {
    pub(crate) name: String,
    pub(crate) detail: Option<String>,
    pub(crate) kind: SymbolKind,
    pub(crate) doc: Option<DocItem>,
    pub(crate) param: Option<DocParam>,
}

/// Walk the AST recursively and discover symbols.
pub(crate) fn find_symbols_at_location<P: AstPayload>(
    codemap: &CodeMap,
    ast: &AstStmtP<P>,
    cursor_position: LineCol,
) -> HashMap<String, Symbol> {
    let mut symbols = HashMap::new();
    fn walk<P: AstPayload>(
        codemap: &CodeMap,
        ast: &AstStmtP<P>,
        cursor_position: LineCol,
        symbols: &mut HashMap<String, Symbol>,
    ) {
        match &ast.node {
            StmtP::Assign(AssignP { lhs, ty: _, rhs }) => lhs.visit_lvalue(|x| {
                symbols.entry(x.0.clone()).or_insert_with(|| Symbol {
                    name: x.0.clone(),
                    kind: (match rhs.node {
                        ExprP::Lambda(_) => SymbolKind::Method,
                        _ => SymbolKind::Variable,
                    }),
                    detail: None,
                    doc: None,
                    param: None,
                });
            }),
            StmtP::AssignModify(dest, _, source) => dest.visit_lvalue(|x| {
                symbols.entry(x.0.clone()).or_insert_with(|| Symbol {
                    name: x.0.clone(),
                    kind: (match source.node {
                        ExprP::Lambda(_) => SymbolKind::Method,
                        _ => SymbolKind::Variable,
                    }),
                    detail: None,
                    doc: None,
                    param: None,
                });
            }),
            StmtP::For(ForP { var, over: _, body }) => {
                var.visit_lvalue(|x| {
                    symbols.entry(x.0.clone()).or_insert_with(|| Symbol {
                        name: x.0.clone(),
                        kind: SymbolKind::Variable,
                        detail: None,
                        doc: None,
                        param: None,
                    });
                });
                walk(codemap, body, cursor_position, symbols);
            }
            StmtP::Def(def) => {
                // Peek into the function definition to find the docstring.
                let doc = get_doc_item_for_def(def);
                symbols.entry(def.name.0.clone()).or_insert_with(|| Symbol {
                    name: def.name.0.clone(),
                    kind: SymbolKind::Method,
                    detail: None,
                    doc: doc.clone().map(DocItem::Function),
                    param: None,
                });

                // Only recurse into method if the cursor is in it.
                if codemap
                    .resolve_span(def.body.span)
                    .contains(cursor_position)
                {
                    symbols.extend(def.params.iter().filter_map(|param| match &param.node {
                        ParameterP::Normal(p, _) | ParameterP::WithDefaultValue(p, _, _) => {
                            Some(
                                (
                                    p.0.clone(),
                                    Symbol {
                                        name: p.0.clone(),
                                        kind: SymbolKind::Variable,
                                        detail: None,
                                        doc: None,
                                        param: doc.as_ref().and_then(|doc| {
                                            doc.find_param_with_name(&p.0).cloned()
                                        }),
                                    },
                                ),
                            )
                        }
                        _ => None,
                    }));
                    walk(codemap, &def.body, cursor_position, symbols);
                }
            }
            StmtP::Load(load) => symbols.extend(load.args.iter().map(|(name, _)| {
                (
                    name.0.clone(),
                    Symbol {
                        name: name.0.clone(),
                        detail: Some(format!("Loaded from {}", load.module.node)),
                        // TODO: This should be dynamic based on the actual loaded value.
                        kind: SymbolKind::Method,
                        // TODO: Pull from the original file.
                        doc: None,
                        param: None,
                    },
                )
            })),
            stmt => stmt.visit_stmt(|x| walk(codemap, x, cursor_position, symbols)),
        }
    }

    walk(codemap, ast, cursor_position, &mut symbols);
    symbols
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::find_symbols_at_location;
    use super::Symbol;
    use super::SymbolKind;
    use crate::codemap::LineCol;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

    #[test]
    fn global_symbols() {
        let ast_module = AstModule::parse(
            "t.star",
            r#"load("foo.star", "exported_a", renamed = "exported_b")

def method(param):
    pass

my_var = True
        "#
            .to_owned(),
            &Dialect::Standard,
        )
        .unwrap();

        assert_eq!(
            find_symbols_at_location(
                &ast_module.codemap,
                &ast_module.statement,
                LineCol { line: 6, column: 0 },
            ),
            HashMap::from([
                (
                    "exported_a".to_owned(),
                    Symbol {
                        name: "exported_a".to_owned(),
                        detail: Some("Loaded from foo.star".to_owned()),
                        kind: SymbolKind::Method,
                        doc: None,
                        param: None,
                    },
                ),
                (
                    "renamed".to_owned(),
                    Symbol {
                        name: "renamed".to_owned(),
                        detail: Some("Loaded from foo.star".to_owned()),
                        kind: SymbolKind::Method,
                        doc: None,
                        param: None,
                    },
                ),
                (
                    "method".to_owned(),
                    Symbol {
                        name: "method".to_owned(),
                        detail: None,
                        kind: SymbolKind::Method,
                        doc: None,
                        param: None,
                    },
                ),
                (
                    "my_var".to_owned(),
                    Symbol {
                        name: "my_var".to_owned(),
                        detail: None,
                        kind: SymbolKind::Variable,
                        doc: None,
                        param: None,
                    },
                ),
            ])
        );
    }

    #[test]
    fn inside_method() {
        let ast_module = AstModule::parse(
            "t.star",
            r#"load("foo.star", "exported_a", renamed = "exported_b")

def method(param):
    pass

my_var = True
        "#
            .to_owned(),
            &Dialect::Standard,
        )
        .unwrap();

        assert_eq!(
            find_symbols_at_location(
                &ast_module.codemap,
                &ast_module.statement,
                LineCol { line: 3, column: 4 },
            ),
            HashMap::from([
                (
                    "exported_a".to_owned(),
                    Symbol {
                        name: "exported_a".to_owned(),
                        detail: Some("Loaded from foo.star".to_owned()),
                        kind: SymbolKind::Method,
                        doc: None,
                        param: None,
                    },
                ),
                (
                    "renamed".to_owned(),
                    Symbol {
                        name: "renamed".to_owned(),
                        detail: Some("Loaded from foo.star".to_owned()),
                        kind: SymbolKind::Method,
                        doc: None,
                        param: None,
                    },
                ),
                (
                    "method".to_owned(),
                    Symbol {
                        name: "method".to_owned(),
                        detail: None,
                        kind: SymbolKind::Method,
                        doc: None,
                        param: None,
                    },
                ),
                (
                    "param".to_owned(),
                    Symbol {
                        name: "param".to_owned(),
                        detail: None,
                        kind: SymbolKind::Variable,
                        doc: None,
                        param: None,
                    }
                ),
                (
                    "my_var".to_owned(),
                    Symbol {
                        name: "my_var".to_owned(),
                        detail: None,
                        kind: SymbolKind::Variable,
                        doc: None,
                        param: None,
                    },
                ),
            ])
        );
    }
}
