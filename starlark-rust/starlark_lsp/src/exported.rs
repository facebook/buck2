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

use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use lsp_types::Documentation;
use lsp_types::MarkupContent;
use lsp_types::MarkupKind;
use starlark::codemap::FileSpan;
use starlark::collections::SmallMap;
use starlark::docs::DocItem;
use starlark::docs::DocMember;
use starlark::docs::markdown::render_doc_item_no_link;
use starlark::syntax::AstModule;
use starlark_syntax::syntax::ast::AstAssignIdent;
use starlark_syntax::syntax::ast::Expr;
use starlark_syntax::syntax::ast::Stmt;
use starlark_syntax::syntax::module::AstModuleFields;
use starlark_syntax::syntax::top_level_stmts::top_level_stmts;

use crate::docs::get_doc_item_for_assign;
use crate::docs::get_doc_item_for_def;

/// The type of an exported symbol.
/// If unknown, will use `Any`.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub(crate) enum SymbolKind {
    /// Any kind of symbol.
    Any,
    /// The symbol represents something that can be called, for example
    /// a `def` or a variable assigned to a `lambda`.
    Function { argument_names: Vec<String> },
}

impl SymbolKind {
    pub(crate) fn from_expr(x: &Expr) -> Self {
        match x {
            Expr::Lambda(lambda) => Self::Function {
                argument_names: lambda
                    .params
                    .iter()
                    .filter_map(|param| param.split().0.map(|name| name.to_string()))
                    .collect(),
            },
            _ => Self::Any,
        }
    }
}

impl From<SymbolKind> for CompletionItemKind {
    fn from(value: SymbolKind) -> Self {
        match value {
            SymbolKind::Any => CompletionItemKind::CONSTANT,
            SymbolKind::Function { .. } => CompletionItemKind::FUNCTION,
        }
    }
}

/// A symbol. Returned from [`AstModule::exported_symbols`].
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Symbol {
    /// The name of the symbol.
    pub(crate) name: String,
    /// The location of its definition.
    pub(crate) span: FileSpan,
    /// The type of symbol it represents.
    pub(crate) kind: SymbolKind,
    /// The documentation for this symbol.
    pub(crate) docs: Option<DocItem>,
}

impl From<Symbol> for CompletionItem {
    fn from(value: Symbol) -> Self {
        let documentation = value.docs.map(|docs| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: render_doc_item_no_link(&value.name, &docs),
            })
        });
        Self {
            label: value.name,
            kind: Some(value.kind.into()),
            documentation,
            ..Default::default()
        }
    }
}

pub(crate) trait AstModuleExportedSymbols {
    /// Which symbols are exported by this module. These are the top-level assignments,
    /// including function definitions. Any symbols that start with `_` are not exported.
    fn exported_symbols(&self) -> Vec<Symbol>;
}

impl AstModuleExportedSymbols for AstModule {
    fn exported_symbols(&self) -> Vec<Symbol> {
        // Map since we only want to store the first of each export
        // IndexMap since we want the order to match the order they were defined in
        let mut result: SmallMap<&str, _> = SmallMap::new();

        fn add<'a>(
            me: &AstModule,
            result: &mut SmallMap<&'a str, Symbol>,
            name: &'a AstAssignIdent,
            kind: SymbolKind,
            resolve_docs: impl FnOnce() -> Option<DocItem>,
        ) {
            if !name.ident.starts_with('_') {
                result.entry(&name.ident).or_insert(Symbol {
                    name: name.ident.clone(),
                    span: me.file_span(name.span),
                    kind,
                    docs: resolve_docs(),
                });
            }
        }

        let mut last_node = None;
        for x in top_level_stmts(self.statement()) {
            match &**x {
                Stmt::Assign(assign) => {
                    assign.lhs.visit_lvalue(|name| {
                        let kind = SymbolKind::from_expr(&assign.rhs);
                        add(self, &mut result, name, kind, || {
                            last_node
                                .and_then(|last| get_doc_item_for_assign(last, &assign.lhs))
                                .map(|x| DocItem::Member(DocMember::Property(x)))
                        });
                    });
                }
                Stmt::AssignModify(dest, _, _) => {
                    dest.visit_lvalue(|name| {
                        add(self, &mut result, name, SymbolKind::Any, || {
                            last_node
                                .and_then(|last| get_doc_item_for_assign(last, dest))
                                .map(|x| DocItem::Member(DocMember::Property(x)))
                        });
                    });
                }
                Stmt::Def(def) => {
                    add(
                        self,
                        &mut result,
                        &def.name,
                        SymbolKind::Function {
                            argument_names: def
                                .params
                                .iter()
                                .filter_map(|param| param.split().0.map(|name| name.to_string()))
                                .collect(),
                        },
                        || {
                            get_doc_item_for_def(def, self.codemap())
                                .map(|x| DocItem::Member(DocMember::Function(x)))
                        },
                    );
                }
                _ => {}
            }
            last_node = Some(x);
        }
        result.into_values().collect()
    }
}

#[cfg(test)]
mod tests {
    use starlark::syntax::Dialect;
    use starlark_syntax::slice_vec_ext::SliceExt;

    use super::*;

    fn module(x: &str) -> AstModule {
        AstModule::parse("X", x.to_owned(), &Dialect::AllOptionsInternal).unwrap()
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
