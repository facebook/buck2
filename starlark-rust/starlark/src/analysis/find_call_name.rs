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

use crate::codemap::ResolvedSpan;
use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::syntax::ast::Argument;
use crate::syntax::ast::AstExpr;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::Expr;
use crate::syntax::AstModule;

impl AstModule {
    /// Find the location of a top level function call that has a kwarg "name", and a string value
    /// matching `name`.
    ///
    /// NOTE: If the AST is exposed in the future, this function may be removed and implemented
    ///       by specific programs instead.
    pub fn find_function_call_with_name(&self, name: &str) -> Option<ResolvedSpan> {
        let mut ret = None;

        fn visit_expr(ret: &mut Option<Span>, name: &str, node: &AstExpr) {
            if ret.is_some() {
                return;
            }

            match node {
                Spanned {
                    node: Expr::Call(identifier, arguments),
                    ..
                } => {
                    if let Expr::Identifier(_) = &identifier.node {
                        let found = arguments.iter().find_map(|argument| match &argument.node {
                            Argument::Named(
                                arg_name,
                                Spanned {
                                    node: Expr::Literal(AstLiteral::String(s)),
                                    ..
                                },
                            ) if arg_name.node == "name" && s.node == name => Some(identifier.span),
                            _ => None,
                        });
                        if found.is_some() {
                            *ret = found;
                        }
                    }
                }
                _ => node.visit_expr(|x| visit_expr(ret, name, x)),
            }
        }

        self.statement.visit_expr(|x| visit_expr(&mut ret, name, x));
        ret.map(|span| self.codemap.resolve_span(span))
    }
}

#[cfg(test)]
mod test {

    use crate::codemap::ResolvedSpan;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

    #[test]
    fn finds_function_calls_with_name_kwarg() -> anyhow::Result<()> {
        let contents = r#"
foo(name = "foo_name")
bar("bar_name")
baz(name = "baz_name")

def x(name = "foo_name"):
    pass
"#;

        let module = AstModule::parse("foo.star", contents.to_owned(), &Dialect::Extended).unwrap();

        assert_eq!(
            Some(ResolvedSpan {
                begin_line: 1,
                begin_column: 0,
                end_line: 1,
                end_column: 3
            }),
            module.find_function_call_with_name("foo_name")
        );
        assert_eq!(None, module.find_function_call_with_name("bar_name"));
        Ok(())
    }
}
