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

use crate::analysis::bind::scope;
use crate::analysis::bind::Assigner;
use crate::analysis::bind::Bind;
use crate::analysis::bind::Scope;
use crate::codemap::CodeMap;
use crate::codemap::Pos;
use crate::codemap::ResolvedSpan;
use crate::codemap::Span;
use crate::codemap::Spanned;
use crate::syntax::ast::ArgumentP;
use crate::syntax::ast::AstLiteral;
use crate::syntax::ast::AstNoPayload;
use crate::syntax::ast::Expr;
use crate::syntax::ast::ExprP;
use crate::syntax::ast::Stmt;
use crate::syntax::uniplate::Visit;
use crate::syntax::AstModule;

/// The location of a definition for a given symbol. See [`AstModule::find_definition`].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DefinitionLocation {
    /// The definition was found at this location in the current file.
    Location {
        source: ResolvedSpan,
        destination: ResolvedSpan,
    },
    /// The symbol was loaded from another file. "destination" is the position within the
    /// "load()" statement, but additionally, the path in that load statement, and the
    /// name of the symbol within that file are provided so that additional lookups can
    /// happen to find the original definition.
    LoadedLocation {
        source: ResolvedSpan,
        destination: ResolvedSpan,
        path: String,
        name: String,
    },
    /// The symbol is the path component of a `load` statement. This is the raw string
    /// that is in the AST, and needs to be properly resolved to a path to be useful.
    LoadPath { source: ResolvedSpan, path: String },
    /// A literal string in the source code. This can be a standalone string, or part of e.g. a
    /// list, a dictionary, a return value, etc.
    StringLiteral {
        source: ResolvedSpan,
        literal: String,
    },
    /// Either the provided location was not an access of a variable, or no definition
    /// could be found.
    NotFound,
}

/// Container that holds an AST module and returns things like definition locations,
/// lists of symbols, etc.
pub(crate) struct LspModule {
    pub(crate) ast: AstModule,
}

impl LspModule {
    pub(crate) fn new(ast: AstModule) -> Self {
        Self { ast }
    }

    /// Attempts to find the location where a symbol is defined in the module, or `None` if it
    /// was not defined anywhere in this file.
    ///
    /// `line` and `col` are zero based indexes of a location of the symbol to attempt to lookup.
    ///
    /// This method also handles scoping properly (i.e. an access of "foo" in a function
    /// will return location of the parameter "foo", even if there is a global called "foo").
    pub(crate) fn find_definition(&self, line: u32, col: u32) -> DefinitionLocation {
        // TODO(nmj): This should probably just store references to all of the AST nodes
        //            when the LSPModule object is created, and then we can do a much faster
        //            lookup, especially in cases where a file has not been changed, so the
        //            LSPModule doesn't need to reparse anything.

        // The inner structure here lets us just hold references, and has some un-resolved
        // spans and the like. We turn it into a more proper structure at the end.
        enum Definition<'a> {
            // The location of the definition of the symbol at the current line/column
            Location {
                source: Span,
                destination: Span,
            },
            LoadedLocation {
                source: Span,
                destination: Span,
                path: &'a str,
                name: &'a str,
            },
            // The definition was not found in the current scope but the name of an identifier
            // was found at the given position. This should be checked in outer scopes
            // to attempt to find the definition.
            Name {
                source: Span,
                name: &'a str,
            },
            // None of the accesses matched the position that was provided.
            NotFound,
        }

        // Look at the given scope and child scopes to try to find the definition of the symbol
        // accessed at Pos.
        fn find_definition_in_scope<'a>(scope: &'a Scope, pos: Pos) -> Definition<'a> {
            for bind in &scope.inner {
                match bind {
                    Bind::Set(_, _) => {}
                    Bind::Get(g) => {
                        if g.span.contains(pos) {
                            let res = match scope.bound.get(g.node.as_str()) {
                                Some((Assigner::Load { path, name }, span)) => {
                                    Definition::LoadedLocation {
                                        source: g.span,
                                        destination: *span,
                                        path,
                                        name: name.as_str(),
                                    }
                                }
                                Some((_, span)) => Definition::Location {
                                    source: g.span,
                                    destination: *span,
                                },
                                // We know the symbol name, but it might only be available in
                                // an outer scope.
                                None => Definition::Name {
                                    source: g.span,
                                    name: g.node.as_str(),
                                },
                            };
                            return res;
                        }
                    }
                    Bind::Flow => {}
                    Bind::Scope(inner_scope) => match find_definition_in_scope(inner_scope, pos) {
                        x @ Definition::Location { .. } | x @ Definition::LoadedLocation { .. } => {
                            return x;
                        }
                        Definition::Name { source, name } => {
                            return match scope.bound.get(name) {
                                None => Definition::Name { source, name },
                                Some((Assigner::Load { path, name }, span)) => {
                                    Definition::LoadedLocation {
                                        source,
                                        destination: *span,
                                        path,
                                        name: name.as_str(),
                                    }
                                }
                                Some((_, span)) => Definition::Location {
                                    source,
                                    destination: *span,
                                },
                            };
                        }
                        Definition::NotFound => {}
                    },
                }
            }
            Definition::NotFound
        }

        let scope = scope(&self.ast);
        let line_span = self.ast.codemap.line_span(line as usize);
        let current_pos = std::cmp::min(line_span.begin() + col, line_span.end());

        match find_definition_in_scope(&scope, current_pos) {
            Definition::Location {
                source,
                destination,
            } => DefinitionLocation::Location {
                source: self.ast.codemap.resolve_span(source),
                destination: self.ast.codemap.resolve_span(destination),
            },
            Definition::Name { source, name } => match scope.bound.get(name) {
                None => DefinitionLocation::NotFound,
                Some((Assigner::Load { path, name }, span)) => DefinitionLocation::LoadedLocation {
                    source: self.ast.codemap.resolve_span(source),
                    destination: self.ast.codemap.resolve_span(*span),
                    path: path.node.clone(),
                    name: name.node.clone(),
                },
                Some((_, span)) => DefinitionLocation::Location {
                    source: self.ast.codemap.resolve_span(source),
                    destination: self.ast.codemap.resolve_span(*span),
                },
            },
            // If we could not find the symbol, see if the current position is within
            // a load statement (these are not exposed as Get/Set in bind).
            Definition::NotFound => self.find_definition_from_ast(current_pos),
            Definition::LoadedLocation {
                source,
                destination,
                path,
                name,
            } => DefinitionLocation::LoadedLocation {
                source: self.ast.codemap.resolve_span(source),
                destination: self.ast.codemap.resolve_span(destination),
                path: path.to_owned(),
                name: name.to_owned(),
            },
        }
    }

    /// Attempt to find the location where an exported symbol is defined.
    pub(crate) fn find_exported_symbol(&self, name: &str) -> Option<ResolvedSpan> {
        self.ast
            .exported_symbols()
            .iter()
            .find_map(|(span, symbol)| {
                if *symbol == name {
                    Some(span.resolve_span())
                } else {
                    None
                }
            })
    }

    fn find_definition_from_ast(&self, pos: Pos) -> DefinitionLocation {
        fn visit_node(
            codemap: &CodeMap,
            pos: Pos,
            ret: &mut Option<DefinitionLocation>,
            node: Visit<AstNoPayload>,
        ) {
            if ret.is_some() {
                return;
            }
            match node {
                Visit::Expr(Spanned {
                    node: Expr::Literal(AstLiteral::String(s)),
                    ..
                }) if s.span.contains(pos) => {
                    *ret = Some(DefinitionLocation::StringLiteral {
                        source: codemap.resolve_span(s.span),
                        literal: s.node.to_owned(),
                    });
                }
                Visit::Stmt(Spanned {
                    node: Stmt::Load(load),
                    ..
                }) => {
                    *ret = if load.module.span.contains(pos) {
                        Some(DefinitionLocation::LoadPath {
                            source: codemap.resolve_span(load.module.span),
                            path: load.module.node.to_owned(),
                        })
                    } else {
                        load.args.iter().find_map(|(assign, name)| {
                            if assign.span.contains(pos) || name.span.contains(pos) {
                                Some(DefinitionLocation::LoadedLocation {
                                    source: codemap.resolve_span(name.span),
                                    destination: codemap.resolve_span(name.span),
                                    path: load.module.node.to_owned(),
                                    name: name.node.to_owned(),
                                })
                            } else {
                                None
                            }
                        })
                    }
                }
                v => v.visit_children(|node| visit_node(codemap, pos, ret, node)),
            }
        }

        let mut ret = None;
        visit_node(
            &self.ast.codemap,
            pos,
            &mut ret,
            Visit::Stmt(&self.ast.statement),
        );
        ret.unwrap_or(DefinitionLocation::NotFound)
    }
}

impl AstModule {
    /// Find the location of a top level function call that has a kwarg "name", and a string value
    /// matching `name`.
    ///
    /// NOTE: If the AST is exposed in the future, this function may be removed and implemented
    ///       by specific programs instead.
    pub fn find_function_call_with_name(&self, name: &str) -> Option<ResolvedSpan> {
        let mut ret = None;

        fn visit_node(ret: &mut Option<Span>, name: &str, node: Visit<AstNoPayload>) {
            if ret.is_some() {
                return;
            }

            match node {
                Visit::Expr(Spanned {
                    node: ExprP::Call(identifier, arguments),
                    ..
                }) => {
                    if let ExprP::Identifier(_, _) = &identifier.node {
                        let found = arguments.iter().find_map(|argument| match &argument.node {
                            ArgumentP::Named(
                                arg_name,
                                Spanned {
                                    node: ExprP::Literal(AstLiteral::String(s)),
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
                Visit::Stmt(s) => s.visit_children(|node| visit_node(ret, name, node)),
                _ => {}
            }
        }

        visit_node(&mut ret, name, Visit::Stmt(&self.statement));
        ret.map(|span| self.codemap.resolve_span(span))
    }
}

#[cfg(test)]
pub(crate) mod helpers {
    use std::collections::hash_map::Entry;
    use std::collections::HashMap;

    use textwrap::dedent;

    use crate::analysis::LspModule;
    use crate::codemap::CodeMap;
    use crate::codemap::Pos;
    use crate::codemap::ResolvedSpan;
    use crate::codemap::Span;
    use crate::syntax::AstModule;
    use crate::syntax::Dialect;

    /// Result of parsing a starlark fixture that has range markers in it. See `FixtureWithRanges::from_fixture`
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub(crate) struct FixtureWithRanges {
        filename: String,
        /// The starlark program with markers removed.
        program: String,
        /// The location of all of the symbols that were indicated by the test fixture.
        ranges: HashMap<String, ResolvedSpan>,
    }

    impl FixtureWithRanges {
        /// Parse a program that has markers in it indicating a "range" by name.
        ///
        /// Markers are simple `<\w+>` and `</\w+>` tags. The returned program is the
        /// program content with these markers removed.
        ///
        /// For example:
        /// ```#starlark
        /// load("foo.star", <a>"bar"</a>);
        ///
        /// x = 1
        /// <bar_highlight><bar_click>b</bar_click>ar</bar_highlight>(<x>x</x>)
        /// ```
        ///
        /// Would return a program body of:
        /// ```#starlark
        /// load("foo.star", "bar");
        ///
        /// x = 1
        /// bar(x)
        /// ```
        ///
        /// And ranges of:
        ///  `a`: 0:17-0:22
        ///  `bar_highlight`: 3:0-3:2
        ///  `bar_click`: 3:0-3:1
        ///  `x`: 3:4-3:5
        pub(crate) fn from_fixture(filename: &str, fixture: &str) -> anyhow::Result<Self> {
            let re = regex::Regex::new(r#"<(/)?(\w+)>"#).unwrap();
            let mut program = String::new();
            let mut ranges: HashMap<String, (Option<usize>, Option<usize>)> = HashMap::new();

            let mut fixture_idx = 0;
            for matches in re.captures_iter(fixture) {
                let full_tag = matches.get(0).unwrap();
                let is_end_tag = matches.get(1).is_some();
                let identifier = matches.get(2).unwrap().as_str().to_owned();

                program.push_str(&fixture[fixture_idx..full_tag.start()]);
                fixture_idx = full_tag.end();

                match (is_end_tag, ranges.entry(identifier.clone())) {
                    (false, Entry::Occupied(_)) => {
                        return Err(anyhow::anyhow!("duplicate open tag `{}` found", identifier));
                    }
                    (false, Entry::Vacant(e)) => {
                        e.insert((Some(program.len()), None));
                    }
                    (true, Entry::Occupied(mut e)) => {
                        e.insert((e.get().0, Some(program.len())));
                    }
                    (true, Entry::Vacant(_)) => {
                        return Err(anyhow::anyhow!(
                            "found closing tag for `{}`, but no open tag",
                            identifier
                        ));
                    }
                }
            }
            program.push_str(&fixture[fixture_idx..fixture.len()]);

            let code_map = CodeMap::new(filename.to_owned(), program.clone());
            let spans: HashMap<String, ResolvedSpan> = ranges
                .into_iter()
                .map(|(id, (start, end))| {
                    let span = Span::new(
                        Pos::new(start.unwrap() as u32),
                        Pos::new(end.unwrap() as u32),
                    );
                    (id, code_map.resolve_span(span))
                })
                .collect();

            Ok(Self {
                filename: filename.to_owned(),
                program,
                ranges: spans,
            })
        }

        pub(crate) fn begin_line(&self, identifier: &str) -> u32 {
            self.ranges
                .get(identifier)
                .expect("identifier to be present")
                .begin_line as u32
        }

        pub(crate) fn begin_column(&self, identifier: &str) -> u32 {
            self.ranges
                .get(identifier)
                .expect("identifier to be present")
                .begin_column as u32
        }

        pub(crate) fn span(&self, identifier: &str) -> ResolvedSpan {
            *self
                .ranges
                .get(identifier)
                .expect("identifier to be present")
        }

        pub(crate) fn module(&self) -> anyhow::Result<LspModule> {
            Ok(LspModule::new(AstModule::parse(
                &self.filename,
                self.program.clone(),
                &Dialect::Extended,
            )?))
        }

        #[cfg(not(windows))]
        pub(crate) fn program(&self) -> String {
            self.program.clone()
        }
    }

    #[test]
    fn parses_fixtures() -> anyhow::Result<()> {
        let fixture = dedent(
            r#"
            load("foo.star", <a>"bar"</a>);

            x = 1
            <bar_highlight><bar_click>b</bar_click>ar</bar_highlight>(<x>x</x>)
            "#,
        )
        .trim()
        .to_owned();

        let expected_program = dedent(
            r#"
            load("foo.star", "bar");

            x = 1
            bar(x)
            "#,
        )
        .trim()
        .to_owned();

        let expected_locations = [
            ("a", 0, 17, 0, 22),
            ("bar_highlight", 3, 0, 3, 3),
            ("bar_click", 3, 0, 3, 1),
            ("x", 3, 4, 3, 5),
        ]
        .into_iter()
        .map(|(id, begin_line, begin_column, end_line, end_column)| {
            let span = ResolvedSpan {
                begin_line,
                begin_column,
                end_line,
                end_column,
            };
            (id.to_owned(), span)
        })
        .collect();

        let expected = FixtureWithRanges {
            filename: "test.star".to_owned(),
            program: expected_program,
            ranges: expected_locations,
        };

        let parsed = FixtureWithRanges::from_fixture("test.star", &fixture)?;

        assert_eq!(expected, parsed);

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use textwrap::dedent;

    use super::helpers::FixtureWithRanges;
    use crate::analysis::DefinitionLocation;
    use crate::analysis::LspModule;

    #[test]
    fn find_definition_loaded_symbol() -> anyhow::Result<()> {
        let contents = dedent(
            r#"
        load("bar.star", <print>print</print> = "other_print");

        x = 1
        y = 2

        def add(y: "int"):
            return x + y

        def invalid_symbol():
            return y + z

        <print_click><p1>p</p1>r<p2>i</p2>n<p3>t</p3></print_click>(x)
        add(3)
        invalid_symbol()
        "#,
        )
        .trim()
        .to_owned();
        let parsed = FixtureWithRanges::from_fixture("foo.star", &contents)?;
        let module = parsed.module()?;

        let expected = DefinitionLocation::LoadedLocation {
            source: parsed.span("print_click"),
            destination: parsed.span("print"),
            path: "bar.star".to_owned(),
            name: "other_print".to_owned(),
        };
        assert_eq!(
            expected,
            module.find_definition(parsed.begin_line("p1"), parsed.begin_column("p1"))
        );
        assert_eq!(
            expected,
            module.find_definition(parsed.begin_line("p2"), parsed.begin_column("p2"))
        );
        assert_eq!(
            expected,
            module.find_definition(parsed.begin_line("p3"), parsed.begin_column("p3"))
        );
        Ok(())
    }

    #[test]
    fn find_definition_function_calls() -> anyhow::Result<()> {
        let contents = dedent(
            r#"
        load("bar.star", "print");

        x = 1
        y = 2

        def <add>add</add>(y: "int"):
            return x + y

        def <invalid_symbol>invalid_symbol</invalid_symbol>():
            return y + z

        print(x)
        <add_click><a1>a</a1><a2>d</a2><a3>d</a3></add_click>(3)
        <invalid_symbol_click><i1>i</i1>nv<i2>a</i2>lid_symbo<i3>l</i3></invalid_symbol_click>()
        "#,
        )
        .trim()
        .to_owned();
        let parsed = FixtureWithRanges::from_fixture("foo.star", &contents)?;
        let module = parsed.module()?;

        let expected_add = DefinitionLocation::Location {
            source: parsed.span("add_click"),
            destination: parsed.span("add"),
        };
        let expected_invalid = DefinitionLocation::Location {
            source: parsed.span("invalid_symbol_click"),
            destination: parsed.span("invalid_symbol"),
        };
        assert_eq!(
            expected_add,
            module.find_definition(parsed.begin_line("a1"), parsed.begin_column("a1"))
        );
        assert_eq!(
            expected_add,
            module.find_definition(parsed.begin_line("a2"), parsed.begin_column("a2"))
        );
        assert_eq!(
            expected_add,
            module.find_definition(parsed.begin_line("a3"), parsed.begin_column("a3"))
        );

        assert_eq!(
            expected_invalid,
            module.find_definition(parsed.begin_line("i1"), parsed.begin_column("i1"))
        );
        assert_eq!(
            expected_invalid,
            module.find_definition(parsed.begin_line("i2"), parsed.begin_column("i2"))
        );
        assert_eq!(
            expected_invalid,
            module.find_definition(parsed.begin_line("i3"), parsed.begin_column("i3"))
        );
        Ok(())
    }

    #[test]
    fn find_definition_function_params() -> anyhow::Result<()> {
        let contents = dedent(
            r#"
        load("bar.star", "print");

        <x>x</x> = 1
        y = 2

        def add(y: "int"):
            return x + y

        def invalid_symbol():
            return y + z

        print(<x_param>x</x_param>)
        add(3)
        invalid_symbol()
        "#,
        )
        .trim()
        .to_owned();
        let parsed = FixtureWithRanges::from_fixture("foo.star", &contents)?;
        let module = parsed.module()?;

        assert_eq!(
            DefinitionLocation::Location {
                source: parsed.span("x_param"),
                destination: parsed.span("x")
            },
            module.find_definition(parsed.begin_line("x_param"), parsed.begin_column("x_param"))
        );
        Ok(())
    }

    #[test]
    fn find_definition_scopes_locals() -> anyhow::Result<()> {
        let contents = dedent(
            r#"
        load("bar.star", "print");

        <x>x</x> = 1
        <y1>y</y1> = 2

        def add(<y2>y</y2>: "int"):
            return <x_var>x</x_var> + <y_var1>y</y_var1>

        def invalid_symbol():
            return <y_var2>y</y_var2> + <z_var>z</z_var>

        print(x)
        add(3)
        invalid_symbol()
        "#,
        )
        .trim()
        .to_owned();
        let parsed = FixtureWithRanges::from_fixture("foo.star", &contents)?;
        let module = parsed.module()?;

        assert_eq!(
            DefinitionLocation::Location {
                source: parsed.span("x_var"),
                destination: parsed.span("x")
            },
            module.find_definition(parsed.begin_line("x_var"), parsed.begin_column("x_var"))
        );
        assert_eq!(
            DefinitionLocation::Location {
                source: parsed.span("y_var1"),
                destination: parsed.span("y2")
            },
            module.find_definition(parsed.begin_line("y_var1"), parsed.begin_column("y_var1"))
        );

        assert_eq!(
            DefinitionLocation::Location {
                source: parsed.span("y_var2"),
                destination: parsed.span("y1")
            },
            module.find_definition(parsed.begin_line("y_var2"), parsed.begin_column("y_var2"))
        );
        assert_eq!(
            DefinitionLocation::NotFound,
            module.find_definition(parsed.begin_line("z_var"), parsed.begin_column("z_var"))
        );
        Ok(())
    }

    #[test]
    fn find_definition_unknown_clicks() -> anyhow::Result<()> {
        let contents = dedent(
            r#"
        load("bar.star", "print");

        x = 1
        y = 2

        def <no_def>add</no_def>(y: "int"):
            return x + y

        def invalid_symbol():
            return y + z

        print(x)
        add(3)
        invalid_symbol()
        "#,
        )
        .trim()
        .to_owned();
        let parsed = FixtureWithRanges::from_fixture("foo.star", &contents)?;
        let module = parsed.module()?;

        assert_eq!(
            DefinitionLocation::NotFound,
            module.find_definition(parsed.begin_line("no_def"), parsed.begin_column("no_def"))
        );

        Ok(())
    }

    #[test]
    fn finds_definition_in_strings() -> anyhow::Result<()> {
        let contents = dedent(
            r#"
        <foo1_click>"f<foo1>o</foo1>o1"</foo1_click>
        [
            <foo2_click>"f<foo2>o</foo2>o2"</foo2_click>,
            "ignored"
        ]
        {
            <foo3_click>"f<foo3>o</foo3>o3"</foo3_click>: "ignored",
            "ignored": <foo4_click>"f<foo4>o</foo4>o4"</foo4_click>,
            "ignored_other": "ignored",
        }

        def f(x = <foo5_click>"f<foo5>o</foo5>o5"</foo5_click>):
            <foo6_click>"f<foo6>o</foo6>o6"</foo6_click>
            [
                <foo7_click>"f<foo7>o</foo7>o7"</foo7_click>,
                "ignored"
            ]
            {
                <foo8_click>"f<foo8>o</foo8>o8"</foo8_click>: "ignored",
                "ignored": <foo9_click>"f<foo9>o</foo9>o9"</foo9_click>,
                "ignored_other": "ignored",
            }
            if x == <foo10_click>"f<foo10>o</foo10>o10"</foo10_click>:
                <foo11_click>"f<foo11>o</foo11>o11"</foo11_click>
                [
                    <foo12_click>"f<foo12>o</foo12>o12"</foo12_click>,
                    "ignored"
                ]
                {
                    <foo13_click>"f<foo13>o</foo13>o13"</foo13_click>: "ignored",
                    "ignored": <foo14_click>"f<foo14>o</foo14>o14"</foo14_click>,
                    "ignored_other": "ignored",
                }
            return <foo15_click>"f<foo15>o</foo15>o15"</foo15_click>

        foo16 = <foo16_click>"f<foo16>o</foo16>o16"</foo16_click>
        "#,
        )
        .trim()
        .to_owned();

        let parsed = FixtureWithRanges::from_fixture("foo.star", &contents)?;
        let module = parsed.module()?;

        fn test(parsed: &FixtureWithRanges, module: &LspModule, name: &str) {
            let expected = DefinitionLocation::StringLiteral {
                source: parsed.span(&format!("{}_click", name)),
                literal: name.to_owned(),
            };
            let actual = module.find_definition(parsed.begin_line(name), parsed.begin_column(name));

            assert_eq!(
                expected, actual,
                "Expected `{:?}` == `{:?}` for test `{}`",
                expected, actual, name
            );
        }

        test(&parsed, &module, "foo1");
        test(&parsed, &module, "foo2");
        test(&parsed, &module, "foo3");
        test(&parsed, &module, "foo4");
        test(&parsed, &module, "foo5");
        test(&parsed, &module, "foo6");
        test(&parsed, &module, "foo7");
        test(&parsed, &module, "foo8");
        test(&parsed, &module, "foo9");
        test(&parsed, &module, "foo10");
        test(&parsed, &module, "foo11");
        test(&parsed, &module, "foo12");
        test(&parsed, &module, "foo13");
        test(&parsed, &module, "foo14");
        test(&parsed, &module, "foo15");
        test(&parsed, &module, "foo16");

        Ok(())
    }

    #[test]
    fn finds_function_calls_with_name_kwarg() -> anyhow::Result<()> {
        let contents = dedent(
            r#"
            <foo>foo</foo>(name = "foo_name")
            bar("bar_name")
            baz(name = "baz_name")

            def x(name = "foo_name"):
                pass
            "#,
        )
        .trim()
        .to_owned();

        let parsed = FixtureWithRanges::from_fixture("foo.star", &contents)?;

        let module = parsed.module()?;
        assert_eq!(
            Some(parsed.span("foo")),
            module.ast.find_function_call_with_name("foo_name")
        );
        assert_eq!(None, module.ast.find_function_call_with_name("bar_name"));
        Ok(())
    }
}
