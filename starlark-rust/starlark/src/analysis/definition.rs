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

use crate::{
    analysis::bind::{scope, Assigner, Bind, Scope},
    codemap::{Pos, ResolvedSpan, Span},
    syntax::AstModule,
};

/// The location of a definition for a given symbol. See [`AstModule::find_definition`].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DefinitionLocation {
    /// The definition was found at this location in the current file.
    Location(ResolvedSpan),
    /// The symbol was loaded from another file. "location" is the position within the
    /// "load()" statement, but additionally, the path in that load statement, and the
    /// name of the symbol within that file are provided so that additional lookups can
    /// happen to find the original definition.
    LoadedLocation {
        location: ResolvedSpan,
        path: String,
        name: String,
    },
    /// Either the provided location was not an access of a variable, or no definition
    /// could be found.
    NotFound,
}

impl AstModule {
    /// Attempts to find the location where a symbol is defined in the module, or `None` if it
    /// was not defined anywhere in this file.
    ///
    /// `line` and `col` are zero based indexes of a location of the symbol to attempt to lookup.
    ///
    /// This method also handles scoping properly (i.e. an access of "foo" in a function
    /// will return location of the parameter "foo", even if there is a global called "foo").
    pub(crate) fn find_definition(&self, line: u32, col: u32) -> DefinitionLocation {
        // The inner structure here lets us just hold references, and has some un-resolved
        // spans and the like. We turn it into a more proper structure at the end.
        enum Definition<'a> {
            // The location of the definition of the symbol at the current line/column
            Location(Span),
            LoadedLocation {
                location: Span,
                path: &'a str,
                name: &'a str,
            },
            // The definition was not found in the current scope but the name of an identifier
            // was found at the given position. This should be checked in outer scopes
            // to attempt to find the definition.
            Name(&'a str),
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
                        if g.span.begin() <= pos && pos <= g.span.end() {
                            let res = match scope.bound.get(g.node.as_str()) {
                                Some((Assigner::Load { path, name }, span)) => {
                                    Definition::LoadedLocation {
                                        location: *span,
                                        path,
                                        name: name.as_str(),
                                    }
                                }
                                Some((_, span)) => Definition::Location(*span),
                                // We know the symbol name, but it might only be available in
                                // an outer scope.
                                None => Definition::Name(g.node.as_str()),
                            };
                            return res;
                        }
                    }
                    Bind::Flow => {}
                    Bind::Scope(inner_scope) => match find_definition_in_scope(inner_scope, pos) {
                        x @ Definition::Location(_) | x @ Definition::LoadedLocation { .. } => {
                            return x;
                        }
                        Definition::Name(missing_symbol) => {
                            return match scope.bound.get(missing_symbol) {
                                None => Definition::Name(missing_symbol),
                                Some((Assigner::Load { path, name }, span)) => {
                                    Definition::LoadedLocation {
                                        location: *span,
                                        path,
                                        name: name.as_str(),
                                    }
                                }
                                Some((_, span)) => Definition::Location(*span),
                            };
                        }
                        Definition::NotFound => {}
                    },
                }
            }
            Definition::NotFound
        }

        let scope = scope(self);
        let line_span = self.codemap.line_span(line as usize);
        let current_pos = std::cmp::min(line_span.begin() + col, line_span.end());

        match find_definition_in_scope(&scope, current_pos) {
            Definition::Location(span) => {
                DefinitionLocation::Location(self.codemap.resolve_span(span))
            }
            Definition::Name(missing_symbol) => match scope.bound.get(missing_symbol) {
                None => DefinitionLocation::NotFound,
                Some((Assigner::Load { path, name }, span)) => DefinitionLocation::LoadedLocation {
                    location: self.codemap.resolve_span(*span),
                    path: path.node.clone(),
                    name: name.node.clone(),
                },
                Some((_, span)) => DefinitionLocation::Location(self.codemap.resolve_span(*span)),
            },
            Definition::NotFound => DefinitionLocation::NotFound,
            Definition::LoadedLocation {
                location,
                path,
                name,
            } => DefinitionLocation::LoadedLocation {
                location: self.codemap.resolve_span(location),
                path: path.to_owned(),
                name: name.to_owned(),
            },
        }
    }

    /// Attempt to find the location where an exported symbol is defined.
    #[allow(dead_code)]
    pub(crate) fn find_exported_symbol(&self, name: &str) -> Option<ResolvedSpan> {
        self.exported_symbols().iter().find_map(|(span, symbol)| {
            if *symbol == name {
                Some(span.resolve_span())
            } else {
                None
            }
        })
    }
}

#[cfg(test)]
mod helpers {
    use std::collections::{hash_map::Entry, HashMap};

    use textwrap::dedent;

    use crate::{
        codemap::{CodeMap, Pos, ResolvedSpan, Span},
        syntax::{AstModule, Dialect},
    };

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

        pub(crate) fn module(&self) -> anyhow::Result<AstModule> {
            AstModule::parse(&self.filename, self.program.clone(), &Dialect::Extended)
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

        <p1>p</p1>r<p2>i</p2>n<p3>t</p3>(x)
        add(3)
        invalid_symbol()
        "#,
        )
        .trim()
        .to_owned();
        let parsed = FixtureWithRanges::from_fixture("foo.star", &contents)?;
        let module = parsed.module()?;

        let expected = DefinitionLocation::LoadedLocation {
            location: parsed.span("print"),
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
        <a1>a</a1><a2>d</a2><a3>d</a3>(3)
        <i1>i</i1>nv<i2>a</i2>lid_symbo<i3>l</i3>()
        "#,
        )
        .trim()
        .to_owned();
        let parsed = FixtureWithRanges::from_fixture("foo.star", &contents)?;
        let module = parsed.module()?;

        let expected_add = DefinitionLocation::Location(parsed.span("add"));
        let expected_invalid = DefinitionLocation::Location(parsed.span("invalid_symbol"));
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
            DefinitionLocation::Location(parsed.span("x")),
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
            DefinitionLocation::Location(parsed.span("x")),
            module.find_definition(parsed.begin_line("x_var"), parsed.begin_column("x_var"))
        );
        assert_eq!(
            DefinitionLocation::Location(parsed.span("y2")),
            module.find_definition(parsed.begin_line("y_var1"), parsed.begin_column("y_var1"))
        );

        assert_eq!(
            DefinitionLocation::Location(parsed.span("y1")),
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
}
