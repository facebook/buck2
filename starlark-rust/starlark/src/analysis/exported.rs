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

use indexmap::IndexMap;

use crate::codemap::FileSpan;
use crate::syntax::ast::Stmt;
use crate::syntax::AstModule;

impl AstModule {
    /// Which symbols are exported by this module. These are the top-level assignments,
    /// including function definitions. Any symbols that start with `_` are not exported.
    pub fn exported_symbols(&self) -> Vec<(FileSpan, &str)> {
        // Map since we only want to store the first of each export
        // IndexMap since we want the order to match the order they were defined in
        let mut result: IndexMap<&str, _> = IndexMap::new();
        self.statement.visit_stmt(|x| match &**x {
            Stmt::Assign(dest, _) | Stmt::AssignModify(dest, _, _) => {
                dest.visit_lvalue(|name| {
                    result.entry(&name.0).or_insert(name.span);
                });
            }
            Stmt::Def(name, ..) => {
                result.entry(&name.0).or_insert(name.span);
            }
            _ => {}
        });
        result
            .into_iter()
            .filter(|(name, _)| !name.starts_with('_'))
            .map(|(name, span)| (self.file_span(span), name))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use gazebo::prelude::*;

    use super::*;
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
            res.map(|(loc, name)| format!("{} {}", loc, name)),
            &["X:3:5-6 b", "X:4:1-2 d"]
        );
    }
}
