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

use dupe::Dupe;
use starlark::syntax::AstModule;
use starlark_syntax::syntax::ast::StmtP;
use starlark_syntax::syntax::top_level_stmts::top_level_stmts;

/// A loaded symbol. Returned from [`AstModule::loaded_symbols`].
#[derive(Debug, PartialEq, Eq, Clone, Dupe, Hash)]
pub struct LoadedSymbol<'a> {
    /// The name of the symbol.
    pub name: &'a str,
    /// The file it's loaded from. Note that this is an unresolved path, so it
    /// might be a relative load.
    pub loaded_from: &'a str,
}

pub(crate) trait AstModuleLoadedSymbols {
    /// Which symbols are loaded by this module. These are the top-level load
    /// statements.
    fn loaded_symbols<'a>(&'a self) -> Vec<LoadedSymbol<'a>>;
}

impl AstModuleLoadedSymbols for AstModule {
    fn loaded_symbols(&self) -> Vec<LoadedSymbol<'_>> {
        top_level_stmts(self.statement())
            .into_iter()
            .filter_map(|x| match &x.node {
                StmtP::Load(l) => Some(l),
                _ => None,
            })
            .flat_map(|l| {
                l.args.iter().map(|symbol| LoadedSymbol {
                    name: &symbol.their,
                    loaded_from: &l.module,
                })
            })
            .collect()
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
    fn test_loaded() {
        let modu = module(
            r#"
load("test", "a", b = "c")
load("foo", "bar")
"#,
        );
        let res = modu.loaded_symbols();
        assert_eq!(
            res.map(|symbol| format!("{}:{}", symbol.loaded_from, symbol.name)),
            &["test:a", "test:c", "foo:bar"]
        );
    }
}
