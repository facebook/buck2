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

#[cfg(all(test, not(windows)))]
pub(crate) use definition::helpers::FixtureWithRanges;
pub(crate) use definition::DefinitionLocation;
pub use types::{EvalMessage, EvalSeverity, Lint};

use crate::{analysis::types::LintT, syntax::AstModule};

mod bind;
mod definition;
mod dubious;
mod exported;
mod flow;
mod incompatible;
mod names;
mod performance;
mod types;

impl AstModule {
    /// Run a static linter over the module. If the complete set of global variables are known
    /// they can be passed as the `globals` argument, resulting in name-resolution lint errors.
    /// The precise checks run by the linter are not considered stable between versions.
    pub fn lint(&self, globals: Option<&[&str]>) -> Vec<Lint> {
        let mut res = Vec::new();
        res.extend(flow::flow_issues(self).into_iter().map(LintT::erase));
        res.extend(
            incompatible::incompatibilities(self)
                .into_iter()
                .map(LintT::erase),
        );
        res.extend(dubious::dubious(self).into_iter().map(LintT::erase));
        res.extend(
            names::name_warnings(self, globals)
                .into_iter()
                .map(LintT::erase),
        );
        res.extend(performance::performance(self).into_iter().map(LintT::erase));
        res
    }
}
