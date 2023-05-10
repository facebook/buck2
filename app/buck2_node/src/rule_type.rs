/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::bzl::ImportPath;
use dupe::Dupe;

/// The identifier used to find the implementation function for this rule. Should point at the output of `rule()`
#[derive(Debug, Clone, derive_more::Display, Eq, PartialEq, Hash, Allocative)]
#[display(fmt = "{}:{}", import_path, name)]
pub struct StarlarkRuleType {
    /// The cell, package, and file that contains the output of `rule()`
    pub import_path: ImportPath,
    /// The name of the symbol that is bound to the output of `rule()`, e.g. `cxx_binary`
    pub name: String,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    derive_more::Display,
    Eq,
    PartialEq,
    Hash,
    Allocative
)]
pub enum RuleType {
    Starlark(Arc<StarlarkRuleType>),
    #[display(fmt = "forward")]
    Forward,
}

impl RuleType {
    pub fn name(&self) -> &str {
        match self {
            RuleType::Starlark(rule_type) => rule_type.name.as_str(),
            RuleType::Forward => "forward",
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::bzl::ImportPath;

    use crate::rule_type::StarlarkRuleType;

    #[test]
    fn function_id_has_useful_string() {
        let import_path = ImportPath::testing_new("root//some/subdir:foo.bzl");
        let name = "foo_binary".to_owned();

        assert_eq!(
            "root//some/subdir/foo.bzl:foo_binary",
            &StarlarkRuleType { import_path, name }.to_string()
        );
    }
}
