/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::bzl::ImportPath;
use gazebo::dupe::Dupe;

/// The identifier used to find the implementation function for this rule. Should point at the output of `rule()`
#[derive(Debug, Clone, derive_more::Display, Eq, PartialEq, Hash)]
#[display(fmt = "{}:{}", "import_path.id()", name)]
pub struct StarlarkRuleType {
    /// The cell, package, and file that contains the output of `rule()`
    pub import_path: ImportPath,
    /// The name of the symbol that is bound to the output of `rule()`, e.g. `cxx_binary`
    pub name: String,
}

#[derive(Debug, Clone, Dupe, derive_more::Display, Eq, PartialEq, Hash)]
pub enum RuleType {
    #[display(fmt = "{}", _0)]
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
