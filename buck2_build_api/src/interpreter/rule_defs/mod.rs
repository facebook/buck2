/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use fancy_regex::Regex;
use starlark::{
    environment::GlobalsBuilder,
    values::{dict::DictRef, none::NoneType},
};
use tracing::warn;

use crate::interpreter::rule_defs::provider::register_builtin_providers;

pub mod artifact;
pub mod artifact_tagging;
pub mod attr;
pub mod cell_root;
pub mod cmd_args;
pub mod context;
pub mod label;
pub mod label_relative_path;
pub mod provider;
pub mod rule;
pub mod target_label;
pub mod transition;
pub mod transitive_set;
pub mod util;

#[starlark_module]
fn extra_functions(builder: &mut GlobalsBuilder) {
    // Load symbols into the module. Should only be used by infra_macros/DEFS.bzl
    fn load_symbols(symbols: DictRef) -> anyhow::Result<NoneType> {
        for (k, v) in symbols.iter() {
            eval.set_module_variable_at_some_point(&k.to_str(), v)?;
        }
        Ok(NoneType)
    }

    // Test if a regex matches
    fn regex_match(regex: &str, str: &str) -> anyhow::Result<bool> {
        let re = Regex::new(regex)?;
        Ok(re.is_match(str)?)
    }

    /// Produce a warning.
    fn warning(#[starlark(require = pos)] x: &str) -> anyhow::Result<NoneType> {
        warn!("{}", x);
        Ok(NoneType)
    }
}

pub fn register_rule_defs(globals: &mut GlobalsBuilder) {
    globals.struct_("attr", attr::register_attr_module);
    rule::register_rule_function(globals);
    cmd_args::register_args_function(globals);
    register_builtin_providers(globals);
    extra_functions(globals);
}

#[cfg(test)]
mod tests {
    use crate::interpreter::testing::{import, Tester};

    #[test]
    fn test_load_symbols() -> anyhow::Result<()> {
        let mut t = Tester::new()?;
        let defines = import("root", "pkg", "test.bzl");
        t.add_import(
            &defines,
            r#"
y = 2
load_symbols({'x': 1, 'z': 3})
"#,
        )?;
        t.run_starlark_test(
            r#"
load("@root//pkg:test.bzl", "x", "y", "z")
def test():
    assert_eq(x + y + z, 6)"#,
        )?;
        Ok(())
    }

    #[test]
    fn test_regex() -> anyhow::Result<()> {
        let mut t = Tester::new()?;
        t.run_starlark_test(
            r#"
def test():
    assert_eq(regex_match("abc|def|ghi", "abc"), True)
    assert_eq(regex_match("abc|def|ghi", "xyz"), False)
    assert_eq(regex_match("^((?!abc).)*$", "abc"), False)
    assert_eq(regex_match("^((?!abc).)*$", "xyz"), True)
"#,
        )?;
        Ok(())
    }
}
