/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::soft_error;
use buck2_interpreter_for_build::attrs::attrs_global::register_attrs;
use buck2_interpreter_for_build::rule::register_rule_function;
use fancy_regex::Regex;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::dict::DictRef;
use starlark::values::none::NoneType;
use thiserror::Error;
use tracing::warn;

use crate::interpreter::rule_defs::provider::registration::register_builtin_providers;

pub mod artifact;
pub mod artifact_tagging;
pub mod cmd_args;
pub mod command_executor_config;
pub mod context;
pub mod label_relative_path;
pub mod provider;
pub mod transition;
pub mod transitive_set;
pub mod util;

#[derive(Debug, Error)]
enum ExtraFunctionErrors {
    #[error("Error produced by Starlark: {category}: {message}\n{call_stack}")]
    StarlarkSoftError {
        category: String,
        message: String,
        call_stack: String,
    },
}

#[starlark_module]
fn extra_functions(builder: &mut GlobalsBuilder) {
    // Load symbols into the module. Should only be used by infra_macros/DEFS.bzl
    fn load_symbols<'v>(
        symbols: DictRef<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<NoneType> {
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

    /// Produce an error that will become a hard error at some point in the future, but
    /// for now is a warning which is logged to the server.
    /// In the open source version of Buck2 tihs function always results in an error.
    ///
    /// Called passing a stable key (must be `snake_case`, used for consistent reporting)
    /// and an arbitrary message (used for debugging). As an example:
    ///
    /// ```python
    /// soft_error("rule_is_too_long", "Length of property exceeds 100 characters in " + repr(ctx.label))
    /// ```
    fn soft_error(
        #[starlark(require = pos)] category: &str,
        #[starlark(require = pos)] message: String,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<NoneType> {
        soft_error!(
            category,
            ExtraFunctionErrors::StarlarkSoftError {
                category: category.to_owned(),
                message,
                call_stack: eval.call_stack().to_string()
            }
            .into()
        )?;
        Ok(NoneType)
    }
}

pub fn register_rule_defs(globals: &mut GlobalsBuilder) {
    register_attrs(globals);
    register_rule_function(globals);
    cmd_args::register_cmd_args(globals);
    register_builtin_providers(globals);
    extra_functions(globals);
}

#[cfg(test)]
mod tests {
    use buck2_core::bzl::ImportPath;
    use buck2_interpreter_for_build::interpreter::testing::Tester;

    use crate::interpreter::rule_defs::register_rule_defs;

    #[test]
    fn test_load_symbols() -> anyhow::Result<()> {
        let mut t = Tester::new()?;
        t.additional_globals(register_rule_defs);
        let defines = ImportPath::testing_new("root//pkg:test.bzl");
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
        t.additional_globals(register_rule_defs);
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
