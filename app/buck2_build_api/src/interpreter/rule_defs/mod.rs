/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::functions::regex::register_regex;
use buck2_interpreter::functions::soft_error::register_soft_error;
use buck2_interpreter::functions::warning::register_warning;
use buck2_interpreter_for_build::attrs::attrs_global::register_attrs;
use buck2_interpreter_for_build::rule::register_rule_function;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::none::NoneType;
use starlark::values::Value;

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

#[starlark_module]
fn extra_functions(builder: &mut GlobalsBuilder) {
    /// Used in a `.bzl` file to set exported symbols. In most cases just defining
    /// the symbol as a top-level binding is sufficient, but sometimes the names
    /// might be programatically generated.
    ///
    /// It is undefined behaviour if you try and use any of the symbols exported
    /// here later in the same module, or if they overlap with existing definitions.
    /// This function should be used rarely.
    fn load_symbols<'v>(
        symbols: SmallMap<&'v str, Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<NoneType> {
        for (k, v) in symbols.into_iter() {
            eval.set_module_variable_at_some_point(k, v)?;
        }
        Ok(NoneType)
    }
}

pub fn register_rule_defs(globals: &mut GlobalsBuilder) {
    register_attrs(globals);
    register_rule_function(globals);
    cmd_args::register_cmd_args(globals);
    register_builtin_providers(globals);
    extra_functions(globals);
    register_regex(globals);
    register_warning(globals);
    register_soft_error(globals);
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
}
