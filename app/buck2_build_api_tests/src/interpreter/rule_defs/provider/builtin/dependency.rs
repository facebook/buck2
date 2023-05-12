/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::provider::collection::ProviderCollection;
use buck2_build_api::interpreter::rule_defs::provider::dependency::Dependency;
use buck2_common::result::SharedResult;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_interpreter_for_build::interpreter::build_context::BuildContext;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::Value;

#[starlark_module]
fn dependency_creator(builder: &mut GlobalsBuilder) {
    fn create_collection<'v>(
        s: &str,
        providers: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Dependency<'v>> {
        let c = BuildContext::from_context(eval)?;
        let label = match ParsedPattern::<ProvidersPatternExtra>::parse_precise(
            s,
            c.cell_info().name().name(),
            c.cell_info().cell_resolver(),
        ) {
            Ok(ParsedPattern::Target(package, target_name, providers)) => providers
                .into_providers_label(package, target_name.as_ref())
                .configure(ConfigurationData::testing_new()),
            _ => {
                eprintln!("Expected a target, not {}", s);
                panic!();
            }
        };
        let collection = eval
            .heap()
            .alloc(ProviderCollection::try_from_value(providers)?);

        Ok(Dependency::new(eval.heap(), label, collection))
    }
}

#[test]
fn dependency_works() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(buck2_build_api::interpreter::rule_defs::register_rule_defs);
    tester.additional_globals(dependency_creator);
    tester.run_starlark_bzl_test(indoc!(
        r#"
        frozen = create_collection("root//foo:bar[baz]", [DefaultInfo()])
        def test():
            notfrozen = create_collection("root//foo:bar[baz]", [DefaultInfo()])
            expect = "<dependency root//foo:bar[baz] (<testing>#<HASH>)>"

            assert_eq_ignore_hash(expect, repr(notfrozen))
            assert_eq({}, notfrozen[DefaultInfo].sub_targets)
            assert_eq(["baz"], notfrozen.label.sub_target)

            assert_eq_ignore_hash(expect, repr(frozen))
            assert_eq({}, frozen[DefaultInfo].sub_targets)
            assert_eq(["baz"], frozen.label.sub_target)
        "#
    ))?;
    Ok(())
}
