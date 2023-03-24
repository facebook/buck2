/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod testing {
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
    use buck2_core::pattern::pattern_type::TargetPatternExtra;
    use buck2_core::pattern::ParsedPattern;
    use buck2_core::target::label::TargetLabel;
    use buck2_interpreter::types::label::Label;
    use buck2_interpreter::types::target_label::StarlarkTargetLabel;
    use starlark::environment::GlobalsBuilder;
    use starlark::eval::Evaluator;
    use starlark::starlark_module;

    use crate::interpreter::build_context::BuildContext;

    #[derive(Debug, thiserror::Error)]
    enum LabelCreatorError {
        #[error("Expected provider, found something else: `{0}`")]
        ExpectedProvider(String),
        #[error("Expected target, found something else: `{0}`")]
        ExpectedTarget(String),
    }

    #[starlark_module]
    pub fn label_creator(builder: &mut GlobalsBuilder) {
        fn label<'v>(s: &str, eval: &mut Evaluator<'v, '_>) -> anyhow::Result<Label> {
            let c = BuildContext::from_context(eval)?;
            let target = match ParsedPattern::<ProvidersPatternExtra>::parse_precise(
                c.cell_info().cell_alias_resolver(),
                s,
            )? {
                ParsedPattern::Target(package, target_name, providers) => {
                    providers.into_providers_label(package, target_name.as_ref())
                }
                _ => return Err(LabelCreatorError::ExpectedProvider(s.to_owned()).into()),
            };
            Ok(Label::new(
                target.configure(ConfigurationData::testing_new()),
            ))
        }

        fn target_label<'v>(
            s: &str,
            eval: &mut Evaluator<'v, '_>,
        ) -> anyhow::Result<StarlarkTargetLabel> {
            let c = BuildContext::from_context(eval)?;
            let target = match ParsedPattern::<TargetPatternExtra>::parse_precise(
                c.cell_info().cell_alias_resolver(),
                s,
            )? {
                ParsedPattern::Target(package, target_name, TargetPatternExtra) => {
                    TargetLabel::new(package, target_name.as_ref())
                }
                _ => return Err(LabelCreatorError::ExpectedTarget(s.to_owned()).into()),
            };
            Ok(StarlarkTargetLabel::new(target))
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::testing::label_creator;
    use crate::interpreter::testing::expect_error;
    use crate::interpreter::testing::Tester;

    #[test]
    fn labels_are_usable() -> anyhow::Result<()> {
        fn new_tester() -> anyhow::Result<Tester> {
            let mut tester = Tester::new()?;
            tester.additional_globals(label_creator);
            Ok(tester)
        }

        let mut tester = new_tester()?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            frozen_l_default = label("root//foo/bar:baz")
            frozen_l = label("root//foo/bar:baz[something]")
            def test():
                l_default = label("root//foo/bar:baz")
                l = label("root//foo/bar:baz[something]")

                assert_eq_ignore_hash("root//foo/bar:baz (<testing>#<HASH>)", repr(frozen_l_default))
                assert_eq_ignore_hash("root//foo/bar:baz (<testing>#<HASH>)", str(frozen_l_default))
                assert_eq("foo/bar", frozen_l_default.package)
                assert_eq("baz", frozen_l_default.name)
                assert_eq(None, frozen_l_default.sub_target)
                assert_eq("root", frozen_l_default.cell)

                assert_eq_ignore_hash("root//foo/bar:baz[something] (<testing>#<HASH>)", repr(frozen_l))
                assert_eq_ignore_hash("root//foo/bar:baz[something] (<testing>#<HASH>)", str(frozen_l))
                assert_eq("foo/bar", frozen_l.package)
                assert_eq("baz", frozen_l.name)
                assert_eq(["something"], frozen_l.sub_target)

                assert_eq_ignore_hash("root//foo/bar:baz (<testing>#<HASH>)", repr(l_default))
                assert_eq_ignore_hash("root//foo/bar:baz (<testing>#<HASH>)", str(l_default))
                assert_eq("foo/bar", l_default.package)
                assert_eq("baz", l_default.name)
                assert_eq(None, l_default.sub_target)

                assert_eq_ignore_hash("root//foo/bar:baz[something] (<testing>#<HASH>)", repr(l))
                assert_eq_ignore_hash("root//foo/bar:baz[something] (<testing>#<HASH>)", str(l))
                assert_eq("foo/bar", l.package)
                assert_eq("baz", l.name)
                assert_eq(["something"], l.sub_target)
                assert_eq("root", l.cell)

            "#
        ))?;

        let mut tester = new_tester()?;
        let invalid_fields = indoc!(
            r#"
            l = label("root//foo:bar[baz]")
            def test():
                l.invalid_field
            "#
        );
        expect_error(
            tester.run_starlark_test(invalid_fields),
            invalid_fields,
            "Object of type `label` has no attribute `invalid_field`",
        );
        Ok(())
    }
}
