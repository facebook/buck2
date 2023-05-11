/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_build_api::actions::artifact::artifact_type::Artifact;
use buck2_build_api::actions::artifact::artifact_type::OutputArtifact;
use buck2_build_api::actions::impls::json::visit_json_artifacts;
use buck2_build_api::actions::impls::json::SerializeValue;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact_tagging::testing::artifact_tag_factory;
use buck2_build_api::interpreter::rule_defs::artifact_tagging::ArtifactTag;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use dupe::Dupe;
use indoc::indoc;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::Value;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

#[test]
fn test_tagging() -> anyhow::Result<()> {
    struct AssertVisitor {
        tag: ArtifactTag,
        artifact: Artifact,
    }

    impl CommandLineArtifactVisitor for AssertVisitor {
        fn visit_input(&mut self, input: ArtifactGroup, tag: Option<&ArtifactTag>) {
            assert_eq!(tag, Some(&self.tag));
            assert_eq!(input, ArtifactGroup::Artifact(self.artifact.dupe()));
        }

        fn visit_output(&mut self, _artifact: OutputArtifact, _tag: Option<&ArtifactTag>) {}
    }

    #[starlark_module]
    fn assertions(builder: &mut GlobalsBuilder) {
        fn check_artifact_is_tagged<'v>(
            tagged: Value<'v>,
            tag: Value<'v>,
            artifact: Value<'v>,
        ) -> anyhow::Result<Value<'v>> {
            let tag = ArtifactTag::from_value(tag).context("Invalid tag")?.dupe();

            let artifact = artifact
                .as_artifact()
                .context("Not an artifact")?
                .get_bound_artifact()
                .context("Not a bound artifact")?
                .dupe();

            visit_json_artifacts(tagged, &mut AssertVisitor { tag, artifact })?;
            Ok(Value::new_none())
        }

        fn check_passthrough<'v>(tagged: Value<'v>, value: Value<'v>) -> anyhow::Result<Value<'v>> {
            let json1 = serde_json::to_string(&SerializeValue {
                value: tagged,
                fs: None,
            })?;

            let json2 = serde_json::to_string(&SerializeValue { value, fs: None })?;

            assert_eq!(json1, json2);

            Ok(Value::new_none())
        }
    }

    let mut tester = Tester::new()?;
    tester.additional_globals(artifact_tag_factory);
    tester.additional_globals(artifactory);
    tester.additional_globals(assertions);

    tester.run_starlark_bzl_test(indoc!(
        r#"
        def test():
            t1 = make_tag()
            a1 = source_artifact("foo", "bar")
            v1 = {"foo": "bar"}

            check_artifact_is_tagged(t1.tag_artifacts(a1), t1, a1)
            check_passthrough(t1.tag_artifacts(v1), v1)
        "#
    ))?;

    Ok(())
}
