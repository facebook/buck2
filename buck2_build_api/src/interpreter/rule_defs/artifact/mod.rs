/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod starlark_artifact;
pub mod starlark_artifact_like;
mod starlark_artifact_value;
mod starlark_declared_artifact;
mod starlark_output_artifact;

use std::fmt::Debug;

use buck2_execute::base_deferred_key::BaseDeferredKey;

pub use self::starlark_artifact::StarlarkArtifact;
pub(crate) use self::starlark_artifact_like::StarlarkArtifactLike;
pub(crate) use self::starlark_artifact_like::ValueAsArtifactLike;
pub use self::starlark_artifact_value::StarlarkArtifactValue;
pub use self::starlark_declared_artifact::StarlarkDeclaredArtifact;
pub use self::starlark_output_artifact::FrozenStarlarkOutputArtifact;
pub use self::starlark_output_artifact::StarlarkOutputArtifact;

#[derive(Debug, thiserror::Error)]
enum ArtifactError {
    #[error("expected artifact {repr} to be used as the output of an action, but it was not")]
    DeclaredArtifactWasNotBound { repr: String },
    #[error(
        "attempted to use source artifact {repr} as the output of an action. Source \
        artifacts may not be outputs."
    )]
    SourceArtifactAsOutput { repr: String },
    #[error(
        "attempted to use artifact {artifact_repr} as the output of an action, but \
        it was already used by another action in {existing_owner}"
    )]
    BoundArtifactAsOutput {
        artifact_repr: String,
        existing_owner: BaseDeferredKey,
    },
}

#[cfg(test)]
pub mod testing {
    use std::sync::Arc;

    use buck2_common::executor_config::PathSeparatorKind;
    use buck2_common::sorted_index_set::SortedIndexSet;
    use buck2_core::buck_path::BuckPath;
    use buck2_core::category::Category;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::paths::AbsPathBuf;
    use buck2_core::fs::paths::ForwardRelativePathBuf;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::Package;
    use buck2_core::pattern::ParsedPattern;
    use buck2_core::pattern::TargetPattern;
    use buck2_core::target::ConfiguredTargetLabel;
    use buck2_core::target::TargetLabel;
    use buck2_execute::artifact::fs::ArtifactFs;
    use buck2_execute::artifact::fs::ExecutorFs;
    use buck2_execute::artifact::source_artifact::SourceArtifact;
    use buck2_execute::base_deferred_key::BaseDeferredKey;
    use buck2_execute::path::buck_out_path::BuckOutPathResolver;
    use buck2_execute::path::buck_out_path::BuckPathResolver;
    use buck2_interpreter::extra::BuildContext;
    use buck2_node::configuration::execution::ExecutionPlatformResolution;
    use gazebo::prelude::*;
    use indexmap::indexset;
    use indexmap::IndexSet;
    use starlark::environment::GlobalsBuilder;
    use starlark::eval::Evaluator;
    use starlark::values::Value;

    use crate::actions::artifact::build_artifact::BuildArtifact;
    use crate::actions::artifact::testing::BuildArtifactTestingExt;
    use crate::actions::artifact::Artifact;
    use crate::actions::registry::ActionsRegistry;
    use crate::actions::testings::SimpleUnregisteredAction;
    use crate::analysis::registry::AnalysisRegistry;
    use crate::artifact_groups::ArtifactGroup;
    use crate::deferred::types::testing::DeferredIdExt;
    use crate::deferred::types::BaseKey;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredRegistry;
    use crate::interpreter::rule_defs::artifact::StarlarkArtifact;
    use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
    use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;
    use crate::interpreter::rule_defs::cmd_args::BaseCommandLineBuilder;
    use crate::interpreter::testing::cells;

    fn get_label(eval: &Evaluator, target: &str) -> anyhow::Result<ConfiguredTargetLabel> {
        let ctx = BuildContext::from_context(eval)?;
        match ParsedPattern::<TargetPattern>::parse_precise(
            ctx.cell_info().cell_alias_resolver(),
            target,
        ) {
            Ok(ParsedPattern::Target(package, target_name)) => {
                Ok(TargetLabel::new(package, target_name).configure(Configuration::testing_new()))
            }
            _ => panic!("expected a valid target"),
        }
    }

    #[starlark_module]
    pub fn artifactory(builder: &mut GlobalsBuilder) {
        fn source_artifact(
            package: &str,
            path: &str,
            eval: &mut Evaluator,
        ) -> anyhow::Result<StarlarkArtifact> {
            let ctx = BuildContext::from_context(eval)?;
            let package = Package::new(
                ctx.cell_info().name().name(),
                CellRelativePath::from_path(package).unwrap(),
            );
            let path = BuckPath::new(
                package,
                PackageRelativePathBuf::try_from(path.to_owned()).unwrap(),
            );
            Ok(StarlarkArtifact::new(SourceArtifact::new(path).into()))
        }

        fn bound_artifact(
            target: &str,
            path: &str,
            eval: &mut Evaluator,
        ) -> anyhow::Result<StarlarkArtifact> {
            let target_label = get_label(eval, target)?;
            let id = DeferredId::testing_new(0);
            let artifact = Artifact::from(BuildArtifact::testing_new(
                target_label,
                ForwardRelativePathBuf::try_from(path.to_owned()).unwrap(),
                id,
            ));
            Ok(StarlarkArtifact::new(artifact))
        }

        fn declared_artifact(
            path: &str,
            eval: &mut Evaluator,
        ) -> anyhow::Result<StarlarkDeclaredArtifact> {
            let target_label = get_label(eval, "//foo:bar")?;
            let mut registry = ActionsRegistry::new(
                BaseDeferredKey::TargetLabel(target_label),
                ExecutionPlatformResolution::unspecified(),
            );
            let artifact = registry.declare_artifact(
                None,
                ForwardRelativePathBuf::try_from(path.to_owned()).unwrap(),
            )?;
            Ok(StarlarkDeclaredArtifact::new(
                None,
                artifact,
                Default::default(),
            ))
        }

        fn declared_bound_artifact(
            target: &str,
            path: &str,
            eval: &mut Evaluator,
        ) -> anyhow::Result<StarlarkDeclaredArtifact> {
            let target_label = get_label(eval, target)?;
            let mut deferred = DeferredRegistry::new(BaseKey::Base(BaseDeferredKey::TargetLabel(
                target_label.dupe(),
            )));
            let mut registry = ActionsRegistry::new(
                BaseDeferredKey::TargetLabel(target_label),
                ExecutionPlatformResolution::unspecified(),
            );
            let artifact = registry.declare_artifact(
                None,
                ForwardRelativePathBuf::try_from(path.to_owned()).unwrap(),
            )?;
            let outputs = indexset![artifact.as_output()];
            registry.register(
                &mut deferred,
                IndexSet::new(),
                outputs,
                SimpleUnregisteredAction::new(
                    vec![],
                    Category::try_from("fake_action").unwrap(),
                    None,
                ),
            )?;
            Ok(StarlarkDeclaredArtifact::new(
                None,
                artifact,
                Default::default(),
            ))
        }

        fn stringify_for_cli<'v>(artifact: Value<'v>) -> anyhow::Result<String> {
            let cell_info = cells(None).unwrap();
            let project_fs =
                ProjectRoot::new(AbsPathBuf::try_from(std::env::current_dir().unwrap()).unwrap());
            let fs = ArtifactFs::new(
                BuckPathResolver::new(cell_info.1),
                BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
                    "buck-out/v2".to_owned(),
                )),
                project_fs,
            );
            let executor_fs = ExecutorFs::new(&fs, PathSeparatorKind::Unix);
            let mut builder = BaseCommandLineBuilder::new(&executor_fs);
            artifact
                .as_artifact()
                .unwrap()
                .as_command_line_like()
                .add_to_command_line(&mut builder)
                .unwrap();
            let cli = builder.build();
            assert_eq!(1, cli.len());
            Ok(cli.get(0).unwrap().to_owned())
        }

        // Mainly tests get_or_declare_output function that can transfer associated artifacts
        // artifact parameter can be either string or artifact
        fn declared_bound_artifact_with_associated_artifacts<'v>(
            artifact: Value<'v>,
            associated_artifacts: Vec<StarlarkArtifact>,
            eval: &mut Evaluator<'v, '_>,
        ) -> anyhow::Result<Value<'v>> {
            let target_label = get_label(eval, "//foo:bar")?;
            let mut analysis_registry = AnalysisRegistry::new_from_owner(
                BaseDeferredKey::TargetLabel(target_label.dupe()),
                ExecutionPlatformResolution::unspecified(),
            );
            let mut actions_registry = ActionsRegistry::new(
                BaseDeferredKey::TargetLabel(target_label.dupe()),
                ExecutionPlatformResolution::unspecified(),
            );
            let mut deferred = DeferredRegistry::new(BaseKey::Base(BaseDeferredKey::TargetLabel(
                target_label.dupe(),
            )));

            let associated_artifacts: IndexSet<ArtifactGroup> = associated_artifacts
                .iter()
                .map(|a| ArtifactGroup::Artifact(a.artifact()))
                .collect();
            let (value, output_artifact) = analysis_registry.get_or_declare_output(
                eval,
                artifact,
                "param_name",
                Arc::new(SortedIndexSet::new(associated_artifacts)),
            )?;

            actions_registry.register(
                &mut deferred,
                IndexSet::new(),
                indexset![output_artifact],
                SimpleUnregisteredAction::new(
                    vec![],
                    Category::try_from("fake_action").unwrap(),
                    None,
                ),
            )?;
            Ok(value)
        }

        fn get_associated_artifacts_as_string<'v>(artifact: Value<'v>) -> anyhow::Result<String> {
            let artifact = artifact.as_artifact().unwrap();
            let (_, associated_artifacts) =
                artifact.get_bound_artifact_and_associated_artifacts()?;
            let s: String = associated_artifacts.iter().map(|a| a.to_string()).collect();
            Ok(s)
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_common::result::SharedResult;
    use indoc::indoc;

    use crate::interpreter::rule_defs::artifact::testing::artifactory;
    use crate::interpreter::testing::expect_error;
    use crate::interpreter::testing::Tester;

    #[test]
    fn source_artifact() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(artifactory);
        tester.run_starlark_bzl_test(indoc!(
            r#"
            a1 = source_artifact("foo/bar", "baz/quz.h")
            a2 = source_artifact("foo/bar", "baz/file1")

            def test():
                a3 = source_artifact("foo/bar", "baz/quz.cpp")
                a4 = source_artifact("foo/bar", "baz/file2")

                assert_eq("<source foo/bar/baz/quz.h>", repr(a1))
                assert_eq("quz.h", a1.basename)
                assert_eq("baz/quz.h", a1.short_path)
                assert_eq(".h", a1.extension)
                assert_eq(True, a1.is_source)
                assert_eq(None, a1.owner)

                assert_eq("<source foo/bar/baz/file1>", repr(a2))
                assert_eq("file1", a2.basename)
                assert_eq("baz/file1", a2.short_path)
                assert_eq("", a2.extension)
                assert_eq(True, a2.is_source)
                assert_eq(None, a2.owner)

                assert_eq("<source foo/bar/baz/quz.cpp>", repr(a3))
                assert_eq("quz.cpp", a3.basename)
                assert_eq("baz/quz.cpp", a3.short_path)
                assert_eq(".cpp", a3.extension)
                assert_eq(True, a3.is_source)
                assert_eq(None, a3.owner)

                assert_eq("<source foo/bar/baz/file2>", repr(a4))
                assert_eq("file2", a4.basename)
                assert_eq("baz/file2", a4.short_path)
                assert_eq("", a4.extension)
                assert_eq(True, a4.is_source)
                assert_eq(None, a4.owner)

                # Validate that attrs are setup properly
                for a in (a1, a2, a3, a4):
                    for prop in dir(a):
                        assert_eq(True, hasattr(a, prop))
                        if prop != "as_output":
                            getattr(a, prop)
            "#
        ))?;

        let as_output = indoc!(
            r#"
            def test():
                source_artifact("foo/bar", "baz/quz.cpp").as_output()
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(as_output),
            as_output,
            "Source artifacts may not be outputs",
        );
        Ok(())
    }

    #[test]
    fn bound_artifact() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(|x| {
            crate::interpreter::rule_defs::register_rule_defs(x);
            artifactory(x)
        });
        tester.run_starlark_bzl_test(indoc!(
            r#"
            a1 = bound_artifact("//foo:bar", "baz/quz.h")
            a2 = bound_artifact("//foo:bar", "baz/file1")

            def test():
                a3 = bound_artifact("//foo:bar", "baz/quz.cpp")
                a4 = bound_artifact("//foo:bar", "baz/file2")

                assert_eq("<build artifact baz/quz.h bound to root//foo:bar (<testing>)>", repr(a1))
                assert_eq("quz.h", a1.basename)
                assert_eq("baz/quz.h", a1.short_path)
                assert_eq(".h", a1.extension)
                assert_eq(False, a1.is_source)
                assert_eq("bar", a1.owner.name)

                assert_eq("<build artifact baz/file1 bound to root//foo:bar (<testing>)>", repr(a2))
                assert_eq("file1", a2.basename)
                assert_eq("baz/file1", a2.short_path)
                assert_eq("", a2.extension)
                assert_eq(False, a2.is_source)
                assert_eq("bar", a2.owner.name)

                assert_eq("<build artifact baz/quz.cpp bound to root//foo:bar (<testing>)>", repr(a3))
                assert_eq("quz.cpp", a3.basename)
                assert_eq("baz/quz.cpp", a3.short_path)
                assert_eq(".cpp", a3.extension)
                assert_eq(False, a3.is_source)
                assert_eq("bar", a3.owner.name)

                assert_eq("<build artifact baz/file2 bound to root//foo:bar (<testing>)>", repr(a4))
                assert_eq("file2", a4.basename)
                assert_eq("baz/file2", a4.short_path)
                assert_eq("", a4.extension)
                assert_eq(False, a4.is_source)
                assert_eq("bar", a4.owner.name)

                # Validate that attrs are setup properly
                for a in (a1, a2, a3, a4):
                    for prop in dir(a):
                        assert_eq(True, hasattr(a, prop))
                        if prop != "as_output":
                            getattr(a, prop)
            "#
        ))?;

        let as_output = indoc!(
            r#"
            def test():
                bound_artifact("//foo:bar", "baz/quz.cpp").as_output()
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(as_output),
            as_output,
            "already used",
        );
        Ok(())
    }

    #[test]
    fn declared_artifact() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(artifactory);
        tester.run_starlark_bzl_test(indoc!(
            r#"
            def test():
                a1 = declared_artifact("baz/quz.cpp")
                a2 = declared_artifact("baz/file2")

                assert_eq("<build artifact baz/quz.cpp>", repr(a1))
                assert_eq("quz.cpp", a1.basename)
                assert_eq(".cpp", a1.extension)
                assert_eq(False, a1.is_source)
                assert_eq(None, a1.owner)
                assert_eq("<output artifact for baz/quz.cpp>", repr(a1.as_output()))

                assert_eq("<build artifact baz/file2>", repr(a2))
                assert_eq("file2", a2.basename)
                assert_eq("", a2.extension)
                assert_eq(False, a2.is_source)
                assert_eq(None, a2.owner)
                assert_eq("<output artifact for baz/file2>", repr(a2.as_output()))

                # Validate that attrs are setup properly
                for a in (a1, a2):
                    for prop in dir(a):
                        assert_eq(True, hasattr(a, prop))
                        if prop != "as_output":
                            getattr(a, prop)
            "#
        ))?;
        Ok(())
    }

    #[test]
    fn declared_bound() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(|x| {
            crate::interpreter::rule_defs::register_rule_defs(x);
            artifactory(x)
        });
        tester.run_starlark_bzl_test(indoc!(
            r#"
            a1 = declared_bound_artifact("//foo:bar", "baz/quz.h")
            a2 = declared_bound_artifact("//foo:bar", "baz/file1")

            def test():
                a3 = declared_bound_artifact("//foo:bar", "baz/quz.cpp")
                a4 = declared_bound_artifact("//foo:bar", "baz/file2")

                assert_eq("<build artifact baz/quz.h bound to root//foo:bar (<testing>)>", repr(a1))
                assert_eq("quz.h", a1.basename)
                assert_eq("baz/quz.h", a1.short_path)
                assert_eq(".h", a1.extension)
                assert_eq(False, a1.is_source)
                assert_eq("bar", a1.owner.name)

                assert_eq("<build artifact baz/file1 bound to root//foo:bar (<testing>)>", repr(a2))
                assert_eq("file1", a2.basename)
                assert_eq("baz/file1", a2.short_path)
                assert_eq("", a2.extension)
                assert_eq(False, a2.is_source)
                assert_eq("bar", a2.owner.name)

                assert_eq("<build artifact baz/quz.cpp bound to root//foo:bar (<testing>)>", repr(a3))
                assert_eq("quz.cpp", a3.basename)
                assert_eq("baz/quz.cpp", a3.short_path)
                assert_eq(".cpp", a3.extension)
                assert_eq(False, a3.is_source)
                assert_eq("bar", a3.owner.name)

                assert_eq("<build artifact baz/file2 bound to root//foo:bar (<testing>)>", repr(a4))
                assert_eq("file2", a4.basename)
                assert_eq("baz/file2", a4.short_path)
                assert_eq("", a4.extension)
                assert_eq(False, a4.is_source)
                assert_eq("bar", a4.owner.name)

                # Validate that attrs are setup properly
                for a in (a1, a2, a3, a4):
                    for prop in dir(a):
                        assert_eq(True, hasattr(a, prop))
                        if prop != "as_output":
                            getattr(a, prop)
            "#
        ))?;

        Ok(())
    }

    #[test]
    fn project_declared_artifact() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(artifactory);
        tester.run_starlark_bzl_test(indoc!(
            r#"
            def test():
                bound = declared_bound_artifact("//foo:bar", "out").project("baz.o")
                assert_eq("<build artifact out/baz.o bound to root//foo:bar (<testing>)>", repr(bound))
                assert_eq("baz.o", bound.basename)
                assert_eq(".o", bound.extension)

                unbound = declared_artifact("out").project("qux.so")
                assert_eq("<build artifact out/qux.so>", repr(unbound))
                assert_eq("<output artifact for out/qux.so>", repr(unbound.as_output()))
                assert_eq("qux.so", unbound.basename)
                assert_eq(".so", unbound.extension)
            "#
        ))?;
        Ok(())
    }

    #[test]
    fn project_source_artifact() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(artifactory);
        let test = indoc!(
            r#"
            def test():
                source_artifact("foo/bar", "baz").project("foo")
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Source artifacts cannot be projected",
        );
        Ok(())
    }

    #[test]
    fn project_artifact() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(artifactory);
        let test = indoc!(
            r#"
            def test():
                bound_artifact("//foo:bar", "baz").project("foo")
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "This artifact was declared by another rule",
        );
        Ok(())
    }

    #[test]
    fn stringifies_for_command_line() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(artifactory);
        tester.run_starlark_bzl_test(indoc!(
            r#"
            a1 = bound_artifact("//foo:bar", "baz/quz.h")
            a2 = source_artifact("foo/bar", "baz/file1")

            def test():
                a3 = bound_artifact("//foo:bar", "baz/quz.cpp")
                a4 = source_artifact("foo/bar", "baz/file2")

                assert_eq_ignore_hash("buck-out/v2/gen/root/<HASH>/foo/__bar__/baz/quz.h", stringify_for_cli(a1))
                assert_eq("foo/bar/baz/file1", stringify_for_cli(a2))
                assert_eq_ignore_hash("buck-out/v2/gen/root/<HASH>/foo/__bar__/baz/quz.cpp", stringify_for_cli(a3))
                assert_eq("foo/bar/baz/file2", stringify_for_cli(a4))
            "#
        ))
    }

    #[test]
    fn bound_artifact_with_associated_artifacts() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(|x| {
            crate::interpreter::rule_defs::register_rule_defs(x);
            artifactory(x)
        });
        tester.run_starlark_bzl_test(indoc!(
            r#"
            def test():
                # declare an artifact (a2) with string and add an associated artifact (a1)
                a1 = source_artifact("foo/bar", "baz/file1")
                a2 = declared_bound_artifact_with_associated_artifacts("baz/quz.h", [a1])
                assert_eq(a2.short_path, "baz/quz.h")
                assert_eq(get_associated_artifacts_as_string(a1), "")
                assert_eq(get_associated_artifacts_as_string(a2), "root//foo/bar/baz/file1")

                # use a predeclared artifact (a3) and add an associated artifact (a4)
                a3 = declared_artifact("wom/bat.h")
                a4 = source_artifact("foo/bar", "baz/file2")
                a5 = declared_bound_artifact_with_associated_artifacts(a3, [a4])
                assert_eq(a3.short_path, "wom/bat.h")
                assert_eq(a5.short_path, "wom/bat.h")
                assert_eq(get_associated_artifacts_as_string(a3), "")
                assert_eq(get_associated_artifacts_as_string(a5), "root//foo/bar/baz/file2")

                # use a predeclared artifact (a3) with no associated artifacts
                a6 = declared_bound_artifact_with_associated_artifacts(a3, [])
                assert_eq(a6.short_path, "wom/bat.h")
                assert_eq(get_associated_artifacts_as_string(a6), "")
            "#
        ))
    }
}
