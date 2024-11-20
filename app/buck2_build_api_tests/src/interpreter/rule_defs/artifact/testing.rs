/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_artifact::actions::key::ActionIndex;
use buck2_artifact::artifact::artifact_type::testing::BuildArtifactTestingExt;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_artifact::artifact::source_artifact::SourceArtifact;
use buck2_build_api::actions::registry::ActionsRegistry;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use buck2_build_api::interpreter::rule_defs::artifact::output_artifact_like::OutputArtifactArg;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::unpack_artifact::UnpackArtifactOrDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_core::category::CategoryRef;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutPathResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::source_path::SourcePath;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::execute::request::OutputType;
use buck2_interpreter_for_build::interpreter::build_context::BuildContext;
use buck2_interpreter_for_build::interpreter::testing::cells;
use buck2_util::arc_str::ArcS;
use dupe::Dupe;
use indexmap::indexset;
use indexmap::IndexSet;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::Value;

use crate::actions::testings::SimpleUnregisteredAction;

fn get_label(eval: &Evaluator, target: &str) -> anyhow::Result<ConfiguredTargetLabel> {
    let ctx = BuildContext::from_context(eval)?;
    match ParsedPattern::<TargetPatternExtra>::parse_precise(
        target,
        ctx.build_file_cell().name(),
        ctx.cell_resolver(),
        ctx.cell_info.cell_alias_resolver(),
    ) {
        Ok(ParsedPattern::Target(package, target_name, TargetPatternExtra)) => {
            Ok(TargetLabel::new(package, target_name.as_ref())
                .configure(ConfigurationData::testing_new()))
        }
        _ => panic!("expected a valid target"),
    }
}

#[starlark_module]
pub(crate) fn artifactory(builder: &mut GlobalsBuilder) {
    fn source_artifact(
        package: &str,
        path: &str,
        eval: &mut Evaluator,
    ) -> anyhow::Result<StarlarkArtifact> {
        let ctx = BuildContext::from_context(eval)?;
        let package = PackageLabel::new(
            ctx.build_file_cell().name(),
            CellRelativePath::from_path(package).unwrap(),
        );
        let path = SourcePath::new(package, ArcS::from(PackageRelativePath::new(path)?));
        Ok(StarlarkArtifact::new(SourceArtifact::new(path).into()))
    }

    fn bound_artifact(
        target: &str,
        path: &str,
        eval: &mut Evaluator,
    ) -> anyhow::Result<StarlarkArtifact> {
        let target_label = get_label(eval, target)?;
        let id = ActionIndex::new(0);
        let artifact = Artifact::from(BuildArtifact::testing_new(target_label, path, id));
        Ok(StarlarkArtifact::new(artifact))
    }

    fn declared_artifact(
        path: &str,
        eval: &mut Evaluator,
    ) -> anyhow::Result<StarlarkDeclaredArtifact> {
        let target_label = get_label(eval, "//foo:bar")?;
        let mut registry = ActionsRegistry::new(
            DeferredHolderKey::Base(BaseDeferredKey::TargetLabel(target_label)),
            ExecutionPlatformResolution::unspecified(),
        );
        let artifact = registry.declare_artifact(
            None,
            ForwardRelativePathBuf::try_from(path.to_owned()).unwrap(),
            OutputType::File,
            None,
        )?;
        Ok(StarlarkDeclaredArtifact::new(
            None,
            artifact,
            AssociatedArtifacts::new(),
        ))
    }

    fn declared_bound_artifact(
        target: &str,
        path: &str,
        eval: &mut Evaluator,
    ) -> anyhow::Result<StarlarkDeclaredArtifact> {
        let target_label = get_label(eval, target)?;
        let mut registry = ActionsRegistry::new(
            DeferredHolderKey::Base(BaseDeferredKey::TargetLabel(target_label.dupe())),
            ExecutionPlatformResolution::unspecified(),
        );
        let artifact = registry.declare_artifact(
            None,
            ForwardRelativePathBuf::try_from(path.to_owned()).unwrap(),
            OutputType::File,
            None,
        )?;
        let outputs = indexset![artifact.as_output()];
        registry.register(
            &DeferredHolderKey::Base(BaseDeferredKey::TargetLabel(target_label.dupe())),
            IndexSet::new(),
            outputs,
            SimpleUnregisteredAction::new(
                vec![],
                CategoryRef::new("fake_action").unwrap().to_owned(),
                None,
            ),
        )?;
        Ok(StarlarkDeclaredArtifact::new(
            None,
            artifact,
            AssociatedArtifacts::new(),
        ))
    }

    fn stringify_for_cli<'v>(artifact: ValueAsArtifactLike<'v>) -> anyhow::Result<String> {
        let cell_info = cells(None).unwrap();
        let project_fs =
            ProjectRoot::new(AbsNormPathBuf::try_from(std::env::current_dir().unwrap()).unwrap())
                .unwrap();
        let fs = ArtifactFs::new(
            cell_info.1,
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
                "buck-out/v2".to_owned(),
            )),
            project_fs,
        );
        let executor_fs = ExecutorFs::new(&fs, PathSeparatorKind::Unix);
        let mut cli = Vec::<String>::new();
        let mut ctx = DefaultCommandLineContext::new(&executor_fs);
        artifact
            .0
            .as_command_line_like()
            .add_to_command_line(&mut cli, &mut ctx)
            .unwrap();
        assert_eq!(1, cli.len());
        Ok(cli.first().unwrap().to_owned())
    }

    // Mainly tests get_or_declare_output function that can transfer associated artifacts
    // artifact parameter can be either string or artifact
    fn declared_bound_artifact_with_associated_artifacts<'v>(
        // TODO(nga): parameters should be either positional or named, not both.
        artifact: OutputArtifactArg<'v>,
        associated_artifacts: UnpackListOrTuple<UnpackArtifactOrDeclaredArtifact<'v>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let target_label = get_label(eval, "//foo:bar")?;
        let mut analysis_registry = AnalysisRegistry::new_from_owner(
            BaseDeferredKey::TargetLabel(target_label.dupe()),
            ExecutionPlatformResolution::unspecified(),
        )?;
        let mut actions_registry = ActionsRegistry::new(
            DeferredHolderKey::Base(BaseDeferredKey::TargetLabel(target_label.dupe())),
            ExecutionPlatformResolution::unspecified(),
        );

        let associated_artifacts = AssociatedArtifacts::from(
            associated_artifacts
                .items
                .iter()
                .map(|a| ArtifactGroup::Artifact(a.artifact().unwrap())),
        );
        let (declaration, output_artifact) =
            analysis_registry.get_or_declare_output(eval, artifact, OutputType::File)?;

        actions_registry.register(
            &DeferredHolderKey::Base(BaseDeferredKey::TargetLabel(target_label.dupe())),
            IndexSet::new(),
            indexset![output_artifact],
            SimpleUnregisteredAction::new(
                vec![],
                CategoryRef::new("fake_action").unwrap().to_owned(),
                None,
            ),
        )?;

        let value = declaration
            .into_declared_artifact(associated_artifacts)
            .to_value();
        Ok(value)
    }

    fn get_associated_artifacts_as_string<'v>(
        artifact: ValueAsArtifactLike<'v>,
    ) -> anyhow::Result<String> {
        let associated_artifacts = artifact.0.get_associated_artifacts();
        let s: String = associated_artifacts
            .iter()
            .flat_map(|v| v.iter())
            .map(|a| a.to_string())
            .collect();
        Ok(s)
    }
}
