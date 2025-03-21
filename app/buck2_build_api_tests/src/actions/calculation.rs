/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use assert_matches::assert_matches;
use buck2_analysis::analysis::calculation::AnalysisKey;
use buck2_artifact::actions::key::ActionIndex;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::testing::BuildArtifactTestingExt;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_artifact::artifact::source_artifact::SourceArtifact;
use buck2_build_api::actions::calculation::command_details;
use buck2_build_api::actions::calculation::ActionCalculation;
use buck2_build_api::actions::execute::dice_data::set_fallback_executor_config;
use buck2_build_api::actions::execute::dice_data::CommandExecutorResponse;
use buck2_build_api::actions::execute::dice_data::HasCommandExecutor;
use buck2_build_api::actions::execute::dice_data::SetCommandExecutor;
use buck2_build_api::actions::execute::dice_data::SetInvalidationTrackingConfig;
use buck2_build_api::actions::execute::dice_data::SetReClient;
use buck2_build_api::actions::impls::run_action_knobs::RunActionKnobs;
use buck2_build_api::actions::registry::RecordedActions;
use buck2_build_api::actions::Action;
use buck2_build_api::actions::RegisteredAction;
use buck2_build_api::analysis::registry::RecordedAnalysisValues;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::artifact_groups::calculation::ArtifactGroupCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::context::SetBuildContextData;
use buck2_build_api::keep_going::HasKeepGoing;
use buck2_build_api::spawner::BuckSpawner;
use buck2_common::dice::cells::SetCellResolver;
use buck2_common::dice::data::testing::SetTestingIoProvider;
use buck2_common::external_symlink::ExternalSymlink;
use buck2_common::file_ops::testing::TestFileOps;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::http::SetHttpClient;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::dice::inject_legacy_config_for_test;
use buck2_configured::nodes::ConfiguredTargetNodeKey;
use buck2_core::category::CategoryRef;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellResolver;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::package::source_path::SourcePath;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::SetDigestConfig;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::blocking::testing::DummyBlockingExecutor;
use buck2_execute::execute::blocking::SetBlockingExecutor;
use buck2_execute::execute::cache_uploader::NoOpCacheUploader;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::prepared::NoOpCommandOptionalExecutor;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::OutputType;
use buck2_execute::execute::result::CommandExecutionReport;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::execute::testing_dry_run::DryRunEntry;
use buck2_execute::execute::testing_dry_run::DryRunExecutor;
use buck2_execute::materialize::materializer::SetMaterializer;
use buck2_execute::materialize::nodisk::NoDiskMaterializer;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_file_watcher::mergebase::SetMergebase;
use buck2_http::HttpClientBuilder;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dice::testing::DiceBuilder;
use dice::DiceTransaction;
use dice::UserComputationData;
use dupe::Dupe;
use indexmap::indexset;
use maplit::btreemap;
use sorted_vector_map::sorted_vector_map;

use crate::actions::testings::SimpleAction;

fn create_test_configured_target_label() -> ConfiguredTargetLabel {
    TargetLabel::testing_parse("cell//pkg:foo").configure(ConfigurationData::testing_new())
}

fn create_test_build_artifact() -> BuildArtifact {
    let configured_target_label = create_test_configured_target_label();
    let deferred_id = ActionIndex::new(0);
    BuildArtifact::testing_new(configured_target_label, "bar.out", deferred_id)
}

fn create_test_source_artifact(package_label: &str, target_name: &str) -> SourceArtifact {
    SourceArtifact::new(SourcePath::testing_new(package_label, target_name))
}

fn registered_action(
    build_artifact: BuildArtifact,
    action: Box<dyn Action>,
) -> Arc<RegisteredAction> {
    let registered_action = RegisteredAction::new(
        build_artifact.key().dupe(),
        action,
        CommandExecutorConfig::testing_local(),
    );
    Arc::new(registered_action)
}

fn mock_analysis_for_action_resolution(
    mut dice_builder: DiceBuilder,
    action_key: &ActionKey,
    registered_action_arc: Arc<RegisteredAction>,
) -> DiceBuilder {
    let configured_target_label = create_test_configured_target_label();
    let configured_node_key = ConfiguredTargetNodeKey(configured_target_label.dupe());

    assert_eq!(
        &DeferredHolderKey::Base(BaseDeferredKey::TargetLabel(configured_target_label.dupe())),
        action_key.holder_key()
    );

    let mut actions = RecordedActions::new();
    actions.insert(action_key.dupe(), registered_action_arc);

    dice_builder = dice_builder.mock_and_return(
        AnalysisKey(configured_target_label.dupe()),
        buck2_error::Ok(MaybeCompatible::Compatible(AnalysisResult::new(
            RecordedAnalysisValues::testing_new(
                action_key.holder_key().dupe(),
                Vec::new(),
                actions,
            ),
            None,
            HashMap::new(),
            0,
            0,
            None,
        ))),
    );

    dice_builder.mock_and_return(
        configured_node_key,
        Ok(MaybeCompatible::Compatible(
            ConfiguredTargetNode::testing_new(
                configured_target_label,
                "foo_lib",
                ExecutionPlatformResolution::new(None, Vec::new()),
                vec![],
                None,
            ),
        )),
    )
}

async fn make_default_dice_state(
    dry_run_tracker: Arc<Mutex<Vec<DryRunEntry>>>,
    temp_fs: &ProjectRootTemp,
    mocks: Vec<Box<dyn FnOnce(DiceBuilder) -> DiceBuilder>>,
) -> anyhow::Result<DiceTransaction> {
    let fs = temp_fs.path().dupe();

    let cell_resolver = CellResolver::testing_with_name_and_path(
        CellName::testing_new("cell"),
        CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell-path".into())),
    );
    let output_path = ProjectRelativePathBuf::unchecked_new("buck-out/v2".into());

    let mut dice_builder = DiceBuilder::new();
    dice_builder = dice_builder.set_data(|data| {
        data.set_testing_io_provider(temp_fs);
        data.set_digest_config(DigestConfig::testing_default());
        data.set_invalidation_tracking_config(true);
    });

    for mock in mocks.into_iter() {
        dice_builder = mock(dice_builder);
    }

    let mut extra = UserComputationData::new();
    extra.set_keep_going(true);
    struct CommandExecutorProvider {
        dry_run_tracker: Arc<Mutex<Vec<DryRunEntry>>>,
    }
    impl HasCommandExecutor for CommandExecutorProvider {
        fn get_command_executor(
            &self,
            artifact_fs: &ArtifactFs,
            _config: &CommandExecutorConfig,
        ) -> buck2_error::Result<CommandExecutorResponse> {
            let executor = Arc::new(DryRunExecutor::new(
                self.dry_run_tracker.dupe(),
                artifact_fs.clone(),
            ));
            Ok(CommandExecutorResponse {
                executor,
                cache_checker: Arc::new(NoOpCommandOptionalExecutor {}),
                platform: Default::default(),
                cache_uploader: Arc::new(NoOpCacheUploader {}),
            })
        }
    }

    set_fallback_executor_config(&mut extra.data, CommandExecutorConfig::testing_local());
    extra.set_command_executor(Box::new(CommandExecutorProvider { dry_run_tracker }));
    extra.set_blocking_executor(Arc::new(DummyBlockingExecutor { fs }));
    extra.set_materializer(Arc::new(NoDiskMaterializer));
    extra.set_re_client(ManagedRemoteExecutionClient::testing_new_dummy());
    extra.set_http_client(HttpClientBuilder::https_with_system_roots().await?.build());
    extra.set_mergebase(Default::default());
    extra.data.set(EventDispatcher::null());
    extra.data.set(RunActionKnobs::default());
    extra.spawner = Arc::new(BuckSpawner::current_runtime().unwrap());

    let mut computations = dice_builder.build(extra)?;
    inject_legacy_config_for_test(
        &mut computations,
        CellName::testing_new("root"),
        LegacyBuckConfig::empty(),
    )?;
    computations.set_buck_out_path(Some(output_path))?;
    computations.set_cell_resolver(cell_resolver)?;

    Ok(computations.commit().await)
}

#[tokio::test]
async fn test_get_action_for_artifact() -> anyhow::Result<()> {
    let build_artifact = create_test_build_artifact();
    let registered_action = registered_action(
        build_artifact.dupe(),
        Box::new(SimpleAction::new(
            indexset![],
            indexset![build_artifact.dupe()],
            vec![],
            CategoryRef::new("fake_action").unwrap().to_owned(),
            None,
        )),
    );

    let mut dice_builder = DiceBuilder::new();
    dice_builder = mock_analysis_for_action_resolution(
        dice_builder,
        build_artifact.key(),
        registered_action.dupe(),
    );
    let mut dice_computations = dice_builder
        .build(UserComputationData::new())?
        .commit()
        .await;

    let result = with_dispatcher_async(
        EventDispatcher::null(),
        ActionCalculation::get_action(&mut dice_computations, build_artifact.key()),
    )
    .await;
    assert_eq!(result?, registered_action);
    Ok(())
}

#[tokio::test]
async fn test_build_action() -> anyhow::Result<()> {
    let temp_fs = ProjectRootTemp::new()?;
    let build_artifact = create_test_build_artifact();
    let registered_action = registered_action(
        build_artifact.dupe(),
        Box::new(SimpleAction::new(
            indexset![],
            indexset![build_artifact.dupe()],
            vec!["foo".to_owned(), "cmd".to_owned()],
            CategoryRef::new("fake_action").unwrap().to_owned(),
            None,
        )),
    );

    let dry_run_tracker = Arc::new(Mutex::new(vec![]));
    let mut dice_computations = make_default_dice_state(
        dry_run_tracker.dupe(),
        &temp_fs,
        vec![{
            let action = registered_action.dupe();
            let action_key = build_artifact.key().dupe();
            Box::new(move |builder| {
                mock_analysis_for_action_resolution(builder, &action_key, action)
            })
        }],
    )
    .await?;

    let result =
        ActionCalculation::build_action(&mut dice_computations, registered_action.key()).await;

    result.unwrap();

    assert_eq!(
        dry_run_tracker.lock().unwrap()[0],
        DryRunEntry {
            args: vec!["foo".to_owned(), "cmd".to_owned()],
            outputs: vec![CommandExecutionOutput::BuildArtifact {
                path: build_artifact.get_path().dupe(),
                output_type: OutputType::File,
            }],
            env: sorted_vector_map![]
        }
    );

    Ok(())
}

#[tokio::test]
async fn test_build_artifact() -> anyhow::Result<()> {
    let temp_fs = ProjectRootTemp::new()?;
    let build_artifact = create_test_build_artifact();
    let registered_action = registered_action(
        build_artifact.dupe(),
        Box::new(SimpleAction::new(
            indexset![],
            indexset![build_artifact.dupe()],
            vec!["bar".to_owned(), "cmd".to_owned()],
            CategoryRef::new("fake_action").unwrap().to_owned(),
            None,
        )),
    );

    let dry_run_tracker = Arc::new(Mutex::new(vec![]));
    let mut dice_computations = make_default_dice_state(dry_run_tracker.dupe(), &temp_fs, {
        let registered_action = registered_action.dupe();
        let action_key = build_artifact.key().dupe();
        vec![Box::new(move |builder| {
            mock_analysis_for_action_resolution(builder, &action_key, registered_action)
        })]
    })
    .await?;

    let result = with_dispatcher_async(
        EventDispatcher::null(),
        ActionCalculation::build_artifact(&mut dice_computations, &build_artifact),
    )
    .await;

    result.unwrap();

    assert_eq!(
        dry_run_tracker.lock().unwrap()[0],
        DryRunEntry {
            args: vec!["bar".to_owned(), "cmd".to_owned()],
            outputs: vec![CommandExecutionOutput::BuildArtifact {
                path: build_artifact.get_path().dupe(),
                output_type: OutputType::File,
            }],
            env: sorted_vector_map![]
        }
    );
    Ok(())
}

#[tokio::test]
async fn test_ensure_artifact_build_artifact() -> anyhow::Result<()> {
    let temp_fs = ProjectRootTemp::new()?;
    let build_artifact = create_test_build_artifact();
    let registered_action = registered_action(
        build_artifact.dupe(),
        Box::new(SimpleAction::new(
            indexset![],
            indexset![build_artifact.dupe()],
            vec!["ensure".to_owned(), "cmd".to_owned()],
            CategoryRef::new("fake_action").unwrap().to_owned(),
            None,
        )),
    );

    let dry_run_tracker = Arc::new(Mutex::new(vec![]));
    let mut dice_computations = make_default_dice_state(dry_run_tracker.dupe(), &temp_fs, {
        let registered_action = registered_action.dupe();
        let action_key = build_artifact.key().dupe();
        vec![Box::new(move |builder| {
            mock_analysis_for_action_resolution(builder, &action_key, registered_action)
        })]
    })
    .await?;

    let result = with_dispatcher_async(
        EventDispatcher::null(),
        dice_computations
            .ensure_artifact_group(&ArtifactGroup::Artifact(build_artifact.dupe().into())),
    )
    .await;

    result.unwrap();

    assert_eq!(
        dry_run_tracker.lock().unwrap()[0],
        DryRunEntry {
            args: vec!["ensure".to_owned(), "cmd".to_owned()],
            outputs: vec![CommandExecutionOutput::BuildArtifact {
                path: build_artifact.get_path().dupe(),
                output_type: OutputType::File,
            }],
            env: sorted_vector_map![]
        }
    );

    Ok(())
}

#[tokio::test]
async fn test_ensure_artifact_source_artifact() -> anyhow::Result<()> {
    let digest_config = DigestConfig::testing_default();

    let path = CellPath::new(
        CellName::testing_new("cell"),
        CellRelativePathBuf::unchecked_new("pkg/src.cpp".to_owned()),
    );
    let source_artifact = create_test_source_artifact("cell//pkg", "src.cpp");
    let metadata = FileMetadata {
        digest: TrackedFileDigest::from_content(b"content", digest_config.cas_digest_config()),
        is_executable: true,
    };

    let dice_builder = DiceBuilder::new().set_data(|data| {
        data.set_digest_config(DigestConfig::testing_default());
    });
    let file_ops = TestFileOps::new_with_files_metadata(btreemap![path => metadata.dupe()]);
    let mut dice_computations = file_ops
        .mock_in_cell(CellName::testing_new("cell"), dice_builder)
        .build(UserComputationData::new())?
        .commit()
        .await;

    let source_artifact = Artifact::from(source_artifact);
    let input = ArtifactGroup::Artifact(source_artifact.dupe());
    let result = with_dispatcher_async(
        EventDispatcher::null(),
        dice_computations.ensure_artifact_group(&input),
    )
    .await?
    .iter()
    .cloned()
    .collect::<Vec<_>>();

    assert_eq!(
        &result,
        &[(
            source_artifact,
            ArtifactValue::file(FileMetadata {
                digest: metadata.digest,
                is_executable: metadata.is_executable,
            })
        )],
    );
    Ok(())
}

#[tokio::test]
async fn test_ensure_artifact_external_symlink() -> anyhow::Result<()> {
    let path = CellPath::new(
        CellName::testing_new("cell"),
        CellRelativePathBuf::unchecked_new("proj/to_gvfs/include".to_owned()),
    );
    let source_artifact = create_test_source_artifact("cell//proj/to_gvfs", "include");
    let symlink = Arc::new(
        ExternalSymlink::new(
            PathBuf::from("/mnt/gvfs"),
            ForwardRelativePathBuf::new("include".to_owned()).unwrap(),
        )
        .unwrap(),
    );

    let dice_builder = DiceBuilder::new().set_data(|data| {
        data.set_digest_config(DigestConfig::testing_default());
    });
    let file_ops = TestFileOps::new_with_symlinks(btreemap![path => symlink.dupe()]);
    let mut dice_computations = file_ops
        .mock_in_cell(CellName::testing_new("cell"), dice_builder)
        .build(UserComputationData::new())?
        .commit()
        .await;

    let source_artifact = Artifact::from(source_artifact);
    let input = ArtifactGroup::Artifact(source_artifact.dupe());
    let result = with_dispatcher_async(
        EventDispatcher::null(),
        dice_computations.ensure_artifact_group(&input),
    )
    .await?
    .iter()
    .cloned()
    .collect::<Vec<_>>();

    assert_eq!(
        &result,
        &[(
            source_artifact,
            ArtifactValue::new(
                DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(symlink)),
                None
            )
        )]
    );
    Ok(())
}

#[tokio::test]
async fn test_command_details_omission() {
    use buck2_data::command_execution_kind::Command;

    let digest_config = DigestConfig::testing_default();

    let mut report = CommandExecutionReport {
        claim: None,
        status: CommandExecutionStatus::Success {
            execution_kind: CommandExecutionKind::Local {
                digest: ActionDigest::empty(digest_config.cas_digest_config()),
                command: vec![],
                env: sorted_vector_map![],
            },
        },
        timing: Default::default(),
        std_streams: CommandStdStreams::Local {
            stdout: "stdout".to_owned().into_bytes(),
            stderr: "stderr".to_owned().into_bytes(),
        },
        exit_code: Some(1),
        additional_message: None,
    };

    let proto = command_details(&report, false).await;
    let command_kind = proto.command_kind.unwrap();
    assert_matches!(command_kind.command, Some(Command::LocalCommand(..)));
    assert_eq!(&proto.stdout, "stdout");
    assert_eq!(&proto.stderr, "stderr");

    let proto = command_details(&report, true).await;
    let command_kind = proto.command_kind.unwrap();
    assert_matches!(command_kind.command, Some(Command::OmittedLocalCommand(..)));
    assert_eq!(&proto.stdout, "");
    assert_eq!(&proto.stderr, "stderr");

    report.status = CommandExecutionStatus::Failure {
        execution_kind: CommandExecutionKind::Local {
            digest: ActionDigest::empty(digest_config.cas_digest_config()),
            command: vec![],
            env: sorted_vector_map![],
        },
    };
    let proto = command_details(&report, true).await;
    let command_kind = proto.command_kind.unwrap();
    assert_matches!(command_kind.command, Some(Command::LocalCommand(..)));
    assert_eq!(&proto.stdout, "stdout");
    assert_eq!(&proto.stderr, "stderr");
}
