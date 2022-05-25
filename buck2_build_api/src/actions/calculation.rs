/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::result::{SharedResult, ToSharedResultExt};
use buck2_interpreter::dice::HasEvents;
use derive_more::Display;
use dice::{DiceComputations, Key};
use futures::stream::FuturesUnordered;
use gazebo::prelude::*;
use tracing::debug;

use crate::{
    actions::{
        artifact::BuildArtifact,
        build_listener::{ActionExecutionSignal, HasBuildSignals},
        ActionKey, RegisteredAction,
    },
    artifact_groups::calculation::ArtifactGroupCalculation,
    deferred::calculation::DeferredCalculation,
    events::proto::ToProtoMessage,
    execute::{
        ActionError, ActionErrorCause, ActionExecutionMetadata, ActionOutputs, HasActionExecutor,
    },
    keep_going,
};

#[async_trait]
pub(crate) trait ActionCalculation {
    /// Returns the 'Action' corresponding to a particular 'ActionKey'.
    async fn get_action(&self, artifact: &ActionKey) -> SharedResult<Arc<RegisteredAction>>;

    /// Builds a specific 'Action' given the 'ActionKey'
    async fn build_action(&self, action_key: &ActionKey) -> SharedResult<ActionOutputs>;

    /// Builds and materializes the given 'BuildArtifact'
    async fn build_artifact(&self, artifact: &BuildArtifact) -> SharedResult<ActionOutputs>;
}

#[async_trait]
impl ActionCalculation for DiceComputations {
    async fn get_action(&self, action_key: &ActionKey) -> SharedResult<Arc<RegisteredAction>> {
        // TODO add async/deferred stuff
        self.compute_deferred_data(action_key)
            .await
            .map(|a| a.dupe())
            .with_context(|| format!("for action key `{}`", action_key))
            .shared_error()
    }

    async fn build_action(&self, action_key: &ActionKey) -> SharedResult<ActionOutputs> {
        // build_action is called for every action key.
        // We don't currently consume this in buck_e2e but it's good to log for debugging purposes.
        debug!("build_action {}", action_key);

        #[derive(Clone, Dupe, Display, Debug, Eq, PartialEq, Hash)]
        #[display(fmt = "Build({})", _0)]
        struct BuildKey(ActionKey);

        #[async_trait]
        impl Key for BuildKey {
            type Value = SharedResult<ActionOutputs>;

            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                // Compute is only called if we have cache miss
                debug!("compute {}", self.0);

                let action = ctx.get_action(&self.0).await?;

                if action.key() != &self.0 {
                    // The action key we start with is on the DICE graph, and thus cached
                    // and properly deduplicated. But if the underlying has a different key,
                    // e.g. due to dynamic_output, then we might have two different action keys
                    // pointing at the same underlying action. We need to make sure that
                    // underlying action only gets called once, so call build_action once
                    // again with the new key to get DICE deduplication.
                    return ctx.build_action(action.key()).await;
                }

                let materialized_inputs = tokio::task::unconstrained(keep_going::try_join_all(
                    action
                        .inputs()?
                        .iter()
                        .map(|a| {
                            let a = a.dupe();
                            async move {
                                let val = ctx.ensure_artifact_group(&a).await?;
                                SharedResult::Ok((a, val))
                            }
                        })
                        .collect::<FuturesUnordered<_>>(),
                ))
                .await?;

                // TODO check input based cache and compute keys
                let events = ctx.per_transaction_data().get_dispatcher();
                let build_signals = ctx.per_transaction_data().get_build_signals();
                let start_event = buck2_data::ActionExecutionStart {
                    key: Some(action.key().as_proto()),
                    kind: action.kind().into(),
                    name: Some(buck2_data::ActionName {
                        category: action.category().as_str().to_owned(),
                        identifier: action.identifier().unwrap_or("").to_owned(),
                    }),
                };

                let executor = ctx.get_action_executor(action.execution_config()).await?;

                // this can be RE
                events
                    .span_async(start_event, async move {
                        let execute_result = executor.execute(materialized_inputs, &action).await;

                        let action_result;
                        let success_stderr;
                        let execution_kind;
                        let wall_time;
                        let error;

                        match execute_result {
                            Ok((outputs, meta)) => {
                                if let Some(signals) = build_signals {
                                    signals.signal(ActionExecutionSignal {
                                        action: action.dupe(),
                                        duration: meta.timing.wall_time,
                                    });
                                }

                                action_result = Ok(outputs);
                                success_stderr = Some(meta.std_streams.to_lossy_stderr().await);
                                execution_kind = Some(meta.execution_kind.as_enum());
                                wall_time = Some(meta.timing.wall_time);
                                error = None;
                            }
                            Err(e) => {
                                // Because we already are sending the error message in the
                                // ActionExecutionEnd event, we slim the error down in the result.
                                // We can then unconditionally print the error message for compute(),
                                // including ones near the beginning of this method, and also not
                                // duplicate any error messages.
                                action_result = Err(anyhow::anyhow!(
                                    "Failed to build artifact(s) for '{}'",
                                    action.owner()
                                )
                                .into());
                                success_stderr = None;
                                execution_kind = e
                                    .downcast_ref::<ActionError>()
                                    .map(|e| e.metadata.execution_kind.as_enum());
                                wall_time = None;
                                error = Some(error_to_proto(&e).await);
                            }
                        };

                        (
                            action_result,
                            buck2_data::ActionExecutionEnd {
                                key: Some(action.key().as_proto()),
                                kind: action.kind().into(),
                                name: Some(buck2_data::ActionName {
                                    category: action.category().as_str().to_owned(),
                                    identifier: action.identifier().unwrap_or("").to_owned(),
                                }),
                                failed: error.is_some(),
                                error,
                                always_print_stderr: action.always_print_stderr(),
                                success_stderr: success_stderr.unwrap_or_default(),
                                wall_time: wall_time.map(Into::into),
                                execution_kind: execution_kind
                                    .unwrap_or(buck2_data::ActionExecutionKind::NotSet)
                                    as i32,
                            },
                        )
                    })
                    .await
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }

            fn validity(x: &Self::Value) -> bool {
                // we don't cache any kind of errors. Ideally, we could try to distinguish different
                // error types and try to cache non-transient error types, but practically there
                // are too many unknowns that may cause more harm than good if we cached errors.
                // So, don't cache it for now, until someday we decide to really need to.
                x.is_ok()
            }
        }

        self.compute(&BuildKey(action_key.dupe())).await
    }

    async fn build_artifact(&self, artifact: &BuildArtifact) -> SharedResult<ActionOutputs> {
        self.build_action(artifact.key()).await
    }
}

async fn error_to_proto(err: &anyhow::Error) -> buck2_data::action_execution_end::Error {
    match err.downcast_ref::<ActionError>() {
        // CommandFailed errors don't necessarily have an exit code; no exit code implies that some other failure
        // happened during action execution (e.g. RE problems) that prevented the command from being executed.
        //
        // They are not user errors, so they're listed as Unknown errors here.
        //
        // TODO (torozco): Those should use manager.error() and they won't be ActionError (just a
        // regular anyhow::Error). We should update the exit_code to come via ActionResultStatus
        // and have it only on Success and Failure, which would prove this branch cannot in fact
        // exist.
        Some(
            err @ ActionError {
                cause:
                    ActionErrorCause::CommandFailed {
                        exit_code: None, ..
                    },
                ..
            },
        ) => buck2_data::action_execution_end::Error::Unknown(format!("{:#}", err)),
        Some(ActionError {
            cause,
            metadata:
                ActionExecutionMetadata {
                    std_streams,
                    execution_kind,
                    timing: _,
                },
        }) => {
            // NOTE: This is a bit sketchy. We know that either we don't care about the exit code,
            // or that it's there and nonzero. A better representation would be to move the
            // exit_code to only be present on the CommandFailed variant, but that's a breaking
            // protobuf change for little benefit, so for now we don't do it.
            let exit_code = match cause {
                ActionErrorCause::CommandFailed { exit_code: Some(i) } => *i as u32,
                _ => 0,
            };

            let std_streams = std_streams.to_lossy().await;

            let command = buck2_data::action_execution_end::CommandExecutionDetails {
                exit_code,
                stdout: std_streams.stdout,
                stderr: std_streams.stderr,
                local_command: execution_kind.as_local_command(),
                remote_command: execution_kind.as_remote_command(),
            };

            match cause {
                outputs @ ActionErrorCause::MissingOutputs(..)
                | outputs @ ActionErrorCause::MismatchedOutputs { .. } => {
                    buck2_data::action_execution_end::Error::MissingOutputs(
                        buck2_data::action_execution_end::CommandOutputsMissing {
                            command: Some(command),
                            message: format!("{:#}", outputs),
                        },
                    )
                }
                timed_out @ ActionErrorCause::TimedOut { .. } => {
                    buck2_data::action_execution_end::Error::TimedOut(
                        buck2_data::action_execution_end::CommandTimedOut {
                            command: Some(command),
                            message: format!("{:#}", timed_out),
                        },
                    )
                }
                ActionErrorCause::CommandFailed { .. } => {
                    buck2_data::action_execution_end::Error::CommandFailed(command)
                }
            }
        }
        None => buck2_data::action_execution_end::Error::Unknown(format!("{:#}", err)),
    }
}

#[cfg(test)]
mod tests {
    use std::{
        path::PathBuf,
        sync::{Arc, Mutex},
    };

    use buck2_common::{
        dice::{
            cells::HasCellResolver, data::testing::SetTestingIoProvider,
            file_ops::testing::FileOpsKey,
        },
        file_ops::{
            testing::TestFileOps, ExternalSymlink, FileDigest, FileMetadata, TrackedFileDigest,
        },
    };
    use buck2_core::{
        category::Category,
        cells::{
            paths::{CellPath, CellRelativePathBuf},
            testing::CellResolverExt,
            CellName, CellResolver,
        },
        configuration::Configuration,
        directory::DirectoryEntry,
        fs::{
            paths::ForwardRelativePathBuf,
            project::{ProjectFilesystemTemp, ProjectRelativePathBuf},
        },
        package::{testing::PackageExt, Package, PackageRelativePathBuf},
        result::ToSharedResultExt,
        target::{testing::ConfiguredTargetLabelExt, ConfiguredTargetLabel, TargetName},
    };
    use dice::{testing::DiceBuilder, DiceTransaction, UserComputationData};
    use events::dispatch::EventDispatcher;
    use gazebo::prelude::*;
    use indexmap::indexset;

    use crate::{
        actions::{
            artifact::{
                testing::BuildArtifactTestingExt, Artifact, ArtifactValue, BuildArtifact,
                SourceArtifact,
            },
            calculation::ActionCalculation,
            directory::ActionDirectoryMember,
            run::knobs::RunActionKnobs,
            testing::SimpleAction,
            Action, RegisteredAction,
        },
        artifact_groups::{calculation::ArtifactGroupCalculation, ArtifactGroup},
        context::SetBuildContextData,
        deferred::{
            calculation::testing::DeferredResolve, testing::DeferredIdExt, AnyValue, DeferredId,
        },
        execute::{
            blocking::{testing::DummyBlockingExecutor, SetBlockingExecutor},
            commands::{
                dice_data::{
                    set_fallback_executor_config, CommandExecutorConfig, HasCommandExecutor,
                    SetCommandExecutor,
                },
                dry_run::{DryRunEntry, DryRunExecutor},
                PreparedCommandExecutor,
            },
            materializer::{nodisk::NoDiskMaterializer, SetMaterializer},
            ActionExecutorConfig,
        },
        path::BuckPath,
    };

    fn create_test_build_artifact(
        package_cell: &str,
        package_path: &str,
        target_name: &str,
    ) -> BuildArtifact {
        let configured_target_label = ConfiguredTargetLabel::testing_new(
            Package::testing_new(package_cell, package_path),
            TargetName::unchecked_new(target_name),
            Configuration::testing_new(),
        );
        let forward_relative_path_buf = ForwardRelativePathBuf::unchecked_new("bar.out".into());
        let deferred_id = DeferredId::testing_new(0);
        BuildArtifact::testing_new(
            configured_target_label,
            forward_relative_path_buf,
            deferred_id,
        )
    }

    fn create_test_source_artifact(
        package_cell: &str,
        package_path: &str,
        target_name: &str,
    ) -> SourceArtifact {
        SourceArtifact::new(BuckPath::new(
            Package::testing_new(package_cell, package_path),
            PackageRelativePathBuf::unchecked_new(target_name.into()),
        ))
    }

    fn registered_action(
        build_artifact: BuildArtifact,
        action: Box<dyn Action>,
    ) -> Arc<RegisteredAction> {
        let registered_action = RegisteredAction::new(
            build_artifact.key().dupe(),
            action,
            ActionExecutorConfig::testing_local(),
        );
        Arc::new(registered_action)
    }

    fn mock_deferred_resolution_calculation(
        dice_builder: DiceBuilder,
        deferred_resolve: DeferredResolve,
        registered_action_arc: Arc<RegisteredAction>,
    ) -> DiceBuilder {
        let an_any: Arc<dyn AnyValue + 'static> = Arc::new(registered_action_arc);
        dice_builder.mock_and_return(deferred_resolve, anyhow::Ok(an_any).shared_error())
    }

    fn make_default_dice_state(
        dry_run_tracker: Arc<Mutex<Vec<DryRunEntry>>>,
        temp_fs: &ProjectFilesystemTemp,
        mocks: Vec<Box<dyn FnOnce(DiceBuilder) -> DiceBuilder>>,
    ) -> DiceTransaction {
        let fs = temp_fs.path().clone();

        let cell_resolver = CellResolver::of_names_and_paths(&[(
            CellName::unchecked_new("cell".into()),
            ProjectRelativePathBuf::unchecked_new("cell-path".into()),
        )]);
        let output_path = ProjectRelativePathBuf::unchecked_new("buck-out/v2".into());

        let mut dice_builder = DiceBuilder::new();
        dice_builder = dice_builder.set_data(|data| data.set_testing_io_provider(temp_fs));

        for mock in mocks.into_iter() {
            dice_builder = mock(dice_builder);
        }

        let mut extra = UserComputationData::new();
        struct CommandExecutorProvider {
            dry_run_tracker: Arc<Mutex<Vec<DryRunEntry>>>,
        }
        impl HasCommandExecutor for CommandExecutorProvider {
            fn get_command_executor(
                &self,
                config: &CommandExecutorConfig,
            ) -> anyhow::Result<Arc<dyn PreparedCommandExecutor>> {
                Ok(Arc::new(DryRunExecutor::new(
                    self.dry_run_tracker.dupe(),
                    Some(config.artifact_fs.clone()),
                )))
            }
        }

        set_fallback_executor_config(&mut extra.data, ActionExecutorConfig::testing_local());
        extra.set_command_executor(box CommandExecutorProvider { dry_run_tracker });
        extra.set_blocking_executor(Arc::new(DummyBlockingExecutor { fs }));
        extra.set_materializer(Arc::new(NoDiskMaterializer));
        extra.data.set(EventDispatcher::null());
        extra.data.set(RunActionKnobs::default());

        let computations = dice_builder.build(extra);
        computations.set_buck_out_path(Some(output_path.into()));
        computations.set_cell_resolver(cell_resolver);

        computations.commit()
    }

    #[tokio::test]
    async fn test_get_action_for_artifact() -> anyhow::Result<()> {
        let build_artifact = create_test_build_artifact("cell", "pkg", "foo");
        let deferred_resolve = DeferredResolve(build_artifact.key().deferred_key().dupe());
        let registered_action = registered_action(
            build_artifact.dupe(),
            box SimpleAction::new(
                indexset![],
                indexset![build_artifact.dupe()],
                vec![],
                vec![],
                Category::try_from("fake_action").unwrap(),
                None,
            ),
        );

        let mut dice_builder = DiceBuilder::new();
        dice_builder = mock_deferred_resolution_calculation(
            dice_builder,
            deferred_resolve,
            registered_action.dupe(),
        );
        let dice_computations = dice_builder.build(UserComputationData::new());

        let result = dice_computations.get_action(build_artifact.key()).await;
        assert_eq!(result?, registered_action);
        Ok(())
    }

    #[tokio::test]
    async fn test_build_action() -> anyhow::Result<()> {
        let temp_fs = ProjectFilesystemTemp::new()?;
        let build_artifact = create_test_build_artifact("cell", "pkg", "foo");
        let deferred_resolve = DeferredResolve(build_artifact.key().deferred_key().dupe());
        let registered_action = registered_action(
            build_artifact.dupe(),
            box SimpleAction::new(
                indexset![],
                indexset![build_artifact.dupe()],
                vec!["foo".to_owned(), "cmd".to_owned()],
                vec![],
                Category::try_from("fake_action").unwrap(),
                None,
            ),
        );

        let dry_run_tracker = Arc::new(Mutex::new(vec![]));
        let dice_computations = make_default_dice_state(
            dry_run_tracker.dupe(),
            &temp_fs,
            vec![{
                let action = registered_action.dupe();
                box move |builder| {
                    mock_deferred_resolution_calculation(builder, deferred_resolve, action)
                }
            }],
        );

        let result = dice_computations
            .build_action(registered_action.key())
            .await;

        assert!(result.is_ok());

        assert_eq!(
            dry_run_tracker.lock().unwrap()[0],
            DryRunEntry {
                args: vec!["foo".to_owned(), "cmd".to_owned()],
                inputs: hashset![],
                outputs: vec![build_artifact.into()],
                env: hashmap![]
            }
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_build_artifact() -> anyhow::Result<()> {
        let temp_fs = ProjectFilesystemTemp::new()?;
        let build_artifact = create_test_build_artifact("cell", "pkg", "foo");
        let deferred_resolve = DeferredResolve(build_artifact.key().deferred_key().dupe());
        let registered_action = registered_action(
            build_artifact.dupe(),
            box SimpleAction::new(
                indexset![],
                indexset![build_artifact.dupe()],
                vec!["bar".to_owned(), "cmd".to_owned()],
                vec![],
                Category::try_from("fake_action").unwrap(),
                None,
            ),
        );

        let dry_run_tracker = Arc::new(Mutex::new(vec![]));
        let dice_computations = make_default_dice_state(dry_run_tracker.dupe(), &temp_fs, {
            let registered_action = registered_action.dupe();
            vec![box move |builder| {
                mock_deferred_resolution_calculation(builder, deferred_resolve, registered_action)
            }]
        });

        let result = dice_computations.build_artifact(&build_artifact).await;

        assert!(result.is_ok());

        assert_eq!(
            dry_run_tracker.lock().unwrap()[0],
            DryRunEntry {
                args: vec!["bar".to_owned(), "cmd".to_owned()],
                inputs: hashset![],
                outputs: vec![build_artifact.into()],
                env: hashmap![]
            }
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_ensure_artifact_build_artifact() -> anyhow::Result<()> {
        let temp_fs = ProjectFilesystemTemp::new()?;
        let build_artifact = create_test_build_artifact("cell", "pkg", "foo");
        let deferred_resolve = DeferredResolve(build_artifact.key().deferred_key().dupe());
        let registered_action = registered_action(
            build_artifact.dupe(),
            box SimpleAction::new(
                indexset![],
                indexset![build_artifact.dupe()],
                vec!["ensure".to_owned(), "cmd".to_owned()],
                vec![],
                Category::try_from("fake_action").unwrap(),
                None,
            ),
        );

        let dry_run_tracker = Arc::new(Mutex::new(vec![]));
        let dice_computations = make_default_dice_state(dry_run_tracker.dupe(), &temp_fs, {
            let registered_action = registered_action.dupe();
            vec![box move |builder| {
                mock_deferred_resolution_calculation(builder, deferred_resolve, registered_action)
            }]
        });

        let result = dice_computations
            .ensure_artifact_group(&ArtifactGroup::Artifact(build_artifact.dupe().into()))
            .await;

        assert!(result.is_ok());

        assert_eq!(
            dry_run_tracker.lock().unwrap()[0],
            DryRunEntry {
                args: vec!["ensure".to_owned(), "cmd".to_owned()],
                inputs: hashset![],
                outputs: vec![build_artifact.into()],
                env: hashmap![]
            }
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_ensure_artifact_source_artifact() -> anyhow::Result<()> {
        let path = CellPath::new(
            CellName::unchecked_new("cell".to_owned()),
            CellRelativePathBuf::unchecked_new("pkg/src.cpp".to_owned()),
        );
        let source_artifact = create_test_source_artifact("cell", "pkg", "src.cpp");
        let metadata = FileMetadata {
            digest: TrackedFileDigest::new(FileDigest::from_bytes("content".as_bytes())),
            is_executable: true,
        };

        let dice_builder = DiceBuilder::new();
        let dice_computations = dice_builder
            .mock_and_return(
                FileOpsKey(),
                Ok(Arc::new(TestFileOps::new_with_files_metadata(
                    btreemap![path => metadata.dupe()],
                ))),
            )
            .build(UserComputationData::new());

        let source_artifact = Artifact::from(source_artifact);
        let input = ArtifactGroup::Artifact(source_artifact.dupe());
        let result = dice_computations
            .ensure_artifact_group(&input)
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
            CellName::unchecked_new("cell".to_owned()),
            CellRelativePathBuf::unchecked_new("proj/to_gvfs/include".to_owned()),
        );
        let source_artifact = create_test_source_artifact("cell", "proj/to_gvfs", "include");
        let symlink = Arc::new(ExternalSymlink::new(
            PathBuf::from("/mnt/gvfs"),
            Some(ForwardRelativePathBuf::unchecked_new("include".to_owned())),
        ));

        let dice_builder = DiceBuilder::new();
        let dice_computations = dice_builder
            .mock_and_return(
                FileOpsKey(),
                Ok(Arc::new(TestFileOps::new_with_symlinks(
                    btreemap![path => symlink.dupe()],
                ))),
            )
            .build(UserComputationData::new());

        let source_artifact = Artifact::from(source_artifact);
        let input = ArtifactGroup::Artifact(source_artifact.dupe());
        let result = dice_computations
            .ensure_artifact_group(&input)
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
}
