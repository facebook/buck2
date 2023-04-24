/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter::zip;
use std::sync::Arc;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::current_span;
use buck2_events::dispatch::span_async;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::result::CommandExecutionReport;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::output_size::OutputSize;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::future;
use futures::stream::FuturesOrdered;
use futures::Future;
use futures::FutureExt;
use indexmap::IndexMap;
use more_futures::cancellation::CancellationContext;
use ref_cast::RefCast;
use tracing::debug;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::build_listener::ActionExecutionSignal;
use crate::actions::build_listener::ActionRedirectionSignal;
use crate::actions::build_listener::HasBuildSignals;
use crate::actions::build_listener::NodeDuration;
use crate::actions::execute::action_executor::ActionOutputs;
use crate::actions::execute::action_executor::HasActionExecutor;
use crate::actions::key::ActionKey;
use crate::actions::RegisteredAction;
use crate::artifact_groups::calculation::ensure_artifact_group_staged;
use crate::deferred::calculation::DeferredCalculation;
use crate::keep_going;

pub struct ActionCalculation;

async fn build_action_impl(
    ctx: &DiceComputations,
    cancellation: &CancellationContext,
    key: &ActionKey,
) -> anyhow::Result<ActionOutputs> {
    // Compute is only called if we have cache miss
    debug!("compute {}", key);

    let action = ActionCalculation::get_action(ctx, key).await?;

    if action.key() != key {
        // The action key we start with is on the DICE graph, and thus cached
        // and properly deduplicated. But if the underlying has a different key,
        // e.g. due to dynamic_output, then we might have two different action keys
        // pointing at the same underlying action. We need to make sure that
        // underlying action only gets called once, so call build_action once
        // again with the new key to get DICE deduplication.
        let res = ActionCalculation::build_action(ctx, action.key()).await;

        if let Some(signals) = ctx.per_transaction_data().get_build_signals() {
            // Notify our critical path tracking that *this action* is secretly that
            // other action we just jumped to.
            signals.signal(ActionRedirectionSignal {
                key: key.dupe(),
                dest: action.key().dupe(),
            });
        }

        return res;
    }

    build_action_no_redirect(ctx, cancellation, action).await
}

async fn build_action_no_redirect(
    ctx: &DiceComputations,
    cancellation: &CancellationContext,
    action: Arc<RegisteredAction>,
) -> anyhow::Result<ActionOutputs> {
    let materialized_inputs = {
        let inputs = action.inputs()?;
        let ensure_futs: FuturesOrdered<_> = inputs
            .iter()
            .map(|v| ensure_artifact_group_staged(ctx, v))
            .collect();

        let ready_inputs: Vec<_> =
            tokio::task::unconstrained(keep_going::try_join_all(ensure_futs)).await?;

        let mut results = IndexMap::with_capacity(inputs.len());
        for (artifact, ready) in zip(inputs.iter(), ready_inputs.into_iter()) {
            results.insert(artifact.clone(), ready.to_group_values(artifact)?);
        }
        results
    };

    let start_event = buck2_data::ActionExecutionStart {
        key: Some(action.key().as_proto()),
        kind: action.kind().into(),
        name: Some(buck2_data::ActionName {
            category: action.category().as_str().to_owned(),
            identifier: action.identifier().unwrap_or("").to_owned(),
        }),
    };

    let executor = ctx
        .get_action_executor(action.execution_config())
        .await
        .context(format!("for action `{}`", action))?;

    let now = Instant::now();

    let fut = async move {
        let (execute_result, command_reports) = executor
            .execute(materialized_inputs, &action, cancellation)
            .await;

        let allow_omit_details = execute_result.is_ok();

        let commands = future::join_all(
            command_reports
                .iter()
                .map(|r| command_execution_report_to_proto(r, allow_omit_details)),
        )
        .await;

        let action_result;
        let execution_kind;
        let wall_time;
        let error;
        let output_size;

        let mut prefers_local = None;
        let mut requires_local = None;
        let mut allows_cache_upload = None;
        let mut did_cache_upload = None;
        let mut eligible_for_full_hybrid = None;

        let mut buck2_revision = None;
        let mut buck2_build_time = None;

        match execute_result {
            Ok((outputs, meta)) => {
                if let Some(signals) = ctx.per_transaction_data().get_build_signals() {
                    signals.signal(ActionExecutionSignal {
                        action: action.dupe(),
                        duration: NodeDuration {
                            user: meta.timing.wall_time,
                            total: now.elapsed(),
                        },
                        span_id: current_span(),
                    });
                }

                output_size = outputs.calc_output_count_and_bytes().bytes;
                action_result = Ok(outputs);
                execution_kind = Some(meta.execution_kind.as_enum());
                wall_time = Some(meta.timing.wall_time);
                error = None;

                if let Some(command) = meta.execution_kind.command() {
                    prefers_local = Some(command.prefers_local);
                    requires_local = Some(command.requires_local);
                    allows_cache_upload = Some(command.allows_cache_upload);
                    did_cache_upload = Some(command.did_cache_upload);
                    eligible_for_full_hybrid = Some(command.eligible_for_full_hybrid);
                }
            }
            Err(e) => {
                // Because we already are sending the error message in the
                // ActionExecutionEnd event, we slim the error down in the result.
                // We can then unconditionally print the error message for compute(),
                // including ones near the beginning of this method, and also not
                // duplicate any error messages.
                action_result = Err(anyhow::anyhow!("Failed to build '{}'", action.owner()));
                // TODO (torozco): Remove (see protobuf file)?
                execution_kind = command_reports
                    .last()
                    .and_then(|r| r.status.execution_kind())
                    .map(|e| e.as_enum());
                wall_time = None;
                error = Some(e.as_proto());
                output_size = 0;
                // We define the below fields only in the instance of an action error
                // so as to reduce Scribe traffic and log it in buck2_action_errors
                buck2_revision = buck2_build_info::revision().map(|s| s.to_owned());
                buck2_build_time = buck2_build_info::time_iso8601().map(|s| s.to_owned());
            }
        };

        let outputs = action_result
            .as_ref()
            .map(|outputs| {
                outputs
                    .iter()
                    .filter_map(|(_artifact, value)| {
                        Some(buck2_data::ActionOutput {
                            tiny_digest: value.digest()?.tiny_digest().to_string(),
                        })
                    })
                    .collect()
            })
            .unwrap_or_default();

        (
            action_result,
            Box::new(buck2_data::ActionExecutionEnd {
                key: Some(action.key().as_proto()),
                kind: action.kind().into(),
                name: Some(buck2_data::ActionName {
                    category: action.category().as_str().to_owned(),
                    identifier: action.identifier().unwrap_or("").to_owned(),
                }),
                failed: error.is_some(),
                error,
                always_print_stderr: action.always_print_stderr(),
                wall_time: wall_time.and_then(|d| d.try_into().ok()),
                execution_kind: execution_kind.unwrap_or(buck2_data::ActionExecutionKind::NotSet)
                    as i32,
                output_size,
                commands,
                outputs,
                prefers_local: prefers_local.unwrap_or_default(),
                requires_local: requires_local.unwrap_or_default(),
                allows_cache_upload: allows_cache_upload.unwrap_or_default(),
                did_cache_upload: did_cache_upload.unwrap_or_default(),
                eligible_for_full_hybrid,
                buck2_revision,
                buck2_build_time,
            }),
        )
    };
    // boxed() the future so that we don't need to allocate space for it while waiting on input dependencies.
    span_async(start_event, fut.boxed()).await
}

/// The cost of these calls are particularly critical. To control the cost (particularly size) of these calls
/// we drop the `async_trait` common in other `*Calculation` types and avoid `async fn` (for
/// build_action/build_artifact at least).
impl ActionCalculation {
    pub(crate) async fn get_action(
        ctx: &DiceComputations,
        action_key: &ActionKey,
    ) -> anyhow::Result<Arc<RegisteredAction>> {
        // TODO add async/deferred stuff
        ctx.compute_deferred_data(action_key.deferred_data())
            .await
            .map(|a| (*a).dupe())
            .with_context(|| format!("for action key `{}`", action_key))
    }

    pub(crate) fn build_action<'a>(
        ctx: &'a DiceComputations,
        action_key: &'a ActionKey,
    ) -> impl Future<Output = anyhow::Result<ActionOutputs>> + 'a {
        // build_action is called for every action key. We don't use `async fn` to ensure that it has minimal cost.
        // We don't currently consume this in buck_e2e but it's good to log for debugging purposes.
        debug!("build_action {}", action_key);
        ctx.compute(BuildKey::ref_cast(action_key))
            .map(|v| v?.unshared_error())
    }

    pub(crate) fn build_artifact<'a>(
        ctx: &'a DiceComputations,
        artifact: &'a BuildArtifact,
    ) -> impl Future<Output = anyhow::Result<ActionOutputs>> + 'a {
        Self::build_action(ctx, artifact.key())
    }
}

#[derive(Clone, Dupe, Display, Debug, Eq, PartialEq, Hash, Allocative, RefCast)]
#[repr(transparent)]
struct BuildKey(ActionKey);

#[async_trait]
impl Key for BuildKey {
    type Value = SharedResult<ActionOutputs>;

    async fn compute(
        &self,
        ctx: &DiceComputations,
        cancellation: &CancellationContext,
    ) -> Self::Value {
        build_action_impl(ctx, cancellation, &self.0)
            .await
            .shared_error()
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

async fn command_execution_report_to_proto(
    report: &CommandExecutionReport,
    allow_omit_details: bool,
) -> buck2_data::CommandExecution {
    let details = command_details(report, allow_omit_details).await;

    let status = match &report.status {
        CommandExecutionStatus::Success { .. } => buck2_data::command_execution::Success {}.into(),
        CommandExecutionStatus::Cancelled => buck2_data::command_execution::Cancelled {}.into(),
        CommandExecutionStatus::Failure { .. } => buck2_data::command_execution::Failure {}.into(),
        CommandExecutionStatus::TimedOut { duration, .. } => {
            buck2_data::command_execution::Timeout {
                duration: (*duration).try_into().ok(),
            }
            .into()
        }
        CommandExecutionStatus::Error { stage, error } => buck2_data::command_execution::Error {
            stage: (*stage).to_owned(),
            error: format!("{:#}", error),
        }
        .into(),
    };

    buck2_data::CommandExecution {
        details: Some(details),
        status: Some(status),
    }
}

async fn command_details(
    command: &CommandExecutionReport,
    allow_omit_details: bool,
) -> buck2_data::CommandExecutionDetails {
    // If the top-level command failed then we don't want to omit any details. If it succeeded and
    // so did this command (it could succeed while not having a success here if we have rejected
    // executions), then we'll strip non-relevant stuff.
    let omit_details =
        allow_omit_details && matches!(command.status, CommandExecutionStatus::Success { .. });

    let signed_exit_code = command.exit_code;

    let stdout;
    let stderr;

    if omit_details {
        stdout = Default::default();
        stderr = command.std_streams.to_lossy_stderr().await;
    } else {
        let pair = command.std_streams.to_lossy().await;
        stdout = pair.stdout;
        stderr = pair.stderr;
    };

    let command_data = command.status.execution_kind().map(|kind| match kind {
        CommandExecutionKind::Local {
            command,
            env,
            digest,
        } => {
            if omit_details {
                buck2_data::OmittedLocalCommand {
                    action_digest: digest.to_string(),
                }
                .into()
            } else {
                buck2_data::LocalCommand {
                    action_digest: digest.to_string(),
                    argv: command.to_owned(),
                    env: env
                        .iter()
                        .map(|(key, value)| buck2_data::local_command::EnvironmentEntry {
                            key: key.clone(),
                            value: value.clone(),
                        })
                        .collect(),
                }
                .into()
            }
        }
        CommandExecutionKind::Remote { digest } => buck2_data::RemoteCommand {
            action_digest: digest.to_string(),
            cache_hit: false,
            queue_time: command.timing.re_queue_time.and_then(|d| d.try_into().ok()),
        }
        .into(),
        CommandExecutionKind::ActionCache { digest } => buck2_data::RemoteCommand {
            action_digest: digest.to_string(),
            cache_hit: true,
            queue_time: command.timing.re_queue_time.and_then(|d| d.try_into().ok()),
        }
        .into(),
    });

    buck2_data::CommandExecutionDetails {
        exit_code: signed_exit_code.and_then(|e| e.try_into().ok()), // To be deprecated in favor of signed_exit_code
        stdout,
        stderr,
        command: command_data,
        signed_exit_code,
        execution_stats: command.timing.execution_stats,
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::Arc;
    use std::sync::Mutex;

    use assert_matches::assert_matches;
    use buck2_common::dice::cells::SetCellResolver;
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_common::dice::file_ops::keys::FileOpsValue;
    use buck2_common::dice::file_ops::testing::FileOpsKey;
    use buck2_common::executor_config::CommandExecutorConfig;
    use buck2_common::external_symlink::ExternalSymlink;
    use buck2_common::file_ops::testing::TestFileOps;
    use buck2_common::file_ops::FileMetadata;
    use buck2_common::file_ops::TrackedFileDigest;
    use buck2_common::result::ToSharedResultExt;
    use buck2_core::buck_path::path::BuckPath;
    use buck2_core::category::Category;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::paths::CellRelativePathBuf;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::directory::DirectoryEntry;
    use buck2_core::fs::artifact_path_resolver::ArtifactFs;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::PackageLabel;
    use buck2_core::target::label::TargetLabel;
    use buck2_core::target::name::TargetNameRef;
    use buck2_events::dispatch::with_dispatcher_async;
    use buck2_events::dispatch::EventDispatcher;
    use buck2_execute::artifact_value::ArtifactValue;
    use buck2_execute::digest_config::DigestConfig;
    use buck2_execute::digest_config::SetDigestConfig;
    use buck2_execute::directory::ActionDirectoryMember;
    use buck2_execute::execute::action_digest::ActionDigest;
    use buck2_execute::execute::blocking::testing::DummyBlockingExecutor;
    use buck2_execute::execute::blocking::SetBlockingExecutor;
    use buck2_execute::execute::dice_data::set_fallback_executor_config;
    use buck2_execute::execute::dice_data::CommandExecutorResponse;
    use buck2_execute::execute::dice_data::HasCommandExecutor;
    use buck2_execute::execute::dice_data::SetCommandExecutor;
    use buck2_execute::execute::dice_data::SetReClient;
    use buck2_execute::execute::kind::CommandExecutionKind;
    use buck2_execute::execute::output::CommandStdStreams;
    use buck2_execute::execute::request::CommandExecutionOutput;
    use buck2_execute::execute::request::OutputType;
    use buck2_execute::execute::result::CommandExecutionReport;
    use buck2_execute::execute::result::CommandExecutionStatus;
    use buck2_execute::execute::testing_dry_run::DryRunEntry;
    use buck2_execute::execute::testing_dry_run::DryRunExecutor;
    use buck2_execute::materialize::materializer::SetMaterializer;
    use buck2_execute::materialize::nodisk::NoDiskMaterializer;
    use buck2_execute::re::manager::ManagedRemoteExecutionClient;
    use dice::testing::DiceBuilder;
    use dice::DiceTransaction;
    use dice::UserComputationData;
    use dupe::Dupe;
    use indexmap::indexset;
    use maplit::btreemap;
    use sorted_vector_map::sorted_vector_map;

    use crate::actions::artifact::artifact_type::testing::BuildArtifactTestingExt;
    use crate::actions::artifact::artifact_type::Artifact;
    use crate::actions::artifact::build_artifact::BuildArtifact;
    use crate::actions::artifact::source_artifact::SourceArtifact;
    use crate::actions::calculation::command_details;
    use crate::actions::calculation::ActionCalculation;
    use crate::actions::impls::run_action_knobs::RunActionKnobs;
    use crate::actions::testings::SimpleAction;
    use crate::actions::Action;
    use crate::actions::RegisteredAction;
    use crate::artifact_groups::calculation::ArtifactGroupCalculation;
    use crate::artifact_groups::ArtifactGroup;
    use crate::context::SetBuildContextData;
    use crate::deferred::calculation::testing::DeferredResolve;
    use crate::deferred::types::testing::DeferredIdExt;
    use crate::deferred::types::AnyValue;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredValueAnyReady;
    use crate::spawner::BuckSpawner;

    fn create_test_build_artifact(
        package_cell: &str,
        package_path: &str,
        target_name: &str,
    ) -> BuildArtifact {
        let configured_target_label = TargetLabel::new(
            PackageLabel::testing_new(package_cell, package_path),
            TargetNameRef::unchecked_new(target_name),
        )
        .configure(ConfigurationData::testing_new());
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
        SourceArtifact::new(BuckPath::testing_new(
            PackageLabel::testing_new(package_cell, package_path),
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
            CommandExecutorConfig::testing_local(),
        );
        Arc::new(registered_action)
    }

    fn mock_deferred_resolution_calculation(
        dice_builder: DiceBuilder,
        deferred_resolve: DeferredResolve,
        registered_action_arc: Arc<RegisteredAction>,
    ) -> DiceBuilder {
        let arc_any: Arc<dyn AnyValue + 'static> = Arc::new(registered_action_arc);
        let an_any = DeferredValueAnyReady::AnyValue(arc_any);
        dice_builder.mock_and_return(deferred_resolve, anyhow::Ok(an_any).shared_error())
    }

    async fn make_default_dice_state(
        dry_run_tracker: Arc<Mutex<Vec<DryRunEntry>>>,
        temp_fs: &ProjectRootTemp,
        mocks: Vec<Box<dyn FnOnce(DiceBuilder) -> DiceBuilder>>,
    ) -> anyhow::Result<DiceTransaction> {
        let fs = temp_fs.path().dupe();

        let cell_resolver = CellResolver::of_names_and_paths(
            CellName::testing_new("root"),
            CellName::testing_new("cell"),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell-path".into())),
        );
        let output_path = ProjectRelativePathBuf::unchecked_new("buck-out/v2".into());

        let mut dice_builder = DiceBuilder::new();
        dice_builder = dice_builder.set_data(|data| {
            data.set_testing_io_provider(temp_fs);
            data.set_digest_config(DigestConfig::testing_default());
        });

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
                artifact_fs: &ArtifactFs,
                _config: &CommandExecutorConfig,
            ) -> anyhow::Result<CommandExecutorResponse> {
                let executor = Arc::new(DryRunExecutor::new(
                    self.dry_run_tracker.dupe(),
                    artifact_fs.clone(),
                ));
                Ok(CommandExecutorResponse {
                    executor,
                    platform: Default::default(),
                })
            }
        }

        set_fallback_executor_config(&mut extra.data, CommandExecutorConfig::testing_local());
        extra.set_command_executor(Box::new(CommandExecutorProvider { dry_run_tracker }));
        extra.set_blocking_executor(Arc::new(DummyBlockingExecutor { fs }));
        extra.set_materializer(Arc::new(NoDiskMaterializer));
        extra.set_re_client(ManagedRemoteExecutionClient::testing_new_dummy());
        extra.data.set(EventDispatcher::null());
        extra.data.set(RunActionKnobs::default());
        extra.spawner = Arc::new(BuckSpawner::default());

        let mut computations = dice_builder.build(extra)?;
        computations.set_buck_out_path(Some(output_path))?;
        computations.set_cell_resolver(cell_resolver)?;

        Ok(computations.commit().await)
    }

    #[tokio::test]
    async fn test_get_action_for_artifact() -> anyhow::Result<()> {
        let build_artifact = create_test_build_artifact("cell", "pkg", "foo");
        let deferred_resolve = DeferredResolve(build_artifact.key().deferred_key().dupe());
        let registered_action = registered_action(
            build_artifact.dupe(),
            Box::new(SimpleAction::new(
                indexset![],
                indexset![build_artifact.dupe()],
                vec![],
                Category::try_from("fake_action").unwrap(),
                None,
            )),
        );

        let mut dice_builder = DiceBuilder::new();
        dice_builder = mock_deferred_resolution_calculation(
            dice_builder,
            deferred_resolve,
            registered_action.dupe(),
        );
        let dice_computations = dice_builder
            .build(UserComputationData::new())?
            .commit()
            .await;

        let result = with_dispatcher_async(
            EventDispatcher::null(),
            ActionCalculation::get_action(&dice_computations, build_artifact.key()),
        )
        .await;
        assert_eq!(result?, registered_action);
        Ok(())
    }

    #[tokio::test]
    async fn test_build_action() -> anyhow::Result<()> {
        let temp_fs = ProjectRootTemp::new()?;
        let build_artifact = create_test_build_artifact("cell", "pkg", "foo");
        let deferred_resolve = DeferredResolve(build_artifact.key().deferred_key().dupe());
        let registered_action = registered_action(
            build_artifact.dupe(),
            Box::new(SimpleAction::new(
                indexset![],
                indexset![build_artifact.dupe()],
                vec!["foo".to_owned(), "cmd".to_owned()],
                Category::try_from("fake_action").unwrap(),
                None,
            )),
        );

        let dry_run_tracker = Arc::new(Mutex::new(vec![]));
        let dice_computations = make_default_dice_state(
            dry_run_tracker.dupe(),
            &temp_fs,
            vec![{
                let action = registered_action.dupe();
                Box::new(move |builder| {
                    mock_deferred_resolution_calculation(builder, deferred_resolve, action)
                })
            }],
        )
        .await?;

        let result =
            ActionCalculation::build_action(&dice_computations, registered_action.key()).await;

        assert!(result.is_ok());

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
        let build_artifact = create_test_build_artifact("cell", "pkg", "foo");
        let deferred_resolve = DeferredResolve(build_artifact.key().deferred_key().dupe());
        let registered_action = registered_action(
            build_artifact.dupe(),
            Box::new(SimpleAction::new(
                indexset![],
                indexset![build_artifact.dupe()],
                vec!["bar".to_owned(), "cmd".to_owned()],
                Category::try_from("fake_action").unwrap(),
                None,
            )),
        );

        let dry_run_tracker = Arc::new(Mutex::new(vec![]));
        let dice_computations = make_default_dice_state(dry_run_tracker.dupe(), &temp_fs, {
            let registered_action = registered_action.dupe();
            vec![Box::new(move |builder| {
                mock_deferred_resolution_calculation(builder, deferred_resolve, registered_action)
            })]
        })
        .await?;

        let result = with_dispatcher_async(
            EventDispatcher::null(),
            ActionCalculation::build_artifact(&dice_computations, &build_artifact),
        )
        .await;

        assert!(result.is_ok());

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
        let build_artifact = create_test_build_artifact("cell", "pkg", "foo");
        let deferred_resolve = DeferredResolve(build_artifact.key().deferred_key().dupe());
        let registered_action = registered_action(
            build_artifact.dupe(),
            Box::new(SimpleAction::new(
                indexset![],
                indexset![build_artifact.dupe()],
                vec!["ensure".to_owned(), "cmd".to_owned()],
                Category::try_from("fake_action").unwrap(),
                None,
            )),
        );

        let dry_run_tracker = Arc::new(Mutex::new(vec![]));
        let dice_computations = make_default_dice_state(dry_run_tracker.dupe(), &temp_fs, {
            let registered_action = registered_action.dupe();
            vec![Box::new(move |builder| {
                mock_deferred_resolution_calculation(builder, deferred_resolve, registered_action)
            })]
        })
        .await?;

        let result = with_dispatcher_async(
            EventDispatcher::null(),
            dice_computations
                .ensure_artifact_group(&ArtifactGroup::Artifact(build_artifact.dupe().into())),
        )
        .await;

        assert!(result.is_ok());

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
        let source_artifact = create_test_source_artifact("cell", "pkg", "src.cpp");
        let metadata = FileMetadata {
            digest: TrackedFileDigest::from_content(b"content", digest_config.cas_digest_config()),
            is_executable: true,
        };

        let dice_builder = DiceBuilder::new().set_data(|data| {
            data.set_digest_config(DigestConfig::testing_default());
        });
        let dice_computations = dice_builder
            .mock_and_return(
                FileOpsKey(),
                Ok(FileOpsValue(Arc::new(
                    TestFileOps::new_with_files_metadata(btreemap![path => metadata.dupe()]),
                ))),
            )
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
        let source_artifact = create_test_source_artifact("cell", "proj/to_gvfs", "include");
        let symlink = Arc::new(
            ExternalSymlink::new(
                PathBuf::from("/mnt/gvfs"),
                Some(ForwardRelativePathBuf::unchecked_new("include".to_owned())),
            )
            .unwrap(),
        );

        let dice_builder = DiceBuilder::new().set_data(|data| {
            data.set_digest_config(DigestConfig::testing_default());
        });
        let dice_computations = dice_builder
            .mock_and_return(
                FileOpsKey(),
                Ok(FileOpsValue(Arc::new(TestFileOps::new_with_symlinks(
                    btreemap![path => symlink.dupe()],
                )))),
            )
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
        use buck2_data::command_execution_details::Command;

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
        };

        let proto = command_details(&report, false).await;
        assert_matches!(proto.command, Some(Command::LocalCommand(..)));
        assert_eq!(&proto.stdout, "stdout");
        assert_eq!(&proto.stderr, "stderr");

        let proto = command_details(&report, true).await;
        assert_matches!(proto.command, Some(Command::OmittedLocalCommand(..)));
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
        assert_matches!(proto.command, Some(Command::LocalCommand(..)));
        assert_eq!(&proto.stdout, "stdout");
        assert_eq!(&proto.stderr, "stderr");
    }
}
