/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the `TestOrchestrator` from `buck2_test_api`.

use std::collections::HashMap;
use std::ffi::OsStr;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::actions::impls::run_action_knobs::HasRunActionKnobs;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::artifact_groups::calculation::ArtifactGroupCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::ExternalRunnerTestInfoCallable;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::FrozenExternalRunnerTestInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::TestCommandMember;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::events::HasEvents;
use buck2_common::executor_config::CommandExecutorConfig;
use buck2_common::executor_config::CommandGenerationOptions;
use buck2_common::executor_config::Executor;
use buck2_common::executor_config::LocalExecutorOptions;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_common::local_resource_state::LocalResourceState;
use buck2_common::result::SharedError;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutTestPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_data::SetupLocalResourcesEnd;
use buck2_data::SetupLocalResourcesStart;
use buck2_data::TestDiscovery;
use buck2_data::TestDiscoveryEnd;
use buck2_data::TestDiscoveryStart;
use buck2_data::TestRunEnd;
use buck2_data::TestRunStart;
use buck2_data::TestSessionInfo;
use buck2_data::TestSuite;
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::execute::blocking::HasBlockingExecutor;
use buck2_execute::execute::claim::MutexClaimManager;
use buck2_execute::execute::command_executor::CommandExecutor;
use buck2_execute::execute::dice_data::CommandExecutorResponse;
use buck2_execute::execute::dice_data::HasCommandExecutor;
use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::prepared::NoOpCommandExecutor;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::request::CommandExecutionInput;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::request::OutputCreationBehavior;
use buck2_execute::execute::result::CommandExecutionMetadata;
use buck2_execute::execute::result::CommandExecutionReport;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::execute::target::CommandExecutionTarget;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_execute_impl::executors::local::apply_local_execution_environment;
use buck2_execute_impl::executors::local::create_output_dirs;
use buck2_execute_impl::executors::local::materialize_inputs;
use buck2_execute_impl::executors::local::EnvironmentBuilder;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_test_api::data::ArgValue;
use buck2_test_api::data::ArgValueContent;
use buck2_test_api::data::ConfiguredTargetHandle;
use buck2_test_api::data::DeclaredOutput;
use buck2_test_api::data::DisplayMetadata;
use buck2_test_api::data::ExecuteResponse;
use buck2_test_api::data::ExecutionResult2;
use buck2_test_api::data::ExecutionStatus;
use buck2_test_api::data::ExecutionStream;
use buck2_test_api::data::ExecutorConfigOverride;
use buck2_test_api::data::ExternalRunnerSpecValue;
use buck2_test_api::data::Output;
use buck2_test_api::data::PrepareForLocalExecutionResult;
use buck2_test_api::data::RequiredLocalResources;
use buck2_test_api::data::TestResult;
use buck2_test_api::protocol::TestOrchestrator;
use derive_more::From;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::channel::mpsc::UnboundedSender;
use futures::FutureExt;
use host_sharing::HostSharingRequirements;
use indexmap::indexset;
use indexmap::IndexMap;
use indexmap::IndexSet;
use more_futures::cancellation::CancellationContext;
use sorted_vector_map::SortedVectorMap;
use starlark::values::FrozenRef;
use uuid::Uuid;

use crate::local_resource_api::LocalResourcesSetupResult;
use crate::local_resource_registry::LocalResourceRegistry;
use crate::local_resource_setup::required_local_resources_setup_contexts;
use crate::local_resource_setup::LocalResourceSetupContext;
use crate::session::TestSession;
use crate::translations;

const MAX_SUFFIX_LEN: usize = 1024;

#[derive(Debug, Eq, PartialEq)]
pub enum ExecutorMessage {
    TestResult(TestResult),
    ExitCode(i32),
    InfoMessage(String),
}

pub struct BuckTestOrchestrator<'a> {
    dice: DiceTransaction,
    session: Arc<TestSession>,
    results_channel: UnboundedSender<anyhow::Result<ExecutorMessage>>,
    events: EventDispatcher,
    liveliness_observer: Arc<dyn LivelinessObserver>,
    digest_config: DigestConfig,
    cancellations: &'a CancellationContext,
    local_resource_state_registry: Arc<LocalResourceRegistry<'a>>,
}

impl<'a> BuckTestOrchestrator<'a> {
    pub async fn new(
        dice: DiceTransaction,
        session: Arc<TestSession>,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        results_channel: UnboundedSender<anyhow::Result<ExecutorMessage>>,
        cancellations: &'a CancellationContext,
        local_resource_state_registry: Arc<LocalResourceRegistry<'a>>,
    ) -> anyhow::Result<BuckTestOrchestrator<'a>> {
        let events = dice.per_transaction_data().get_dispatcher().dupe();
        let digest_config = dice.global_data().get_digest_config();
        Ok(Self::from_parts(
            dice,
            session,
            liveliness_observer,
            results_channel,
            events,
            digest_config,
            cancellations,
            local_resource_state_registry,
        ))
    }

    fn from_parts(
        dice: DiceTransaction,
        session: Arc<TestSession>,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        results_channel: UnboundedSender<anyhow::Result<ExecutorMessage>>,
        events: EventDispatcher,
        digest_config: DigestConfig,
        cancellations: &'a CancellationContext,
        local_resource_state_registry: Arc<LocalResourceRegistry<'a>>,
    ) -> BuckTestOrchestrator<'a> {
        Self {
            dice,
            session,
            results_channel,
            events,
            liveliness_observer,
            digest_config,
            cancellations,
            local_resource_state_registry,
        }
    }

    async fn require_alive(&self) -> Result<(), Cancelled> {
        if !self.liveliness_observer.is_alive().await {
            return Err(Cancelled);
        }

        Ok(())
    }

    async fn execute2(
        &self,
        metadata: DisplayMetadata,
        test_target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: SortedVectorMap<String, ArgValue>,
        timeout: Duration,
        host_sharing_requirements: HostSharingRequirements,
        pre_create_dirs: Vec<DeclaredOutput>,
        executor_override: Option<ExecutorConfigOverride>,
        required_local_resources: RequiredLocalResources,
    ) -> Result<ExecutionResult2, ExecuteError> {
        self.require_alive().await?;

        let test_target = self.session.get(test_target)?;

        let fs = self.dice.get_artifact_fs().await?;

        let test_info = self.get_test_info(&test_target).await?;
        let test_executor = self
            .get_test_executor(&test_target, &test_info, executor_override, &fs)
            .await?;
        let test_executable_expanded = self
            .expand_test_executable(
                &test_target,
                &test_info,
                cmd,
                env,
                pre_create_dirs,
                &test_executor.executor_fs(),
            )
            .await?;

        let ExpandedTestExecutable {
            cwd,
            cmd: expanded_cmd,
            env: expanded_env,
            inputs,
            supports_re,
            declared_outputs,
        } = test_executable_expanded;

        let executor_preference = self.executor_preference(supports_re)?;

        let required_resources = if test_executor.is_local_execution_possible(executor_preference) {
            let setup_local_resources_executor = self.get_local_executor(&fs)?;

            let setup_contexts = {
                let executor_fs = setup_local_resources_executor.executor_fs();
                let mut cmd_line_context = DefaultCommandLineContext::new(&executor_fs);
                required_local_resources_setup_contexts(
                    &mut cmd_line_context,
                    &test_info,
                    &required_local_resources,
                )?
            };
            // Some timeout is neeeded, use the same value as for the test itself which is better than nothing.
            let resources = self
                .setup_local_resources(setup_contexts, setup_local_resources_executor, timeout)
                .await?;

            self.require_alive().await?;

            resources
        } else {
            vec![]
        };

        let execution_request = self
            .create_command_execution_request(
                cwd,
                expanded_cmd,
                expanded_env,
                inputs,
                declared_outputs,
                &fs,
                Some(timeout),
                Some(host_sharing_requirements),
                Some(executor_preference),
                required_resources,
            )
            .await?;

        let (stdout, stderr, status, timing, outputs) = self
            .execute_shared(&test_target, metadata, &test_executor, execution_request)
            .await?;

        self.require_alive().await?;

        let (outputs, paths_to_materialize) = outputs
            .into_iter()
            .map(|test_path| {
                let project_path = fs.buck_out_path_resolver().resolve_test(&test_path);
                let abs_path = fs.fs().resolve(&project_path);
                let declared_output = DeclaredOutput {
                    name: test_path.into_path(),
                };
                ((declared_output, Output::LocalPath(abs_path)), project_path)
            })
            .unzip();

        // Request materialization in case this ran on RE. Eventually Tpx should be able to
        // understand remote outputs but currently we don't have this.
        self.dice
            .per_transaction_data()
            .get_materializer()
            .ensure_materialized(paths_to_materialize)
            .await
            .context("Error materializing test outputs")?;

        Ok(ExecutionResult2 {
            status,
            stdout,
            stderr,
            outputs,
            start_time: timing.start_time,
            execution_time: timing.execution_time,
        })
    }
}

struct PreparedLocalResourceSetupContext {
    pub target: ConfiguredTargetLabel,
    pub execution_request: CommandExecutionRequest,
    pub env_var_mapping: IndexMap<String, String>,
}

// A token used to implement From
struct Cancelled;

// NOTE: This doesn't implement Error so that we can't accidentally lose the Cancelled variant.
#[derive(From)]
enum ExecuteError {
    Error(anyhow::Error),
    Cancelled(Cancelled),
}

#[async_trait]
impl<'a> TestOrchestrator for BuckTestOrchestrator<'a> {
    async fn execute2(
        &self,
        metadata: DisplayMetadata,
        test_target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: SortedVectorMap<String, ArgValue>,
        timeout: Duration,
        host_sharing_requirements: HostSharingRequirements,
        pre_create_dirs: Vec<DeclaredOutput>,
        executor_override: Option<ExecutorConfigOverride>,
        required_local_resources: RequiredLocalResources,
    ) -> anyhow::Result<ExecuteResponse> {
        let res = BuckTestOrchestrator::execute2(
            self,
            metadata,
            test_target,
            cmd,
            env,
            timeout,
            host_sharing_requirements,
            pre_create_dirs,
            executor_override,
            required_local_resources,
        )
        .await;

        match res {
            Ok(r) => Ok(ExecuteResponse::Result(r)),
            Err(ExecuteError::Cancelled(Cancelled)) => Ok(ExecuteResponse::Cancelled),
            Err(ExecuteError::Error(e)) => Err(e),
        }
    }

    async fn report_test_result(&self, r: TestResult) -> anyhow::Result<()> {
        let event = buck2_data::instant_event::Data::TestResult(translations::convert_test_result(
            r.clone(),
            &self.session,
        )?);
        self.events.instant_event(event);
        self.results_channel
            .unbounded_send(Ok(ExecutorMessage::TestResult(r)))
            .map_err(|_| anyhow::Error::msg("Test result was received after end-of-tests"))?;
        Ok(())
    }

    async fn report_tests_discovered(
        &self,
        test_target: ConfiguredTargetHandle,
        suite: String,
        names: Vec<String>,
    ) -> anyhow::Result<()> {
        let test_target = self.session.get(test_target)?;

        self.events.instant_event(TestDiscovery {
            data: Some(buck2_data::test_discovery::Data::Tests(TestSuite {
                suite_name: suite,
                test_names: names,
                target_label: Some(test_target.target().as_proto()),
            })),
        });

        Ok(())
    }

    async fn report_test_session(&self, session_info: String) -> anyhow::Result<()> {
        self.events.instant_event(TestDiscovery {
            data: Some(buck2_data::test_discovery::Data::Session(TestSessionInfo {
                info: session_info,
            })),
        });

        Ok(())
    }

    async fn end_of_test_results(&self, exit_code: i32) -> anyhow::Result<()> {
        self.results_channel
            .unbounded_send(Ok(ExecutorMessage::ExitCode(exit_code)))
            .map_err(|_| anyhow::Error::msg("end_of_tests was received twice"))?;
        self.results_channel.close_channel();
        Ok(())
    }

    async fn prepare_for_local_execution(
        &self,
        _metadata: DisplayMetadata,
        test_target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: SortedVectorMap<String, ArgValue>,
        pre_create_dirs: Vec<DeclaredOutput>,
    ) -> anyhow::Result<PrepareForLocalExecutionResult> {
        let test_target = self.session.get(test_target)?;

        let fs = self.dice.get_artifact_fs().await?;

        let test_info = self.get_test_info(&test_target).await?;
        // Tests are not run, so there is no executor override.
        let executor = self
            .get_test_executor(&test_target, &test_info, None, &fs)
            .await?;
        let test_executable_expanded = self
            .expand_test_executable(
                &test_target,
                &test_info,
                cmd,
                env,
                pre_create_dirs,
                &executor.executor_fs(),
            )
            .await?;

        let ExpandedTestExecutable {
            cwd,
            cmd: expanded_cmd,
            env: expanded_env,
            inputs,
            supports_re: _,
            declared_outputs,
        } = test_executable_expanded;

        let execution_request = self
            .create_command_execution_request(
                cwd,
                expanded_cmd,
                expanded_env,
                inputs,
                declared_outputs,
                &fs,
                None,
                None,
                None,
                vec![],
            )
            .await?;

        let materializer = self.dice.per_transaction_data().get_materializer();
        let blocking_executor = self.dice.get_blocking_executor();

        materialize_inputs(&fs, &materializer, &execution_request).await?;

        create_output_dirs(
            &fs,
            &execution_request,
            materializer.dupe(),
            blocking_executor,
            self.cancellations,
        )
        .await?;

        Ok(create_prepare_for_local_execution_result(
            &fs,
            execution_request,
        ))
    }

    async fn attach_info_message(&self, message: String) -> anyhow::Result<()> {
        self.results_channel
            .unbounded_send(Ok(ExecutorMessage::InfoMessage(message)))
            .map_err(|_| anyhow::Error::msg("Message received after end-of-tests"))?;
        Ok(())
    }
}

impl<'b> BuckTestOrchestrator<'b> {
    fn executor_preference(&self, test_supports_re: bool) -> anyhow::Result<ExecutorPreference> {
        let mut executor_preference = ExecutorPreference::Default;

        if !self.session.options().allow_re {
            // We don't ban RE (we only prefer not to use it) if the session doesn't allow it, so
            // that executor overrides or default executor can still route executions to RE.
            executor_preference = executor_preference.and(ExecutorPreference::LocalPreferred)?;
        }

        if !test_supports_re {
            // But if the test doesn't support RE at all, then we ban it.
            executor_preference = executor_preference.and(ExecutorPreference::LocalRequired)?;
        }

        Ok(executor_preference)
    }

    async fn execute_shared(
        &self,
        test_target: &ConfiguredProvidersLabel,
        metadata: DisplayMetadata,
        executor: &CommandExecutor,
        request: CommandExecutionRequest,
    ) -> Result<
        (
            ExecutionStream,
            ExecutionStream,
            ExecutionStatus,
            CommandExecutionMetadata,
            Vec<BuckOutTestPath>,
        ),
        ExecuteError,
    > {
        let manager = CommandExecutionManager::new(
            Box::new(MutexClaimManager::new()),
            self.events.dupe(),
            self.liveliness_observer.dupe(),
        );

        let mut action_key_suffix = match &metadata {
            DisplayMetadata::Listing(_) => "listing".to_owned(),
            DisplayMetadata::Testing { testcases, .. } => testcases.join(" "),
        };
        if action_key_suffix.len() > MAX_SUFFIX_LEN {
            let truncated = "(truncated)";
            action_key_suffix.truncate(MAX_SUFFIX_LEN - truncated.len());
            action_key_suffix += truncated;
        }

        let test_target = TestTarget {
            target: test_target.target(),
            action_key_suffix,
        };

        // For test execution, we currently do not do any cache queries

        let prepared_action = executor.prepare_action(&request, self.digest_config)?;
        let prepared_command = PreparedCommand {
            target: &test_target as _,
            request: &request,
            prepared_action: &prepared_action,
            digest_config: self.digest_config,
        };
        let command = executor.exec_cmd(manager, &prepared_command, self.cancellations);

        // instrument execution with a span.
        // TODO(brasselsprouts): migrate this into the executor to get better accuracy.
        let CommandExecutionResult {
            outputs,
            report:
                CommandExecutionReport {
                    std_streams,
                    exit_code,
                    status,
                    timing,
                    ..
                },
            rejected_execution: _,
            did_cache_upload: _,
            eligible_for_full_hybrid: _,
        } = match metadata {
            DisplayMetadata::Listing(listing) => {
                let start = TestDiscoveryStart {
                    suite_name: listing,
                };
                let end = TestDiscoveryEnd {};
                self.events
                    .span_async(start, async move { (command.await, end) })
                    .await
            }
            DisplayMetadata::Testing { suite, testcases } => {
                let start = TestRunStart {
                    suite: Some(TestSuite {
                        suite_name: suite,
                        test_names: testcases,
                        target_label: Some(test_target.target.as_proto()),
                    }),
                };
                let end = TestRunEnd {};
                self.events
                    .span_async(start, async move { (command.await, end) })
                    .await
            }
        };

        let outputs = outputs
            .into_keys()
            .filter_map(|output| Some(output.into_test_path()?.0))
            .collect();

        let std_streams = std_streams
            .into_bytes()
            .await
            .context("Error accessing test output")?;
        let stdout = ExecutionStream::Inline(std_streams.stdout);
        let stderr = ExecutionStream::Inline(std_streams.stderr);

        Ok(match status {
            CommandExecutionStatus::Success { .. } => (
                stdout,
                stderr,
                ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(0),
                },
                timing,
                outputs,
            ),
            CommandExecutionStatus::Failure { .. } => (
                stdout,
                stderr,
                ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(1),
                },
                timing,
                outputs,
            ),
            CommandExecutionStatus::TimedOut { duration, .. } => (
                stdout,
                stderr,
                ExecutionStatus::TimedOut { duration },
                timing,
                outputs,
            ),
            CommandExecutionStatus::Error { stage: _, error } => (
                ExecutionStream::Inline(Default::default()),
                ExecutionStream::Inline(format!("{:?}", error).into_bytes()),
                ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(1),
                },
                timing,
                outputs,
            ),
            CommandExecutionStatus::Cancelled => {
                return Err(ExecuteError::Cancelled(Cancelled));
            }
        })
    }

    fn get_command_executor(
        &self,
        fs: &ArtifactFs,
        test_target_node: &ConfiguredTargetNode,
        executor_override: Option<&CommandExecutorConfig>,
    ) -> anyhow::Result<CommandExecutor> {
        let executor_config = match executor_override {
            Some(o) => o,
            None => test_target_node
                .execution_platform_resolution()
                .executor_config()
                .context("Error accessing executor config")?,
        };

        let CommandExecutorResponse {
            executor,
            platform,
            cache_checker: _,
        } = self.dice.get_command_executor(fs, executor_config)?;
        let run_action_knobs = self.dice.per_transaction_data().get_run_action_knobs();
        let executor = CommandExecutor::new(
            executor,
            // Caching is not enabled for tests yet. Use the NoOp
            Arc::new(NoOpCommandExecutor {}),
            fs.clone(),
            executor_config.options,
            platform,
            run_action_knobs.enforce_re_timeouts,
        );
        Ok(executor)
    }

    fn get_local_executor(&self, fs: &ArtifactFs) -> anyhow::Result<CommandExecutor> {
        let executor_config = CommandExecutorConfig {
            executor: Executor::Local(LocalExecutorOptions::default()),
            options: CommandGenerationOptions {
                path_separator: PathSeparatorKind::system_default(),
                output_paths_behavior: Default::default(),
            },
        };
        let CommandExecutorResponse {
            executor,
            platform,
            cache_checker: _,
        } = self.dice.get_command_executor(fs, &executor_config)?;
        let run_action_knobs = self.dice.per_transaction_data().get_run_action_knobs();
        let executor = CommandExecutor::new(
            executor,
            Arc::new(NoOpCommandExecutor {}),
            fs.clone(),
            executor_config.options,
            platform,
            run_action_knobs.enforce_re_timeouts,
        );
        Ok(executor)
    }

    async fn get_test_info(
        &self,
        test_target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<FrozenRef<'static, FrozenExternalRunnerTestInfo>> {
        let providers = self
            .dice
            .get_providers(test_target)
            .await?
            .require_compatible()?;

        let providers = providers.provider_collection();
        providers
            .get_provider(ExternalRunnerTestInfoCallable::provider_id_t())
            .context("Test executable only supports ExternalRunnerTestInfo providers")
    }

    async fn get_test_executor(
        &self,
        test_target: &ConfiguredProvidersLabel,
        test_info: &FrozenExternalRunnerTestInfo,
        executor_override: Option<ExecutorConfigOverride>,
        fs: &ArtifactFs,
    ) -> anyhow::Result<CommandExecutor> {
        // NOTE: get_providers() implicitly calls this already but it's not the end of the world
        // since this will get cached in DICE.
        let node = self
            .dice
            .get_configured_target_node(test_target.target())
            .await?
            .require_compatible()?;

        let resolved_executor_override = match executor_override.as_ref() {
            Some(executor_override) => Some(
                &test_info
                    .executor_override(&executor_override.name)
                    .context("The `executor_override` provided does not exist")
                    .with_context(|| {
                        format!(
                            "Error processing `executor_override`: `{}`",
                            executor_override.name
                        )
                    })?
                    .0,
            ),
            None => test_info.default_executor().map(|o| &o.0),
        };

        self.get_command_executor(
            fs,
            &node,
            resolved_executor_override.as_ref().map(|a| &***a),
        )
        .context("Error constructing CommandExecutor")
    }

    async fn expand_test_executable(
        &self,
        test_target: &ConfiguredProvidersLabel,
        test_info: &FrozenExternalRunnerTestInfo,
        cmd: Vec<ArgValue>,
        env: SortedVectorMap<String, ArgValue>,
        pre_create_dirs: Vec<DeclaredOutput>,
        executor_fs: &ExecutorFs<'_>,
    ) -> anyhow::Result<ExpandedTestExecutable> {
        let output_root = self
            .session
            .prefix()
            .join(ForwardRelativePathBuf::unchecked_new(
                Uuid::new_v4().to_string(),
            ));

        let mut declared_outputs = IndexMap::<BuckOutTestPath, OutputCreationBehavior>::new();

        let mut supports_re = true;

        let cwd;
        let expanded;

        {
            let opts = self.session.options();

            cwd = if test_info.run_from_project_root() || opts.force_run_from_project_root {
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("".to_owned()))
            } else {
                supports_re = false;
                // For compatibility with v1,
                let cell_resolver = self.dice.get_cell_resolver().await?;
                let cell = cell_resolver.get(test_target.target().pkg().cell_name())?;
                cell.path().to_buf()
            };

            let expander = Execute2RequestExpander {
                test_info,
                output_root: &output_root,
                declared_outputs: &mut declared_outputs,
                fs: executor_fs,
                cmd,
                env,
            };

            expanded = if test_info.use_project_relative_paths()
                || opts.force_use_project_relative_paths
            {
                expander.expand::<DefaultCommandLineContext>()
            } else {
                supports_re = false;
                expander.expand::<AbsCommandLineContext>()
            }?;
        };

        let (expanded_cmd, expanded_env, inputs) = expanded;

        for output in pre_create_dirs {
            let test_path = BuckOutTestPath::new(output_root.clone(), output.name);
            declared_outputs.insert(test_path, OutputCreationBehavior::Create);
        }

        Ok(ExpandedTestExecutable {
            cwd: cwd.project_relative_path().to_buf(),
            cmd: expanded_cmd,
            env: expanded_env,
            inputs,
            declared_outputs,
            supports_re,
        })
    }

    async fn create_command_execution_request(
        &self,
        cwd: ProjectRelativePathBuf,
        cmd: Vec<String>,
        env: SortedVectorMap<String, String>,
        cmd_inputs: IndexSet<ArtifactGroup>,
        declared_outputs: IndexMap<BuckOutTestPath, OutputCreationBehavior>,
        fs: &ArtifactFs,
        timeout: Option<Duration>,
        host_sharing_requirements: Option<HostSharingRequirements>,
        executor_preference: Option<ExecutorPreference>,
        required_local_resources: Vec<LocalResourceState>,
    ) -> anyhow::Result<CommandExecutionRequest> {
        let mut inputs = Vec::with_capacity(cmd_inputs.len());
        for input in &cmd_inputs {
            // we already built these before reaching out to tpx, so these should already be ready
            // hence we don't actually need to spawn these in parallel
            // TODO (T102328660): Does CommandExecutionRequest need this artifact?
            inputs.push(CommandExecutionInput::Artifact(Box::new(
                self.dice.ensure_artifact_group(input).await?,
            )));
        }

        // NOTE: This looks a bit awkward, that's because fbcode's rustfmt and ours slightly
        // disagree about format here...
        let outputs = declared_outputs
            .into_iter()
            .map(|(path, create)| CommandExecutionOutput::TestPath { path, create })
            .collect();
        let mut request = CommandExecutionRequest::new(
            vec![],
            cmd,
            CommandExecutionPaths::new(inputs, outputs, fs, self.digest_config)?,
            env,
        );
        request = request
            .with_working_directory(cwd)
            .with_local_environment_inheritance(EnvironmentInheritance::test_allowlist())
            .with_disable_miniperf(true)
            .with_required_local_resources(required_local_resources)?;
        if let Some(timeout) = timeout {
            request = request.with_timeout(timeout)
        }
        if let Some(host_sharing_requirements) = host_sharing_requirements {
            request = request.with_host_sharing_requirements(host_sharing_requirements);
        }
        if let Some(executor_preference) = executor_preference {
            request = request.with_executor_preference(executor_preference);
        }
        Ok(request)
    }

    async fn setup_local_resources(
        &self,
        setup_contexts: Vec<LocalResourceSetupContext>,
        executor: CommandExecutor,
        timeout: Duration,
    ) -> Result<Vec<LocalResourceState>, ExecuteError> {
        let setup_commands = futures::future::try_join_all(
            setup_contexts
                .into_iter()
                .map(|context| self.prepare_local_resource(context, executor.fs(), timeout)),
        )
        .await?;

        self.require_alive().await?;

        let resource_futs = setup_commands.into_iter().map(|context| {
            let local_resource_target = context.target.dupe();
            self.local_resource_state_registry
                .0
                .entry(local_resource_target.dupe())
                .or_insert_with(|| {
                    let setup = Self::start_local_resource(
                        self.events.dupe(),
                        self.liveliness_observer.dupe(),
                        self.digest_config.dupe(),
                        executor.dupe(),
                        context,
                        self.cancellations,
                    );
                    async move {
                        setup
                            .await
                            .with_context(|| {
                                format!(
                                    "Error setting up local resource declared in `{}`",
                                    local_resource_target
                                )
                            })
                            .shared_error()
                    }
                    .boxed()
                    .shared()
                })
                .clone()
        });

        Ok(futures::future::try_join_all(resource_futs)
            .await
            .map_err(anyhow::Error::from)?)
    }

    async fn prepare_local_resource(
        &self,
        context: LocalResourceSetupContext,
        fs: &ArtifactFs,
        timeout: Duration,
    ) -> anyhow::Result<PreparedLocalResourceSetupContext> {
        let futs = context
            .input_artifacts
            .iter()
            .map(|group| self.dice.ensure_artifact_group(group));
        let inputs = futures::future::try_join_all(futs).await?;
        let inputs = inputs
            .into_iter()
            .map(|group_values| CommandExecutionInput::Artifact(Box::new(group_values)))
            .collect();
        let paths = CommandExecutionPaths::new(inputs, indexset![], fs, self.digest_config)?;
        let mut execution_request =
            CommandExecutionRequest::new(vec![], context.cmd, paths, Default::default());
        execution_request = execution_request.with_timeout(timeout);
        Ok(PreparedLocalResourceSetupContext {
            target: context.target,
            execution_request,
            env_var_mapping: context.env_var_mapping,
        })
    }

    async fn start_local_resource(
        events: EventDispatcher,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        digest_config: DigestConfig,
        executor: CommandExecutor,
        context: PreparedLocalResourceSetupContext,
        cancellations: &'b CancellationContext,
    ) -> SharedResult<LocalResourceState> {
        let manager = CommandExecutionManager::new(
            Box::new(MutexClaimManager::new()),
            events.dupe(),
            liveliness_observer,
        );

        let local_resource_target = LocalResourceTarget {
            target: &context.target,
        };
        let prepared_action = executor.prepare_action(&context.execution_request, digest_config)?;
        let prepared_command = PreparedCommand {
            target: &local_resource_target as _,
            request: &context.execution_request,
            prepared_action: &prepared_action,
            digest_config,
        };
        let command = executor.exec_cmd(manager, &prepared_command, cancellations);

        let start = SetupLocalResourcesStart {};
        let end = SetupLocalResourcesEnd {};
        let execution_result = events
            .span_async(start, async move { (command.await, end) })
            .await;

        let CommandExecutionResult {
            outputs: _,
            report:
                CommandExecutionReport {
                    std_streams,
                    exit_code,
                    status,
                    timing: _,
                    ..
                },
            rejected_execution: _,
            did_cache_upload: _,
            eligible_for_full_hybrid: _,
        } = execution_result;

        let std_streams = std_streams
            .into_bytes()
            .await
            .context("Error accessing setup local resource output")?;

        match status {
            CommandExecutionStatus::Success { .. } => {}
            CommandExecutionStatus::Failure { .. } => {
                return Err(SharedError::new(anyhow::anyhow!(
                    "Local resource setup command failed with `{}` exit code, stdout:\n{}\nstderr:\n{}\n",
                    exit_code.unwrap_or(1),
                    String::from_utf8_lossy(&std_streams.stdout),
                    String::from_utf8_lossy(&std_streams.stderr),
                )));
            }
            CommandExecutionStatus::TimedOut { duration, .. } => {
                return Err(SharedError::new(anyhow::anyhow!(
                    "Local resource setup command timed out after `{}s`, stdout:\n{}\nstderr:\n{}\n",
                    duration.as_secs(),
                    String::from_utf8_lossy(&std_streams.stdout),
                    String::from_utf8_lossy(&std_streams.stderr),
                )));
            }
            CommandExecutionStatus::Error { stage: _, error } => {
                return Err(SharedError::new(error));
            }
            CommandExecutionStatus::Cancelled => {
                return Err(SharedError::new(anyhow::anyhow!(
                    "Local resource setup command cancelled"
                )));
            }
        };

        let string_content = String::from_utf8_lossy(&std_streams.stdout);
        let data: LocalResourcesSetupResult = serde_json::from_str(&string_content)
            .context("Error parsing local resource setup command output")?;
        let state = data.into_state(context.target.clone(), &context.env_var_mapping)?;

        Ok(state)
    }
}

impl<'a> Drop for BuckTestOrchestrator<'a> {
    fn drop(&mut self) {
        // If we didn't close the sender yet, then notify the receiver that our stream is
        // incomplete.
        let _ignored = self.results_channel.unbounded_send(Err(anyhow::Error::msg(
            "BuckTestOrchestrator exited before end-of-tests was received",
        )));
    }
}

struct Execute2RequestExpander<'a> {
    test_info: &'a FrozenExternalRunnerTestInfo,
    output_root: &'a ForwardRelativePath,
    declared_outputs: &'a mut IndexMap<BuckOutTestPath, OutputCreationBehavior>,
    fs: &'a ExecutorFs<'a>,
    cmd: Vec<ArgValue>,
    env: SortedVectorMap<String, ArgValue>,
}

impl<'a> Execute2RequestExpander<'a> {
    /// Expand a command and env. Return CLI, env, and inputs.
    fn expand<B>(
        self,
    ) -> anyhow::Result<(
        Vec<String>,
        SortedVectorMap<String, String>,
        IndexSet<ArtifactGroup>,
    )>
    where
        B: CommandLineContextExt<'a>,
    {
        let cli_args_for_interpolation = self
            .test_info
            .command()
            .filter_map(|c| match c {
                TestCommandMember::Literal(..) => None,
                TestCommandMember::Arglike(a) => Some(a),
            })
            .collect::<Vec<_>>();

        let env_for_interpolation = self.test_info.env().collect::<HashMap<_, _>>();

        let expand_arg_value = |cli: &mut Vec<String>,
                                ctx: &mut dyn CommandLineContext,
                                artifact_visitor: &mut dyn CommandLineArtifactVisitor,
                                declared_outputs: &mut IndexMap<
            BuckOutTestPath,
            OutputCreationBehavior,
        >,
                                value: ArgValue| {
            let ArgValue { content, format } = value;

            let mut cli = CommandLineBuilderFormatWrapper { inner: cli, format };

            match content {
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::Verbatim(v)) => {
                    v.add_to_command_line(&mut cli, ctx)?;
                }
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::ArgHandle(h)) => {
                    let arg = cli_args_for_interpolation
                        .get(h.0)
                        .with_context(|| format!("Invalid ArgHandle: {:?}", h))?;

                    arg.visit_artifacts(artifact_visitor)?;
                    arg.add_to_command_line(&mut cli, ctx)?;
                }
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::EnvHandle(h)) => {
                    let arg = env_for_interpolation
                        .get(h.0.as_str())
                        .with_context(|| format!("Invalid EnvHandle: {:?}", h))?;
                    arg.visit_artifacts(artifact_visitor)?;
                    arg.add_to_command_line(&mut cli, ctx)?;
                }
                ArgValueContent::DeclaredOutput(output) => {
                    let test_path = BuckOutTestPath::new(self.output_root.to_owned(), output.name);
                    let path = self
                        .fs
                        .fs()
                        .buck_out_path_resolver()
                        .resolve_test(&test_path);
                    let path = ctx.resolve_project_path(path)?.into_string();
                    cli.push_arg(path);
                    declared_outputs.insert(test_path, OutputCreationBehavior::Parent);
                }
            };

            anyhow::Ok(())
        };

        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();

        let mut expanded_cmd = Vec::<String>::new();
        let mut ctx = B::new(self.fs);
        for var in self.cmd {
            expand_arg_value(
                &mut expanded_cmd,
                &mut ctx,
                &mut artifact_visitor,
                self.declared_outputs,
                var,
            )?;
        }

        let expanded_env = self
            .env
            .into_iter()
            .map(|(k, v)| {
                let mut env = Vec::<String>::new();
                let mut ctx = B::new(self.fs);
                expand_arg_value(
                    &mut env,
                    &mut ctx,
                    &mut artifact_visitor,
                    self.declared_outputs,
                    v,
                )?;
                // TODO (torozco): Just use a String directly
                anyhow::Ok((k, env.join(" ")))
            })
            .collect::<Result<SortedVectorMap<_, _>, _>>()?;

        let inputs = artifact_visitor.inputs;

        Ok((expanded_cmd, expanded_env, inputs))
    }
}

trait CommandLineContextExt<'a>: CommandLineContext + 'a {
    fn new(fs: &'a ExecutorFs) -> Self;
}

impl<'a> CommandLineContextExt<'a> for DefaultCommandLineContext<'a> {
    fn new(fs: &'a ExecutorFs) -> Self {
        Self::new(fs)
    }
}

impl<'a> CommandLineContextExt<'a> for AbsCommandLineContext<'a> {
    fn new(fs: &'a ExecutorFs) -> Self {
        Self::new(fs)
    }
}

struct CommandLineBuilderFormatWrapper<'a> {
    inner: &'a mut dyn CommandLineBuilder,
    format: Option<String>,
}

impl<'a> CommandLineBuilder for CommandLineBuilderFormatWrapper<'a> {
    fn push_arg(&mut self, s: String) {
        let s = if let Some(format) = &self.format {
            format.replace("{}", &s)
        } else {
            s
        };

        self.inner.push_arg(s);
    }
}

struct ExpandedTestExecutable {
    cwd: ProjectRelativePathBuf,
    cmd: Vec<String>,
    env: SortedVectorMap<String, String>,
    inputs: IndexSet<ArtifactGroup>,
    supports_re: bool,
    declared_outputs: IndexMap<BuckOutTestPath, OutputCreationBehavior>,
}

fn create_prepare_for_local_execution_result(
    fs: &ArtifactFs,
    request: CommandExecutionRequest,
) -> PrepareForLocalExecutionResult {
    let relative_cwd = request
        .working_directory()
        .unwrap_or_else(|| ProjectRelativePath::empty());
    let cwd = fs.fs().resolve(relative_cwd);
    let cmd = request.all_args_vec();

    let mut env = LossyEnvironment::new();
    apply_local_execution_environment(
        &mut env,
        &cwd,
        request.env(),
        request.local_environment_inheritance(),
    );

    PrepareForLocalExecutionResult {
        cmd,
        env: env.into_inner(),
        cwd,
    }
}

struct LossyEnvironment {
    inner: SortedVectorMap<String, String>,
}

impl LossyEnvironment {
    fn new() -> Self {
        Self {
            inner: SortedVectorMap::new(),
        }
    }

    fn into_inner(self) -> SortedVectorMap<String, String> {
        self.inner
    }
}

impl EnvironmentBuilder for LossyEnvironment {
    fn clear(&mut self) {
        self.inner.clear();
    }

    fn set<K, V>(&mut self, key: K, val: V)
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        self.inner.insert(
            key.as_ref().to_string_lossy().into_owned(),
            val.as_ref().to_string_lossy().into_owned(),
        );
    }

    fn remove<K>(&mut self, key: K)
    where
        K: AsRef<OsStr>,
    {
        self.inner.remove(&*key.as_ref().to_string_lossy());
    }
}

#[derive(Debug)]
struct TestTarget<'a> {
    target: &'a ConfiguredTargetLabel,
    action_key_suffix: String,
}

impl CommandExecutionTarget for TestTarget<'_> {
    fn re_action_key(&self) -> String {
        format!("{} test {}", self.target, self.action_key_suffix)
    }

    fn re_affinity_key(&self) -> String {
        self.target.to_string()
    }

    fn as_proto_action_key(&self) -> buck2_data::ActionKey {
        buck2_data::ActionKey {
            id: Default::default(),
            owner: Some(buck2_data::action_key::Owner::TestTargetLabel(
                self.target.as_proto(),
            )),
            key: Default::default(),
        }
    }

    fn as_proto_action_name(&self) -> buck2_data::ActionName {
        buck2_data::ActionName {
            category: "test".to_owned(),
            identifier: "".to_owned(),
        }
    }
}

#[derive(Debug)]
struct LocalResourceTarget<'a> {
    target: &'a ConfiguredTargetLabel,
}

impl CommandExecutionTarget for LocalResourceTarget<'_> {
    fn re_action_key(&self) -> String {
        String::new()
    }

    fn re_affinity_key(&self) -> String {
        String::new()
    }

    fn as_proto_action_key(&self) -> buck2_data::ActionKey {
        buck2_data::ActionKey {
            id: Default::default(),
            owner: Some(buck2_data::action_key::Owner::LocalResourceSetup(
                self.target.as_proto(),
            )),
            key: Default::default(),
        }
    }

    fn as_proto_action_name(&self) -> buck2_data::ActionName {
        buck2_data::ActionName {
            category: "setup_local_resource".to_owned(),
            identifier: "".to_owned(),
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_build_api::context::SetBuildContextData;
    use buck2_common::dice::cells::SetCellResolver;
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_common::liveliness_observer::NoopLivelinessObserver;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_events::dispatch::EventDispatcher;
    use buck2_test_api::data::TestStatus;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use futures::channel::mpsc;
    use futures::channel::mpsc::UnboundedReceiver;
    use futures::future;
    use futures::stream::TryStreamExt;

    use super::*;

    async fn make() -> anyhow::Result<(
        BuckTestOrchestrator<'static>,
        UnboundedReceiver<anyhow::Result<ExecutorMessage>>,
    )> {
        let fs = ProjectRootTemp::new().unwrap();

        let cell_resolver = CellResolver::testing_with_name_and_path(
            CellName::testing_new("cell"),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell".to_owned())),
        );
        let buckout_path = ProjectRelativePathBuf::unchecked_new("buck_out/v2".into());
        let mut dice = DiceBuilder::new()
            .set_data(|d| d.set_testing_io_provider(&fs))
            .build(UserComputationData::new())?;
        dice.set_buck_out_path(Some(buckout_path))?;
        dice.set_cell_resolver(cell_resolver)?;

        let dice = dice.commit().await;

        let (sender, receiver) = mpsc::unbounded();

        Ok((
            BuckTestOrchestrator::from_parts(
                dice,
                Arc::new(TestSession::new(Default::default())),
                NoopLivelinessObserver::create(),
                sender,
                EventDispatcher::null(),
                DigestConfig::testing_default(),
                CancellationContext::testing(),
                Arc::new(LocalResourceRegistry::new()),
            ),
            receiver,
        ))
    }

    #[tokio::test]
    async fn orchestrator_results() -> anyhow::Result<()> {
        let (orchestrator, channel) = make().await?;

        let target =
            ConfiguredTargetLabel::testing_parse("cell//pkg:foo", ConfigurationData::testing_new());

        let target = ConfiguredProvidersLabel::new(target, Default::default());
        let target = orchestrator.session.register(target);

        let jobs = async {
            orchestrator
                .report_test_result(TestResult {
                    target,
                    status: TestStatus::PASS,
                    msg: None,
                    name: "First - test".to_owned(),
                    duration: Some(Duration::from_micros(1)),
                    details: "1".to_owned(),
                })
                .await?;

            orchestrator
                .report_test_result(TestResult {
                    target,
                    status: TestStatus::FAIL,
                    msg: None,
                    name: "Second - test".to_owned(),
                    duration: Some(Duration::from_micros(2)),
                    details: "2".to_owned(),
                })
                .await?;

            orchestrator.end_of_test_results(0).await?;

            anyhow::Ok(())
        };

        let ((), results) = future::try_join(jobs, channel.try_collect::<Vec<_>>()).await?;

        assert_eq!(
            results,
            vec![
                ExecutorMessage::TestResult(TestResult {
                    target,

                    status: TestStatus::PASS,
                    msg: None,
                    name: "First - test".to_owned(),
                    duration: Some(Duration::from_micros(1)),
                    details: "1".to_owned(),
                }),
                ExecutorMessage::TestResult(TestResult {
                    target,

                    status: TestStatus::FAIL,
                    msg: None,
                    name: "Second - test".to_owned(),
                    duration: Some(Duration::from_micros(2)),
                    details: "2".to_owned(),
                }),
                ExecutorMessage::ExitCode(0),
            ]
        );

        Ok(())
    }

    #[tokio::test]
    async fn orchestrator_attach_info_messages() -> anyhow::Result<()> {
        let (orchestrator, channel) = make().await?;

        let jobs = async {
            orchestrator.attach_info_message("yolo".to_owned()).await?;

            orchestrator.end_of_test_results(0).await?;

            anyhow::Ok(())
        };

        let ((), results) = future::try_join(jobs, channel.try_collect::<Vec<_>>()).await?;

        assert_eq!(
            results,
            vec![
                ExecutorMessage::InfoMessage("yolo".to_owned(),),
                ExecutorMessage::ExitCode(0),
            ]
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_orchestrator_channel_drop() -> anyhow::Result<()> {
        let (orchestrator, channel) = make().await?;
        drop(orchestrator);

        let res = channel.try_collect::<Vec<_>>().await;
        assert!(res.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_orchestrator_closes_channel() -> anyhow::Result<()> {
        let (orchestrator, channel) = make().await?;
        let sender = orchestrator.results_channel.clone();
        orchestrator.end_of_test_results(1).await?;

        assert!(sender.is_closed());
        drop(channel);

        Ok(())
    }
}
