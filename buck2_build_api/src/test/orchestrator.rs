/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the `TestOrchestrator` from `test_api`.

use std::collections::HashMap;
use std::ffi::OsStr;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::category::Category;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::provider::ConfiguredProvidersLabel;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_data::TestDiscovery;
use buck2_data::TestDiscoveryEnd;
use buck2_data::TestDiscoveryStart;
use buck2_data::TestRunEnd;
use buck2_data::TestRunStart;
use buck2_data::TestSessionInfo;
use buck2_data::TestSuite;
use buck2_interpreter::dice::HasEvents;
use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use dice::DiceTransaction;
use events::dispatch::EventDispatcher;
use futures::channel::mpsc::UnboundedSender;
use gazebo::prelude::*;
use host_sharing::HostSharingRequirements;
use indexmap::IndexMap;
use indexmap::IndexSet;
use once_cell::sync::Lazy;
use test_api::data::ArgValue;
use test_api::data::ArgValueContent;
use test_api::data::ConfiguredTargetHandle;
use test_api::data::DeclaredOutput;
use test_api::data::DisplayMetadata;
use test_api::data::ExecutionResult2;
use test_api::data::ExecutionStatus;
use test_api::data::ExecutionStream;
use test_api::data::ExecutorConfigOverride;
use test_api::data::ExternalRunnerSpecValue;
use test_api::data::Output;
use test_api::data::PrepareForLocalExecutionResult;
use test_api::data::TestResult;
use test_api::protocol::TestOrchestrator;
use uuid::Uuid;

use crate::actions::artifact::ArtifactFs;
use crate::actions::artifact::ArtifactValue;
use crate::actions::artifact::ExecutorFs;
use crate::artifact_groups::ArtifactGroup;
use crate::calculation::Calculation;
use crate::deferred::BaseDeferredKey;
use crate::execute::blocking::HasBlockingExecutor;
use crate::execute::commands;
use crate::execute::commands::dice_data::HasCommandExecutor;
use crate::execute::commands::local::apply_local_execution_environment;
use crate::execute::commands::local::create_output_dirs;
use crate::execute::commands::local::materialize_inputs;
use crate::execute::commands::local::EnvironmentBuilder;
use crate::execute::commands::ClaimManager;
use crate::execute::commands::CommandExecutionInput;
use crate::execute::commands::CommandExecutionManager;
use crate::execute::commands::CommandExecutionRequest;
use crate::execute::commands::CommandExecutionResult;
use crate::execute::commands::CommandExecutionTarget;
use crate::execute::commands::CommandExecutionTimingData;
use crate::execute::commands::CommandExecutor;
use crate::execute::commands::EnvironmentInheritance;
use crate::execute::commands::OutputCreationBehavior;
use crate::execute::materializer::HasMaterializer;
use crate::execute::CommandExecutorConfig;
use crate::interpreter::rule_defs::cmd_args::AbsCommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::BaseCommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilderContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::provider::builtin::external_runner_test_info::ExternalRunnerTestInfoCallable;
use crate::interpreter::rule_defs::provider::builtin::external_runner_test_info::FrozenExternalRunnerTestInfo;
use crate::interpreter::rule_defs::provider::builtin::external_runner_test_info::TestCommandMember;
use crate::nodes::configured::ConfiguredTargetNode;
use crate::path::BuckOutTestPath;
use crate::test::orchestrator::commands::CommandExecutionOutput;
use crate::test::session::TestSession;
use crate::test::translations;

static TEST_CATEGORY: Lazy<Category> = Lazy::new(|| Category::try_from("test").unwrap());

#[derive(Debug, Eq, PartialEq)]
pub enum TestResultOrExitCode {
    TestResult(TestResult),
    ExitCode(i32),
}

pub struct BuckTestOrchestrator {
    dice: DiceTransaction,
    session: Arc<TestSession>,
    results_channel: UnboundedSender<anyhow::Result<TestResultOrExitCode>>,
    events: EventDispatcher,
    /// In order to run commands, we need to have unique identifiers.
    /// Unfortunately, Tpx might run the same test multiple times, so we have to manufacture our own.
    /// Through multiple executions of multiple builds, we don't want to keep creating fresh
    /// identifiers (e.g. Uuid or similar) because each might create some temporary outputs on disk,
    /// so use sequential identifiers for each target.
    identifiers: DashMap<ConfiguredTargetLabel, usize>,
}

impl BuckTestOrchestrator {
    pub async fn new(
        dice: DiceTransaction,
        session: Arc<TestSession>,
        results_channel: UnboundedSender<anyhow::Result<TestResultOrExitCode>>,
    ) -> anyhow::Result<Self> {
        let events = dice.per_transaction_data().get_dispatcher().dupe();
        Ok(Self::from_parts(dice, session, results_channel, events))
    }

    fn from_parts(
        dice: DiceTransaction,
        session: Arc<TestSession>,
        results_channel: UnboundedSender<anyhow::Result<TestResultOrExitCode>>,
        events: EventDispatcher,
    ) -> Self {
        Self {
            dice,
            session,
            results_channel,
            events,
            identifiers: Default::default(),
        }
    }

    /// Produce a unique identifier every time it is called with the same target
    fn new_identifier(&self, target: &ConfiguredTargetLabel) -> String {
        let i = {
            // The identifiers value always contains the value to use
            match self.identifiers.entry(target.dupe()) {
                Entry::Occupied(mut e) => {
                    let v = *e.get();
                    *e.get_mut() = v + 1;
                    v
                }
                Entry::Vacant(e) => {
                    e.insert(1);
                    0
                }
            }
        };
        format!("test-{}", i)
    }
}

#[async_trait]
impl TestOrchestrator for BuckTestOrchestrator {
    async fn execute2(
        &self,
        metadata: DisplayMetadata,
        test_target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: HashMap<String, ArgValue>,
        timeout: Duration,
        host_sharing_requirements: HostSharingRequirements,
        pre_create_dirs: Vec<DeclaredOutput>,
        executor_override: Option<ExecutorConfigOverride>,
    ) -> anyhow::Result<ExecutionResult2> {
        let test_target = self.session.get(test_target)?;

        let fs = self.dice.get_artifact_fs().await?;

        let test_executable_expanded = self
            .expand_test_executable(
                &fs,
                &test_target,
                cmd,
                env,
                pre_create_dirs,
                executor_override,
            )
            .await?;

        let ExpandedTestExecutable {
            cwd,
            cmd: expanded_cmd,
            env: expanded_env,
            inputs,
            supports_re,
            declared_outputs,
            executor,
        } = test_executable_expanded;
        let execution_request = self
            .create_command_execution_request(
                cwd,
                expanded_cmd,
                expanded_env,
                inputs,
                declared_outputs,
            )
            .await?;

        let (stdout, stderr, status, timing, outputs) = self
            .execute_shared(
                test_target,
                metadata,
                Some(host_sharing_requirements),
                timeout,
                supports_re,
                &executor,
                execution_request,
            )
            .await?;

        let (outputs, paths_to_materialize) = outputs
            .into_iter()
            .filter_map(|(output, _value)| output.into_test_path())
            .map(|(test_path, _)| {
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

    async fn report_test_result(&self, r: TestResult) -> anyhow::Result<()> {
        let event = buck2_data::instant_event::Data::TestResult(translations::convert_test_result(
            r.clone(),
        )?);
        self.events.instant_event(event);
        self.results_channel
            .unbounded_send(Ok(TestResultOrExitCode::TestResult(r)))
            .map_err(|_| anyhow::Error::msg("Test result was received after end-of-tests"))?;
        Ok(())
    }

    async fn report_tests_discovered(
        &self,
        // FIXME(bobyf): Should be using this
        _target: ConfiguredTargetHandle,
        suite: String,
        names: Vec<String>,
    ) -> anyhow::Result<()> {
        self.events.instant_event(TestDiscovery {
            data: Some(buck2_data::test_discovery::Data::Tests(TestSuite {
                suite_name: suite,
                test_names: names,
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
            .unbounded_send(Ok(TestResultOrExitCode::ExitCode(exit_code)))
            .map_err(|_| anyhow::Error::msg("end_of_tests was received twice"))?;
        self.results_channel.close_channel();
        Ok(())
    }

    async fn prepare_for_local_execution(
        &self,
        _metadata: DisplayMetadata,
        test_target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: HashMap<String, ArgValue>,
        pre_create_dirs: Vec<DeclaredOutput>,
    ) -> anyhow::Result<PrepareForLocalExecutionResult> {
        let test_target = self.session.get(test_target)?;

        let fs = self.dice.get_artifact_fs().await?;

        let test_executable_expanded = self
            .expand_test_executable(
                &fs,
                &test_target,
                cmd,
                env,
                pre_create_dirs,
                None, // No executor used, so there isn't a executor to override.
            )
            .await?;

        let ExpandedTestExecutable {
            cwd,
            cmd: expanded_cmd,
            env: expanded_env,
            inputs,
            supports_re: _,
            declared_outputs,
            executor: _,
        } = test_executable_expanded;

        let execution_request = self
            .create_command_execution_request(
                cwd,
                expanded_cmd,
                expanded_env,
                inputs,
                declared_outputs,
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
        )
        .await?;

        Ok(create_prepare_for_local_execution_result(
            &fs,
            execution_request,
        ))
    }
}

impl BuckTestOrchestrator {
    async fn execute_shared(
        &self,
        test_target: ConfiguredProvidersLabel,
        metadata: DisplayMetadata,
        host_sharing_requirements: Option<HostSharingRequirements>,
        timeout: Duration,
        supports_re: bool,
        executor: &CommandExecutor,
        mut request: CommandExecutionRequest,
    ) -> anyhow::Result<(
        ExecutionStream,
        ExecutionStream,
        ExecutionStatus,
        CommandExecutionTimingData,
        IndexMap<CommandExecutionOutput, ArtifactValue>,
    )> {
        request = request.with_timeout(timeout).with_custom_tmpdir(false);
        if let Some(requirements) = host_sharing_requirements {
            request = request.with_host_sharing_requirements(requirements);
        }

        if !(self.session.options().allow_re && supports_re) {
            request = request.with_local_only(true)
        }

        let manager = CommandExecutionManager::new(
            executor.name(),
            <dyn ClaimManager>::new_simple(),
            self.events.dupe(),
        );

        // We'd love to use the `metadata` field to generate a unique identifier,
        // but Tpx might run the same test repeatedly, so it is not unique.
        let identifier = self.new_identifier(test_target.target());
        let owner = BaseDeferredKey::TargetLabel(test_target.target().dupe());

        let command = executor.exec_cmd(
            CommandExecutionTarget {
                owner: &owner,
                category: &TEST_CATEGORY,
                identifier: Some(&identifier),
            },
            &request,
            manager,
        );

        // instrument execution with a span.
        // TODO(brasselsprouts): migrate this into the executor to get better accuracy.
        let CommandExecutionResult {
            outputs,
            std_streams,
            exit_code,
            metadata,
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
                    }),
                };
                let end = TestRunEnd {};
                self.events
                    .span_async(start, async move { (command.await, end) })
                    .await
            }
        };

        let std_streams = std_streams
            .into_bytes()
            .await
            .context("Error accessing test output")?;
        let stdout = ExecutionStream::Inline(std_streams.stdout);
        let stderr = ExecutionStream::Inline(std_streams.stderr);

        Ok(match metadata.status {
            commands::ActionResultStatus::Success { .. } => (
                stdout,
                stderr,
                ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(0),
                },
                metadata.timing,
                outputs,
            ),
            commands::ActionResultStatus::Failure { .. } => (
                stdout,
                stderr,
                ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(1),
                },
                metadata.timing,
                outputs,
            ),
            commands::ActionResultStatus::TimedOut { duration, .. } => (
                stdout,
                stderr,
                ExecutionStatus::TimedOut { duration },
                metadata.timing,
                outputs,
            ),
            commands::ActionResultStatus::Error(_, error) => (
                ExecutionStream::Inline(Default::default()),
                ExecutionStream::Inline(format!("{:?}", error).into_bytes()),
                ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(1),
                },
                metadata.timing,
                outputs,
            ),
            commands::ActionResultStatus::ClaimRejected => {
                panic!("should be impossible for the executor to finish with a rejected claim")
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

        let io_provider = self.dice.global_data().get_io_provider();
        let project_fs = io_provider.fs();
        let executor = self
            .dice
            .get_command_executor(fs, project_fs, executor_config)?;
        let executor = CommandExecutor::new(executor, fs.clone(), executor_config.path_separator);
        Ok(executor)
    }

    async fn expand_test_executable(
        &self,
        fs: &ArtifactFs,
        test_target: &ConfiguredProvidersLabel,
        cmd: Vec<ArgValue>,
        env: HashMap<String, ArgValue>,
        pre_create_dirs: Vec<DeclaredOutput>,
        executor_override: Option<ExecutorConfigOverride>,
    ) -> anyhow::Result<ExpandedTestExecutable> {
        // NOTE: get_providers() implicitly calls this already but it's not the end of the world
        // since this will get cached in DICE.
        let node = self
            .dice
            .get_configured_target_node(test_target.target())
            .await?
            .require_compatible()?;

        let providers = self
            .dice
            .get_providers(test_target)
            .await?
            .require_compatible()?;

        let output_root =
            self.session
                .prefix()
                .join_unnormalized(ForwardRelativePathBuf::unchecked_new(
                    Uuid::new_v4().to_string(),
                ));

        let mut declared_outputs = IndexMap::<BuckOutTestPath, OutputCreationBehavior>::new();

        // NOTE: This likely needs more logic, in time.
        let mut supports_re = true;

        let cwd;
        let expanded;
        let executor;

        {
            let opts = self.session.options();

            let providers = providers.provider_collection();
            let test_info = providers
                .get_provider(ExternalRunnerTestInfoCallable::provider_id_t())
                .context("Test executable only supports ExternalRunnerTestInfo providers")?;

            cwd = if test_info.run_from_project_root() || opts.force_run_from_project_root {
                ProjectRelativePathBuf::unchecked_new("".to_owned())
            } else {
                supports_re = false;
                // For compatibility with v1,
                let cell_resolver = self.dice.get_cell_resolver().await;
                let cell = cell_resolver.get(test_target.target().pkg().cell_name())?;
                cell.path().to_owned()
            };

            let resolved_executor_override = match executor_override.as_ref() {
                Some(executor_override) => Some(
                    test_info
                        .executor_override(&executor_override.name)
                        .context("The `executor_override` provided does not exist")
                        .and_then(|o| {
                            o.command_executor_config()
                                .context("The `executor_override` is invalid")
                        })
                        .with_context(|| {
                            format!(
                                "Error processing `executor_override`: `{}`",
                                executor_override.name
                            )
                        })?,
                ),
                None => test_info
                    .default_executor()
                    .map(|o| {
                        o.command_executor_config()
                            .context("The `default_executor` is invalid")
                    })
                    .transpose()?,
            };

            executor = self
                .get_command_executor(fs, &node, resolved_executor_override.as_deref())
                .context("Error constructing CommandExecutor")?;

            let expander = Execute2RequestExpander {
                test_info: &test_info,
                output_root: &output_root,
                declared_outputs: &mut declared_outputs,
                fs: &executor.executor_fs(),
                cmd,
                env,
            };

            expanded = if test_info.use_project_relative_paths()
                || opts.force_use_project_relative_paths
            {
                expander.expand::<BaseCommandLineBuilder>()
            } else {
                supports_re = false;
                expander.expand::<AbsCommandLineBuilder>()
            }?;
        };

        let (expanded_cmd, expanded_env, inputs) = expanded;

        for output in pre_create_dirs {
            let test_path = BuckOutTestPath::new(output_root.clone(), output.name);
            declared_outputs.insert(test_path, OutputCreationBehavior::Create);
        }

        Ok(ExpandedTestExecutable {
            cwd,
            cmd: expanded_cmd,
            env: expanded_env,
            inputs,
            declared_outputs,
            supports_re,
            executor,
        })
    }

    async fn create_command_execution_request(
        &self,
        cwd: ProjectRelativePathBuf,
        cmd: Vec<String>,
        env: HashMap<String, String>,
        cmd_inputs: IndexSet<ArtifactGroup>,
        declared_outputs: IndexMap<BuckOutTestPath, OutputCreationBehavior>,
    ) -> anyhow::Result<CommandExecutionRequest> {
        let mut inputs = Vec::with_capacity(cmd_inputs.len());
        for input in &cmd_inputs {
            // we already built these before reaching out to tpx, so these should already be ready
            // hence we don't actually need to spawn these in parallel
            // TODO (T102328660): Does CommandExecutionRequest need this artifact?
            inputs.push(CommandExecutionInput::Artifact(
                self.dice.ensure_artifact_group(input).await?,
            ));
        }

        // NOTE: This looks a bit awkward, that's because fbcode's rustfmt and ours slightly
        // disagree about format here...
        let mut request = CommandExecutionRequest::new(cmd, inputs, Default::default(), env);
        request = request
            .with_test_outputs(declared_outputs)
            .with_working_directory(cwd)
            .with_local_environment_inheritance(EnvironmentInheritance::test_allowlist());
        Ok(request)
    }
}

impl Drop for BuckTestOrchestrator {
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
    env: HashMap<String, ArgValue>,
}

impl<'a> Execute2RequestExpander<'a> {
    /// Expand a command and env. Return CLI, env, and inputs.
    fn expand<B>(
        self,
    ) -> anyhow::Result<(
        Vec<String>,
        HashMap<String, String>,
        IndexSet<ArtifactGroup>,
    )>
    where
        B: CommandLineBuilderExt<'a>,
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

        let expand_arg_value = |cli_builder: &mut dyn CommandLineBuilder,
                                artifact_visitor: &mut dyn CommandLineArtifactVisitor,
                                declared_outputs: &mut IndexMap<
            BuckOutTestPath,
            OutputCreationBehavior,
        >,
                                value: ArgValue| {
            let ArgValue { content, format } = value;

            let mut cli_builder = CommandLineBuilderFormatWrapper {
                inner: cli_builder,
                format,
            };

            match content {
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::Verbatim(v)) => {
                    v.add_to_command_line(&mut cli_builder)?;
                }
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::ArgHandle(h)) => {
                    let arg = cli_args_for_interpolation
                        .get(h.0)
                        .with_context(|| format!("Invalid ArgHandle: {:?}", h))?;

                    arg.visit_artifacts(artifact_visitor)?;
                    arg.add_to_command_line(&mut cli_builder)?;
                }
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::EnvHandle(h)) => {
                    let arg = env_for_interpolation
                        .get(h.0.as_str())
                        .with_context(|| format!("Invalid EnvHandle: {:?}", h))?;
                    arg.visit_artifacts(artifact_visitor)?;
                    arg.add_to_command_line(&mut cli_builder)?;
                }
                ArgValueContent::DeclaredOutput(output) => {
                    let test_path = BuckOutTestPath::new(self.output_root.to_owned(), output.name);
                    let path = self
                        .fs
                        .fs()
                        .buck_out_path_resolver()
                        .resolve_test(&test_path);
                    let path = cli_builder.resolve_project_path(path)?.into_string();
                    cli_builder.add_arg_string(path);
                    declared_outputs.insert(test_path, OutputCreationBehavior::Parent);
                }
            };

            anyhow::Ok(())
        };

        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();

        let mut builder = B::new(self.fs);
        for var in self.cmd {
            expand_arg_value(
                &mut builder,
                &mut artifact_visitor,
                self.declared_outputs,
                var,
            )?;
        }

        let expanded_cmd = builder.build();

        let expanded_env = self
            .env
            .into_iter()
            .map(|(k, v)| {
                let mut builder = B::new(self.fs);
                expand_arg_value(
                    &mut builder,
                    &mut artifact_visitor,
                    self.declared_outputs,
                    v,
                )?;
                anyhow::Ok((k, builder.build().join(" ")))
            })
            .collect::<Result<HashMap<_, _>, _>>()?;

        let inputs = artifact_visitor.inputs;

        Ok((expanded_cmd, expanded_env, inputs))
    }
}

trait CommandLineBuilderExt<'a>: CommandLineBuilder + 'a {
    fn new(fs: &'a ExecutorFs) -> Self;
    fn build(self) -> Vec<String>;
}

impl<'a> CommandLineBuilderExt<'a> for BaseCommandLineBuilder<'a> {
    fn new(fs: &'a ExecutorFs) -> Self {
        Self::new(fs)
    }

    fn build(self) -> Vec<String> {
        Self::build(self)
    }
}

impl<'a> CommandLineBuilderExt<'a> for AbsCommandLineBuilder<'a> {
    fn new(fs: &'a ExecutorFs) -> Self {
        Self::new(fs)
    }

    fn build(self) -> Vec<String> {
        Self::build(self)
    }
}

struct CommandLineBuilderFormatWrapper<'a> {
    inner: &'a mut dyn CommandLineBuilder,
    format: Option<String>,
}

impl<'a> CommandLineBuilderContext for CommandLineBuilderFormatWrapper<'a> {
    fn resolve_project_path(
        &self,
        path: ProjectRelativePathBuf,
    ) -> anyhow::Result<CommandLineLocation> {
        self.inner.resolve_project_path(path)
    }

    fn fs(&self) -> &ExecutorFs {
        self.inner.fs()
    }

    fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
        self.inner.next_macro_file_path()
    }
}

impl<'a> CommandLineBuilder for CommandLineBuilderFormatWrapper<'a> {
    fn add_arg_string(&mut self, s: String) {
        let s = if let Some(format) = &self.format {
            format.replace("{}", &s)
        } else {
            s
        };

        self.inner.add_arg_string(s);
    }
}

struct ExpandedTestExecutable {
    cwd: ProjectRelativePathBuf,
    cmd: Vec<String>,
    env: HashMap<String, String>,
    inputs: IndexSet<ArtifactGroup>,
    supports_re: bool,
    declared_outputs: IndexMap<BuckOutTestPath, OutputCreationBehavior>,
    executor: CommandExecutor,
}

fn create_prepare_for_local_execution_result(
    fs: &ArtifactFs,
    request: CommandExecutionRequest,
) -> PrepareForLocalExecutionResult {
    let relative_cwd = request
        .working_directory()
        .unwrap_or_else(|| ProjectRelativePath::unchecked_new(""));
    let cwd = fs.fs().resolve(relative_cwd);
    let cmd = request.args().map(String::from);

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
    inner: HashMap<String, String>,
}

impl LossyEnvironment {
    fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    fn into_inner(self) -> HashMap<String, String> {
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
}

#[cfg(test)]
mod tests {
    use buck2_common::dice::cells::HasCellResolver;
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::fs::project::ProjectFilesystemTemp;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use futures::channel::mpsc;
    use futures::channel::mpsc::UnboundedReceiver;
    use futures::future;
    use futures::stream::TryStreamExt;
    use test_api::data::testing::ConfiguredTargetHandleExt;
    use test_api::data::TestStatus;

    use super::*;
    use crate::context::SetBuildContextData;

    fn make() -> (
        BuckTestOrchestrator,
        UnboundedReceiver<anyhow::Result<TestResultOrExitCode>>,
    ) {
        let fs = ProjectFilesystemTemp::new().unwrap();

        let cell_resolver = CellResolver::of_names_and_paths(&[(
            CellName::unchecked_new("cell".to_owned()),
            ProjectRelativePathBuf::unchecked_new("cell".to_owned()),
        )]);
        let buckout_path = ForwardRelativePathBuf::unchecked_new("buck_out/v2".into());
        let dice = DiceBuilder::new()
            .set_data(|d| d.set_testing_io_provider(&fs))
            .build(UserComputationData::new());
        dice.set_buck_out_path(Some(buckout_path));
        dice.set_cell_resolver(cell_resolver);

        let dice = dice.commit();

        let (sender, receiver) = mpsc::unbounded();

        (
            BuckTestOrchestrator::from_parts(
                dice,
                Arc::new(TestSession::new(Default::default())),
                sender,
                EventDispatcher::null(),
            ),
            receiver,
        )
    }

    #[tokio::test]
    async fn orchestrator_results() -> anyhow::Result<()> {
        let (orchestrator, channel) = make();

        let jobs = async {
            orchestrator
                .report_test_result(TestResult {
                    target: ConfiguredTargetHandle::testing_new(0),
                    status: TestStatus::PASS,
                    stdout: "1".to_owned(),
                    stderr: "2".to_owned(),
                    msg: None,
                    name: "First - test".to_owned(),
                    duration: Some(Duration::from_micros(1)),
                })
                .await?;

            orchestrator
                .report_test_result(TestResult {
                    target: ConfiguredTargetHandle::testing_new(0),
                    status: TestStatus::FAIL,
                    stdout: "3".to_owned(),
                    stderr: "4".to_owned(),
                    msg: None,
                    name: "Second - test".to_owned(),
                    duration: Some(Duration::from_micros(2)),
                })
                .await?;

            orchestrator.end_of_test_results(0).await?;

            anyhow::Ok(())
        };

        let ((), results) = future::try_join(jobs, channel.try_collect::<Vec<_>>()).await?;

        assert_eq!(
            results,
            vec![
                TestResultOrExitCode::TestResult(TestResult {
                    target: ConfiguredTargetHandle::testing_new(0),

                    status: TestStatus::PASS,
                    stdout: "1".to_owned(),
                    stderr: "2".to_owned(),
                    msg: None,
                    name: "First - test".to_owned(),
                    duration: Some(Duration::from_micros(1))
                }),
                TestResultOrExitCode::TestResult(TestResult {
                    target: ConfiguredTargetHandle::testing_new(0),

                    status: TestStatus::FAIL,
                    stdout: "3".to_owned(),
                    stderr: "4".to_owned(),
                    msg: None,
                    name: "Second - test".to_owned(),
                    duration: Some(Duration::from_micros(2))
                }),
                TestResultOrExitCode::ExitCode(0),
            ]
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_orchestrator_channel_drop() {
        let (orchestrator, channel) = make();
        drop(orchestrator);

        let res = channel.try_collect::<Vec<_>>().await;
        assert!(res.is_err());
    }

    #[tokio::test]
    async fn test_orchestrator_closes_channel() -> anyhow::Result<()> {
        let (orchestrator, channel) = make();
        let sender = orchestrator.results_channel.clone();
        orchestrator.end_of_test_results(1).await?;

        assert!(sender.is_closed());
        drop(channel);

        Ok(())
    }
}
