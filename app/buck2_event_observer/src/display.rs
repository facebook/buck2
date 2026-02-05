/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// TODO(brasselsprouts): move this onto the original core types and convert in events

use std::fmt;
use std::fmt::Write;
use std::sync::Arc;
use std::time::Duration;

use buck2_common::convert::ProstDurationExt;
use buck2_data::ActionKey;
use buck2_data::ActionName;
use buck2_data::AnonTarget;
use buck2_data::BxlFunctionKey;
use buck2_data::BxlFunctionLabel;
use buck2_data::ConfiguredTargetLabel;
use buck2_data::FileWatcherKind;
use buck2_data::TargetLabel;
use buck2_data::action_key;
use buck2_data::span_start_event::Data;
use buck2_error::BuckErrorContext;
use buck2_events::BuckEvent;
use buck2_test_api::data::TestStatus;
use buck2_util::commas::commas;
use buck2_util::truncate::truncate;
use dupe::Dupe;
use starlark_map::ordered_set::OrderedSet;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;
use superconsole::style::Stylize;
use termwiz::escape::Action;
use termwiz::escape::ControlCode;

use crate::action_sub_error_display::ActionSubErrorDisplay;
use crate::fmt_duration;
use crate::verbosity::Verbosity;
use crate::what_ran::command_to_string;
use crate::what_ran::worker_command_as_fallback_to_string;

#[derive(Copy, Clone, Dupe)]
pub struct TargetDisplayOptions {
    with_configuration: bool,
}

impl TargetDisplayOptions {
    pub fn for_log() -> Self {
        Self {
            with_configuration: true,
        }
    }

    pub fn for_build_report() -> Self {
        Self {
            with_configuration: true,
        }
    }

    pub fn for_console(with_configuration: bool) -> Self {
        Self { with_configuration }
    }

    pub fn for_chrome_trace() -> Self {
        Self {
            with_configuration: false,
        }
    }
}

pub fn display_configured_target_label(
    ctl: &ConfiguredTargetLabel,
    opts: TargetDisplayOptions,
) -> buck2_error::Result<String> {
    if let ConfiguredTargetLabel {
        label: Some(TargetLabel { package, name }),
        configuration: Some(configuration),
        // We never display execution configurations at the moment
        execution_configuration: _,
    } = ctl
    {
        Ok(if opts.with_configuration {
            format!("{}:{} ({})", package, name, configuration.full_name)
        } else {
            format!("{package}:{name}")
        })
    } else {
        Err(ParseEventError::InvalidConfiguredTargetLabel.into())
    }
}

fn display_configured_target_label_opt(
    ctl: Option<&ConfiguredTargetLabel>,
    opts: TargetDisplayOptions,
) -> buck2_error::Result<String> {
    Ok(match ctl {
        Some(ctl) => display_configured_target_label(ctl, opts)?,
        None => {
            // Should never happen, but better not error here.
            "unknown target".to_owned()
        }
    })
}

pub fn display_anon_target(ctl: &AnonTarget) -> buck2_error::Result<String> {
    if let AnonTarget {
        name: Some(TargetLabel { package, name }),
        // We currently never display execution configurations, only normal configurations
        execution_configuration: _,
        hash,
    } = ctl
    {
        Ok(format!("{package}:{name}@{hash}"))
    } else {
        Err(ParseEventError::InvalidAnonTarget.into())
    }
}

pub fn display_analysis_target(
    target: &buck2_data::analysis_start::Target,
    opts: TargetDisplayOptions,
) -> buck2_error::Result<String> {
    use buck2_data::analysis_start::Target;
    match target {
        Target::StandardTarget(ctl) => display_configured_target_label(ctl, opts),
        Target::AnonTarget(anon) => display_anon_target(anon),
        Target::DynamicLambda(dynamic) => {
            use buck2_data::dynamic_lambda_owner::Owner;
            match dynamic
                .owner
                .as_ref()
                .buck_error_context("Missing `owner`")?
            {
                Owner::TargetLabel(target_label) => {
                    display_configured_target_label(target_label, opts)
                }
                Owner::BxlKey(bxl_key) => display_bxl_key(bxl_key),
                Owner::AnonTarget(anon_target) => display_anon_target(anon_target),
            }
        }
    }
}

pub fn display_bxl_key(ctl: &BxlFunctionKey) -> buck2_error::Result<String> {
    if let BxlFunctionKey {
        label: Some(BxlFunctionLabel { bxl_path, name }),
    } = ctl
    {
        Ok(format!("{bxl_path}:{name}"))
    } else {
        Err(ParseEventError::MissingBxlFunctionLabel.into())
    }
}

pub fn display_action_owner(
    owner: &action_key::Owner,
    opts: TargetDisplayOptions,
) -> buck2_error::Result<String> {
    match owner {
        action_key::Owner::TargetLabel(target_label)
        | action_key::Owner::TestTargetLabel(target_label)
        | action_key::Owner::LocalResourceSetup(target_label) => {
            display_configured_target_label(target_label, opts)
        }
        action_key::Owner::BxlKey(bxl_key) => display_bxl_key(bxl_key),
        action_key::Owner::AnonTarget(anon_target) => display_anon_target(anon_target),
    }
}

pub fn display_action_key(
    action_key: &ActionKey,
    opts: TargetDisplayOptions,
) -> buck2_error::Result<String> {
    if let ActionKey {
        owner: Some(owner), ..
    } = action_key
    {
        display_action_owner(owner, opts)
    } else {
        Err(ParseEventError::MissingActionOwner.into())
    }
}

pub fn display_action_name_opt(name: Option<&ActionName>) -> String {
    match name {
        Some(name) if name.identifier.is_empty() => name.category.clone(),
        Some(name) => format!("{} {}", name.category, name.identifier),
        _ => "unknown".to_owned(),
    }
}

pub fn display_action_identity(
    action_key: Option<&ActionKey>,
    name: Option<&ActionName>,
    opts: TargetDisplayOptions,
) -> buck2_error::Result<String> {
    let key_string = match action_key {
        Some(key) => display_action_key(key, opts),
        None => Err(ParseEventError::MissingActionKey.into()),
    }?;
    let action_string = match name {
        Some(ActionName {
            category,
            identifier,
        }) if !identifier.is_empty() => format!(" ({category} {identifier})"),
        Some(ActionName { category, .. }) => format!(" ({category})"),
        None => String::new(),
    };

    Ok(format!("{key_string}{action_string}"))
}

/// Formats event payloads for display.
pub fn display_event(event: &BuckEvent, opts: TargetDisplayOptions) -> buck2_error::Result<String> {
    let res: buck2_error::Result<_> = try {
        let data = match event.data() {
            buck2_data::buck_event::Data::SpanStart(start) => start.data.as_ref().unwrap(),
            _ => Err(buck2_error::Error::from(ParseEventError::UnexpectedEvent))?,
        };

        let res: buck2_error::Result<_> = match data {
            Data::ActionExecution(action) => match &action.key {
                Some(key) => {
                    let string = display_action_key(key, opts)?;
                    let action_descriptor = display_action_name_opt(action.name.as_ref());
                    Ok(format!("{string} -- action ({action_descriptor})"))
                }
                None => Err(ParseEventError::MissingActionKey.into()),
            },
            Data::FinalMaterialization(materialization) => {
                let build = &materialization
                    .artifact
                    .as_ref()
                    .ok_or(ParseEventError::MissingArtifact)?;

                let key = display_action_key(
                    build
                        .key
                        .as_ref()
                        .ok_or(ParseEventError::MissingActionKey)?,
                    opts,
                )?;
                let path = {
                    if build.path.is_empty() {
                        Err(ParseEventError::MissingMaterializationPath)
                    } else {
                        Ok(&build.path)
                    }
                }?;
                Ok(format!("{key} -- materializing `{path}`"))
            }
            Data::Analysis(analysis) => match &analysis.target {
                Some(target) => {
                    let target = display_analysis_target(target, opts)?;
                    Ok(format!("{target} -- running analysis"))
                }
                None => Err(ParseEventError::MissingConfiguredTargetLabel.into()),
            },
            Data::AnalysisStage(info) => {
                let stage = info
                    .stage
                    .as_ref()
                    .buck_error_context("analysis stage is missing")?;
                let stage = display_analysis_stage(stage);
                Ok(stage.into())
            }
            Data::AnalysisResolveQueries(resolve_queries) => Ok(format!(
                "{} -- analysis queries",
                display_configured_target_label_opt(
                    resolve_queries.standard_target.as_ref(),
                    opts
                )?
            )),
            Data::LoadPackage(load) => Ok(format!("{} -- loading package file tree", load.path)),
            Data::Load(load) => Ok(format!("{} -- evaluating build file", load.module_id)),
            Data::ExecutorStage(info) => {
                let stage = info
                    .stage
                    .as_ref()
                    .buck_error_context("executor stage is missing")?;
                let stage =
                    display_executor_stage(stage).buck_error_context("unknown executor stage")?;
                Ok(stage.into())
            }
            Data::TestDiscovery(discovery) => Ok(format!(
                "Test {} -- discovering tests",
                discovery.suite_name
            )),
            Data::TestStart(start) => match &start.suite {
                Some(suite) => {
                    let tests = {
                        if suite.test_names.len() < 100 {
                            suite.test_names.join(" ")
                        } else {
                            format!(
                                "{}...({} more)",
                                suite.test_names.first().unwrap(),
                                suite.test_names.len() - 1
                            )
                        }
                    };
                    Ok(format!("Test {} -- {}", suite.suite_name, tests))
                }
                None => Err(ParseEventError::MissingSuiteName.into()),
            },
            Data::CommandCritical(..) => Err(ParseEventError::UnexpectedEvent.into()),
            Data::Command(..) => Err(ParseEventError::UnexpectedEvent.into()),
            Data::FileWatcher(x) => Ok(format!(
                "Syncing file changes via {}",
                display_file_watcher(x.provider)
            )),
            Data::MatchDepFiles(buck2_data::MatchDepFilesStart {
                checking_filtered_inputs,
                remote_cache,
            }) => {
                let location = if *remote_cache {
                    "remote cache"
                } else {
                    "local"
                };
                let detail = if *checking_filtered_inputs {
                    "full"
                } else {
                    "partial"
                };
                Ok(format!("dep_files({detail},{location})"))
            }
            Data::SharedTask(buck2_data::SharedTaskStart { owner_trace_id }) => Ok(format!(
                "Waiting on task from another command: {owner_trace_id}"
            )),
            Data::CacheUpload(_) => Ok("upload (action)".to_owned()),
            Data::DepFileUpload(_) => Ok("upload (dep_file)".to_owned()),
            Data::CreateOutputSymlinks(..) => Ok("Creating output symlinks".to_owned()),
            Data::InstallEventInfo(info) => Ok(format!(
                "Sending {} at path {}",
                info.artifact_name, info.file_path
            )),
            Data::DiceStateUpdate(..) => Ok("Syncing changes to graph".to_owned()),
            Data::Materialization(..) => Ok("materializing".to_owned()),
            Data::DiceCriticalSection(..) => Err(ParseEventError::UnexpectedEvent.into()),
            Data::DiceBlockConcurrentCommand(cmd) => Ok(format!(
                "Waiting for command [{}] to finish",
                truncate(&cmd.cmd_args, 200),
            )),
            Data::DiceSynchronizeSection(..) => Ok("Synchronizing buck2 internal state".to_owned()),
            Data::DiceCleanup(..) => Ok("Cleaning up graph state".to_owned()),
            Data::ExclusiveCommandWait(buck2_data::ExclusiveCommandWaitStart { command_name }) => {
                if let Some(name) = command_name {
                    Ok(format!("Waiting for command [{name}] to finish"))
                } else {
                    Ok("Waiting for dice".to_owned())
                }
            }
            Data::DeferredPreparationStage(prep) => {
                use buck2_data::deferred_preparation_stage_start::Stage;
                match prep.stage.as_ref().buck_error_context("Missing `stage`")? {
                    Stage::MaterializedArtifacts(_) => Ok("local_materialize_inputs".to_owned()),
                }
            }
            Data::DynamicLambda(lambda) => {
                use buck2_data::dynamic_lambda_start::Owner;

                let label = match lambda
                    .owner
                    .as_ref()
                    .buck_error_context("Missing `owner`")?
                {
                    Owner::TargetLabel(target_label) => {
                        display_configured_target_label(target_label, opts)
                    }
                    Owner::BxlKey(bxl_key) => display_bxl_key(bxl_key),
                    Owner::AnonTarget(anon_target) => display_anon_target(anon_target),
                }?;

                Ok(format!("{label} -- dynamic analysis"))
            }
            Data::BxlExecution(execution) => {
                Ok(format!("Executing BXL script `{}`", execution.name))
            }
            Data::BxlDiceInvocation(..) => Ok("Waiting for graph computations".to_owned()),
            Data::ReUpload(..) => Ok("re_upload".to_owned()),
            Data::ConnectToInstaller(buck2_data::ConnectToInstallerStart { tcp_port }) => {
                Ok(format!("Connecting to installer on port {tcp_port}"))
            }
            Data::Fake(fake) => Ok(format!("{} -- speak of the devil", fake.caramba)),
            Data::LocalResources(..) => Ok("Local resources setup".to_owned()),
            Data::ReleaseLocalResources(..) => Ok("Releasing local resources".to_owned()),
            Data::BxlEnsureArtifacts(..) => Err(ParseEventError::UnexpectedEvent.into()),
            Data::ActionErrorHandlerExecution(..) => {
                Ok("Running error handler on action failure".to_owned())
            }
            Data::CqueryUniverseBuild(..) => Ok("Building cquery universe".to_owned()),
            Data::ComputeDetailedAggregatedMetrics(..) => {
                Ok("Computing detailed aggregated metrics".to_owned())
            }
        };

        // This shouldn't really be necessary, but that's how try blocks work :(
        res?
    };

    res.with_buck_error_context(|| InvalidBuckEvent(Arc::new(event.clone())).to_string())
}

fn display_file_watcher(provider: i32) -> &'static str {
    match buck2_data::FileWatcherProvider::try_from(provider) {
        Ok(buck2_data::FileWatcherProvider::Watchman) => "Watchman",
        Ok(buck2_data::FileWatcherProvider::RustNotify) => "notify",
        Ok(buck2_data::FileWatcherProvider::FsHashCrawler) => "fs_hash_crawler",
        Ok(buck2_data::FileWatcherProvider::EdenFs) => "EdenFS",
        Err(_) => "unknown mechanism",
    }
}

pub fn display_analysis_stage(stage: &buck2_data::analysis_stage_start::Stage) -> &'static str {
    use buck2_data::analysis_stage_start::Stage;

    match stage {
        Stage::EvaluateRule(()) => "evaluate_rule",
    }
}

pub fn display_file_watcher_end(file_watcher_end: &buck2_data::FileWatcherEnd) -> Vec<String> {
    const MAX_PRINT_MESSAGES: usize = 3;
    let mut res = Vec::new();

    if let Some(stats) = &file_watcher_end.stats {
        // The `FileWatcherEvent` contain no duplicates. However, there can be two distinct events
        // for the same file, e.g. `foo` was deleted as a file and then created as a directory.
        //
        // It looks really odd to print "File changed: foo" twice, so we dedupe user messages.
        //
        // If there are more file change records than we passed over, and some of the omitted ones are
        // duplicates on the same file, then our "additional file change events" count is slightly high.
        // Shouldn't be a big deal in practice, since it is rare, and fairly big numbers already.

        let mut to_print = OrderedSet::new();
        for x in &stats.events {
            to_print.insert((&x.path, x.kind()));
        }
        for (path, kind) in to_print.iter().take(MAX_PRINT_MESSAGES) {
            let kind = match kind {
                FileWatcherKind::Directory => "Directory",
                FileWatcherKind::File | FileWatcherKind::Symlink => "File",
            };
            res.push(format!("{kind} changed: {path}"));
        }
        let unprinted_paths =
            // those we have the names of but didn't print
            to_print.len().saturating_sub(MAX_PRINT_MESSAGES) +
                // plus those we didn't get the names for
                (stats.events_processed as usize).saturating_sub(stats.events.len());
        if unprinted_paths > 0 {
            res.push(format!("{unprinted_paths} additional file change events"));
        }

        if let Some(fresh_instance) = &stats.fresh_instance_data {
            let file_watcher = if stats.watchman_version.is_some() {
                "Watchman"
            } else {
                "File Watcher"
            };

            let mut msg = format!("{file_watcher} fresh instance: ");
            let mut comma = commas();
            if fresh_instance.new_mergebase {
                comma(&mut msg).unwrap();
                write!(&mut msg, "new mergebase").unwrap();
            }
            if fresh_instance.cleared_dice {
                comma(&mut msg).unwrap();
                write!(&mut msg, "cleared graph state").unwrap();
            }
            if fresh_instance.cleared_dep_files {
                comma(&mut msg).unwrap();
                write!(&mut msg, "cleared dep files").unwrap();
            }
            res.push(msg);
        }
    }

    res
}

pub fn display_executor_stage(
    stage: &buck2_data::executor_stage_start::Stage,
) -> Option<&'static str> {
    use buck2_data::executor_stage_start::Stage;

    let label = match stage {
        Stage::Prepare(..) => "prepare",
        Stage::CacheQuery(cache_query) => {
            match buck2_data::CacheType::try_from(cache_query.cache_type).unwrap() {
                buck2_data::CacheType::ActionCache => "re_action_cache",
                buck2_data::CacheType::RemoteDepFileCache => "re_dep_file_cache",
            }
        }
        Stage::CacheHit(..) => "re_download",
        Stage::Re(re) => {
            use buck2_data::re_stage::Stage;

            match re.stage.as_ref()? {
                Stage::Execute(..) => "re_execute",
                Stage::Download(..) => "re_download",
                Stage::Queue(..) => "re_queued",
                Stage::QueueOverQuota(..) => "re_queued(over_quota)",
                Stage::QueueAcquiringDependencies(..) => "re_queued(waiting_on_deps)",
                Stage::QueueNoWorkerAvailable(..) => "re_queued(no_workers)",
                Stage::QueueCancelled(..) => "re_cancelled",
                Stage::WorkerDownload(..) => "re_worker_download",
                Stage::WorkerUpload(..) => "re_worker_upload",
                Stage::Unknown(..) => "re_unknown",
                Stage::MaterializeFailedInputs(..) => "re_materialize_failed_inputs",
                Stage::BeforeActionExecution(_) => "initialize_re_worker",
                Stage::AfterActionExecution(_) => "release_re_worker",
            }
        }
        Stage::Local(local) => {
            use buck2_data::local_stage::Stage;

            match local.stage.as_ref()? {
                Stage::Queued(..) => "local_queued",
                Stage::Execute(..) => "local_execute",
                Stage::MaterializeInputs(..) => "local_materialize_inputs",
                Stage::PrepareOutputs(_) => "local_prepare_outputs",
                Stage::AcquireLocalResource(_) => "acquire_local_resource",
                Stage::WorkerInit(_) => "initialize_worker",
                Stage::WorkerExecute(_) => "worker_execute",
                Stage::WorkerQueued(..) => "worker_queued",
                Stage::WorkerWait(_) => "initialize_worker",
            }
        }
    };

    Some(label)
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum ParseEventError {
    #[error("Missing configured target label")]
    MissingConfiguredTargetLabel,
    #[error("Invalid configured target label")]
    InvalidConfiguredTargetLabel,
    #[error("Invalid anon target")]
    InvalidAnonTarget,
    #[error("Missing action key")]
    MissingActionKey,
    #[error("Missing suite name")]
    MissingSuiteName,
    #[error("Missing artifact")]
    MissingArtifact,
    #[error("Missing materialization path")]
    MissingMaterializationPath,
    #[error("Missing action owner")]
    MissingActionOwner,
    #[error("Missing bxl function label")]
    MissingBxlFunctionLabel,
    #[error("Unexpected event")]
    UnexpectedEvent,
}

#[derive(buck2_error::Error, Debug)]
#[error("Invalid buck event: `{0:?}`")]
#[buck2(tag = Tier0)]
pub struct InvalidBuckEvent(pub Arc<BuckEvent>);

pub fn format_test_result(
    test_result: &buck2_data::TestResult,
    verbosity: Verbosity,
) -> buck2_error::Result<Option<Lines>> {
    let buck2_data::TestResult {
        name,
        status,
        duration,
        details,
        ..
    } = test_result;
    let status = TestStatus::try_from(*status)?;

    // Pass results normally have no details, unless the --print-passing-details is set.
    // Do not display anything for passing tests unless verbosity is high or details are present
    // to avoid cluttering the UI with unimportant test results.
    if matches!(&status, TestStatus::PASS | TestStatus::LISTING_SUCCESS)
        && details.is_empty()
        && !verbosity.print_all_commands()
    {
        return Ok(None);
    }

    let prefix = match status {
        TestStatus::FAIL => Span::new_styled("✗ Fail".to_owned().red()),
        TestStatus::SKIP => Span::new_styled("↷ Skip".to_owned().cyan()),
        TestStatus::OMITTED => Span::new_styled("\u{20E0} Omitted".to_owned().cyan()),
        TestStatus::FATAL => Span::new_styled("⚠ Fatal".to_owned().red()),
        TestStatus::TIMEOUT => Span::new_styled("✉ Timeout".to_owned().cyan()),
        TestStatus::INFRA_FAILURE => Span::new_styled("🛠 Infra Failure".to_owned().magenta()),
        TestStatus::PASS => Span::new_styled("✓ Pass".to_owned().green()),
        TestStatus::LISTING_SUCCESS => Span::new_styled("✓ Listing success".to_owned().green()),
        TestStatus::UNKNOWN => Span::new_styled("? Unknown".to_owned().cyan()),
        TestStatus::RERUN => Span::new_styled("↻ Rerun".to_owned().cyan()),
        TestStatus::LISTING_FAILED => Span::new_styled("⚠ Listing failed".to_owned().red()),
    }?;
    let mut base = Line::from_iter([prefix, Span::new_unstyled(format!(": {name}",))?]);

    if let Some(duration) = duration {
        if let Ok(duration) = Duration::try_from(*duration) {
            base.push(Span::new_unstyled(format!(
                " ({})",
                fmt_duration::fmt_duration(duration)
            ))?);
        }
    }
    // If a test has details, we always show them. It's the test runner's
    // responsibility to withhold details when these are not relevant.
    // For instance, tpx will always withhold details of passing tests
    // unless the --print-passing-details is set.
    let mut lines = vec![base];
    if !details.is_empty() {
        lines.append(&mut Lines::from_multiline_string(details, Default::default()).0);
    }
    Ok(Some(Lines(lines)))
}

pub struct ActionErrorDisplay<'a> {
    pub action_id: String,
    pub reason: String,
    pub command: Option<&'a buck2_data::CommandExecutionDetails>,
    pub error_diagnostics: Option<&'a buck2_data::ActionErrorDiagnostics>,
}

fn strip_trailing_newline(stream_contents: &str) -> &str {
    match stream_contents.strip_suffix('\n') {
        None => stream_contents,
        Some(s) => s.strip_suffix('\r').unwrap_or(s),
    }
}

impl ActionErrorDisplay<'_> {
    /// Format the error message in a way that is suitable for use with the build report
    ///
    /// The output may include terminal colors that need to be sanitized.
    pub fn simple_format_for_build_report(&self) -> String {
        let s = self.simple_format_inner(None::<&'static mut dyn for<'x> FnMut(&'x str) -> String>);
        sanitize_output_colors(s.as_bytes())
    }

    /// Format the error message in a way that is suitable for use with the simpleconsole
    ///
    /// The output may include terminal colors that need to be sanitized
    pub fn simple_format_with_timestamps(
        &self,
        with_timestamps: impl FnMut(&str) -> String,
    ) -> String {
        self.simple_format_inner(Some(with_timestamps))
    }

    fn simple_format_inner(
        &self,
        mut with_timestamps: Option<impl FnMut(&str) -> String>,
    ) -> String {
        let mut s = String::new();
        macro_rules! append {
            ($fmt:expr $(, $args:expr)*) => {{
                let mut message = format!($fmt $(, $args)*);
                if let Some(with_timestamps) = &mut with_timestamps {
                    message = with_timestamps(&message);
                }
                writeln!(s, "{message}").unwrap();
            }};
        }
        append!("Action failed: {}", self.action_id);
        append!("{}", self.reason);
        let Some(command_failed) = &self.command else {
            return s;
        };
        if let Some(command_kind) = command_failed.command_kind.as_ref() {
            use buck2_data::command_execution_kind::Command;
            match command_kind.command.as_ref() {
                Some(Command::LocalCommand(local_command)) => {
                    append!("Local command: {}", command_to_string(local_command));
                }
                Some(Command::WorkerCommand(worker_command)) => {
                    append!(
                        "Local worker command: {}",
                        worker_command_as_fallback_to_string(worker_command)
                    );
                }
                Some(Command::WorkerInitCommand(worker_init_command)) => {
                    append!(
                        "Local worker initialization command: {}",
                        command_to_string(worker_init_command)
                    );
                }
                Some(Command::RemoteCommand(remote_command)) => {
                    if buck2_core::is_open_source() {
                        append!("Remote action digest: '{}'", remote_command.action_digest);
                    } else {
                        append!(
                            "Remote action{}, reproduce with: `frecli cas download-action {}`",
                            if remote_command.cache_hit {
                                " cache hit"
                            } else {
                                ""
                            },
                            remote_command.action_digest
                        );
                    }
                }
                Some(Command::OmittedLocalCommand(..)) | None => {
                    // Nothing to show in this case.
                }
            };
        }

        let mut append_stream = |name, contents: &str| {
            if contents.is_empty() {
                append!("{name}: <empty>");
            } else {
                append!("{name}:");
                let contents = strip_trailing_newline(contents);
                writeln!(s, "{contents}").unwrap();
            }
        };

        append_stream("Stdout", &command_failed.cmd_stdout);
        append_stream("Stderr", &command_failed.cmd_stderr);

        if let Some(additional_info) = &command_failed.additional_message {
            if !additional_info.is_empty() {
                append_stream("Info", additional_info);
            }
        }

        if let Some(error_diagnostics) = self.error_diagnostics {
            match error_diagnostics.data.as_ref().unwrap() {
                buck2_data::action_error_diagnostics::Data::SubErrors(sub_errors) => {
                    let sub_errors = &sub_errors.sub_errors;
                    if !sub_errors.is_empty() {
                        let mut all_sub_errors = String::new();
                        for sub_error in sub_errors {
                            // Display errors based on show_in_stderr flag is true
                            if sub_error.show_in_stderr {
                                if let Some(display_msg) = sub_error.display() {
                                    writeln!(all_sub_errors, "- {}", display_msg).unwrap();
                                }
                            }
                        }
                        if !all_sub_errors.is_empty() {
                            append_stream(
                                "\nAction sub-errors produced by error handlers",
                                &all_sub_errors,
                            );
                        }
                    }
                }
                buck2_data::action_error_diagnostics::Data::HandlerInvocationError(error) => {
                    append_stream("\nCould not produce error diagnostics", error);
                }
            };
        }
        s
    }
}

pub fn get_action_error_reason(error: &buck2_data::ActionError) -> buck2_error::Result<String> {
    use buck2_data::action_error::Error;

    Ok(
        match error
            .error
            .as_ref()
            .buck_error_context("Internal error: Missing error in action error")?
        {
            Error::MissingOutputs(missing_outputs) => {
                format!("Required outputs are missing: {}", missing_outputs.message)
            }
            Error::Unknown(error_string) => error_string.to_owned(),
            Error::CommandExecutionError(buck2_data::CommandExecutionError {}) => {
                match &error.last_command {
                    Some(c) => failure_reason_for_command_execution(c)?,
                    None => "Unexpected command status".to_owned(),
                }
            }
        },
    )
}

pub fn display_action_error(
    error: &buck2_data::ActionError,
    opts: TargetDisplayOptions,
) -> buck2_error::Result<ActionErrorDisplay<'_>> {
    let command = error.last_command.as_ref().and_then(|c| c.details.as_ref());

    let reason = get_action_error_reason(error)?;

    Ok(ActionErrorDisplay {
        action_id: display_action_identity(error.key.as_ref(), error.name.as_ref(), opts)?,
        reason,
        command,
        error_diagnostics: error.error_diagnostics.as_ref(),
    })
}

fn failure_reason_for_command_execution(
    command_execution: &buck2_data::CommandExecution,
) -> buck2_error::Result<String> {
    use buck2_data::command_execution::Cancelled;
    use buck2_data::command_execution::Error;
    use buck2_data::command_execution::Failure;
    use buck2_data::command_execution::Status;
    use buck2_data::command_execution::Success;
    use buck2_data::command_execution::Timeout;
    use buck2_data::command_execution::WorkerFailure;

    let command = command_execution
        .details
        .as_ref()
        .buck_error_context("CommandExecution did not include a `command`")?;

    let status = command_execution
        .status
        .as_ref()
        .buck_error_context("CommandExecution did not include a `status`")?;

    let locality = if let Some(command_kind) = command.command_kind.as_ref() {
        use buck2_data::command_execution_kind::Command;
        match command_kind.command {
            Some(Command::RemoteCommand(..)) => "Remote ",
            Some(Command::LocalCommand(..)) | Some(Command::OmittedLocalCommand(..)) => "Local ",
            Some(Command::WorkerInitCommand(..)) => "Local Worker Initialization ",
            Some(Command::WorkerCommand(..)) => "Local Worker ",
            None => "",
        }
    } else {
        ""
    };

    Ok(match status {
        Status::Success(Success {}) => "Unexpected command status".to_owned(),
        Status::Failure(Failure {}) | Status::WorkerFailure(WorkerFailure {}) => {
            struct OptionalExitCode {
                code: Option<i32>,
            }

            impl fmt::Display for OptionalExitCode {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    match self.code {
                        Some(code) => {
                            if (i16::MIN as i32) < code && code < (i16::MAX as i32) {
                                write!(f, "{code}")
                            } else {
                                let code = code as u32;
                                write!(f, "{code} ({code:#X})")
                            }
                        }
                        None => write!(f, "<no exit code>"),
                    }
                }
            }

            format!(
                "{}command returned non-zero exit code {}",
                locality,
                OptionalExitCode {
                    code: command.signed_exit_code
                }
            )
        }
        Status::Timeout(Timeout { duration }) => {
            let duration = duration
                .as_ref()
                .buck_error_context("Timeout did not include a `duration`")?
                .try_into_duration()
                .buck_error_context("Timeout `duration` was invalid")?;

            format!("Command timed out after {:.3}s", duration.as_secs_f64(),)
        }
        Status::Error(Error { stage, error }) => {
            format!("Internal error (stage: {stage}): {error}")
        }
        Status::Cancelled(Cancelled {}) => "Command was cancelled".to_owned(),
    })
}

pub fn success_stderr(
    action: &buck2_data::ActionExecutionEnd,
    verbosity: Verbosity,
) -> buck2_error::Result<Option<&str>> {
    if !(verbosity.print_success_stderr() || action.always_print_stderr) {
        return Ok(None);
    }

    let stderr = match action.commands.last() {
        Some(command) => {
            &command
                .details
                .as_ref()
                .buck_error_context("CommandExecution did not include a `command`")?
                .cmd_stderr
        }
        None => return Ok(None),
    };

    if stderr.is_empty() {
        return Ok(None);
    }

    Ok(Some(stderr))
}

pub fn sanitize_output_colors(stderr: &[u8]) -> String {
    let mut sanitized = String::with_capacity(stderr.len());
    let mut parser = termwiz::escape::parser::Parser::new();
    parser.parse(stderr, |a| match a {
        Action::Print(c) => sanitized.push(c),
        Action::Control(cc) => match cc {
            ControlCode::CarriageReturn => sanitized.push('\r'),
            ControlCode::LineFeed => sanitized.push('\n'),
            ControlCode::HorizontalTab => sanitized.push('\t'),
            _ => {}
        },
        _ => {}
    });
    sanitized
}

/// Display information extracted from a CriticalPathEntry2.
pub struct CriticalPathEntryDisplay<'a> {
    /// The kind of critical path entry (e.g., "action", "analysis", "materialization").
    pub kind: &'a str,
    /// The name/label of the entry (e.g., target label, package name).
    pub name: String,
    /// Optional category (e.g., for actions).
    pub category: Option<&'a str>,
    /// Optional identifier (e.g., action identifier, file path for materializations).
    pub identifier: Option<&'a str>,
    /// Optional execution kind for actions (e.g., "local", "remote").
    pub execution_kind: Option<&'static str>,
}

impl<'a> CriticalPathEntryDisplay<'a> {
    /// Extracts display information from a CriticalPathEntry2.
    pub fn from_entry(
        entry: &'a buck2_data::CriticalPathEntry2,
        opts: TargetDisplayOptions,
    ) -> buck2_error::Result<Option<Self>> {
        use buck2_data::critical_path_entry2::Entry;

        let entry_data = match &entry.entry {
            Some(entry) => entry,
            None => return Ok(None),
        };

        let (kind, name, category, identifier, execution_kind) = match entry_data {
            Entry::Analysis(analysis) => {
                use buck2_data::critical_path_entry2::analysis::Target;

                let name = match &analysis.target {
                    Some(Target::StandardTarget(t)) => display_configured_target_label(t, opts)?,
                    None => "unknown".to_owned(),
                };
                ("analysis", name, None, None, None)
            }
            Entry::DynamicAnalysis(analysis) => {
                use buck2_data::critical_path_entry2::dynamic_analysis::Target;

                let name = match &analysis.target {
                    Some(Target::StandardTarget(t)) => display_configured_target_label(t, opts)?,
                    None => "anon-unknown".to_owned(),
                };
                ("dynamic_analysis", name, None, None, None)
            }
            Entry::ActionExecution(action_execution) => {
                use buck2_data::critical_path_entry2::action_execution::Owner;

                let category = action_execution.name.as_ref().map(|n| n.category.as_str());
                let identifier = action_execution
                    .name
                    .as_ref()
                    .map(|n| n.identifier.as_str());

                let execution_kind = Some(
                    buck2_data::ActionExecutionKind::try_from(action_execution.execution_kind)
                        .unwrap_or(buck2_data::ActionExecutionKind::NotSet)
                        .as_str_name(),
                );

                let name = match &action_execution.owner {
                    Some(Owner::TargetLabel(t)) => display_configured_target_label(t, opts)?,
                    Some(Owner::BxlKey(t)) => display_bxl_key(t)?,
                    Some(Owner::AnonTarget(t)) => display_anon_target(t)?,
                    None => "unknown".to_owned(),
                };
                ("action", name, category, identifier, execution_kind)
            }
            Entry::FinalMaterialization(materialization) => {
                use buck2_data::critical_path_entry2::final_materialization::Owner;

                let identifier = Some(materialization.path.as_str());

                let name = match &materialization.owner {
                    Some(Owner::TargetLabel(t)) => display_configured_target_label(t, opts)?,
                    Some(Owner::BxlKey(t)) => display_bxl_key(t)?,
                    Some(Owner::AnonTarget(t)) => display_anon_target(t)?,
                    None => "unknown".to_owned(),
                };
                ("materialization", name, None, identifier, None)
            }
            Entry::ComputeCriticalPath(..) => {
                ("compute-critical-path", String::new(), None, None, None)
            }
            Entry::Load(load) => ("load", load.package.clone(), None, None, None),
            Entry::Listing(listing) => ("listing", listing.package.clone(), None, None, None),
            Entry::GenericEntry(generic_entry) => {
                (generic_entry.kind.as_str(), String::new(), None, None, None)
            }
            Entry::Waiting(entry) => {
                let name = entry.category.clone().unwrap_or_default();
                ("waiting", name, None, None, None)
            }
            Entry::TestExecution(test_execution) => {
                let name = match &test_execution.target_label {
                    Some(t) => display_configured_target_label(t, opts)?,
                    None => "unknown".to_owned(),
                };
                ("test-execution", name, None, None, None)
            }
            Entry::TestListing(test_listing) => {
                let name = match &test_listing.target_label {
                    Some(t) => display_configured_target_label(t, opts)?,
                    None => "unknown".to_owned(),
                };
                ("test-listing", name, None, None, None)
            }
        };

        Ok(Some(CriticalPathEntryDisplay {
            kind,
            name,
            category,
            identifier,
            execution_kind,
        }))
    }

    /// Returns a formatted display name combining kind and name.
    pub fn display_name(&self) -> String {
        if self.name.is_empty() {
            self.kind.to_owned()
        } else {
            format!("{}: {}", self.kind, self.name)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn removes_color_characters() {
        let message = "\x1b[0mFoo\t\x1b[34mBar\n\x1b[DBaz\r\nQuz";

        let sanitized = sanitize_output_colors(message.as_bytes());

        assert_eq!("Foo\tBar\nBaz\r\nQuz", sanitized);
    }

    #[test]
    fn strips_trailing_newline_character() {
        let stream_contents = "test\n";
        let res = strip_trailing_newline(stream_contents);
        assert_eq!(res, "test");
    }

    #[test]
    fn preserves_duplicate_newlines() {
        let stream_contents = "test\n\n";
        let res = strip_trailing_newline(stream_contents);
        assert_eq!(res, "test\n");
    }

    #[test]
    fn preserves_other_trailing_whitespace() {
        let stream_contents = "test    \t";
        let res = strip_trailing_newline(stream_contents);
        assert_eq!(res, stream_contents);
    }

    #[test]
    fn preserves_leading_whitespace() {
        let stream_contents = "\n  test";
        let res = strip_trailing_newline(stream_contents);
        assert_eq!(res, stream_contents);
    }

    #[test]
    fn correctly_handles_carriage_return() {
        let stream_contents = "test\r\n";
        let res = strip_trailing_newline(stream_contents);
        assert_eq!(res, "test");
    }
}
