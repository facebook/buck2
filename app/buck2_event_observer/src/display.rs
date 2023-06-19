/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// TODO(brasselsprouts): move this onto the original core types and convert in events

use std::borrow::Cow;
use std::fmt;
use std::fmt::Write;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context as _;
use buck2_common::convert::ProstDurationExt;
use buck2_data::action_key;
use buck2_data::span_start_event::Data;
use buck2_data::ActionKey;
use buck2_data::ActionName;
use buck2_data::AnonTarget;
use buck2_data::BxlFunctionKey;
use buck2_data::BxlFunctionLabel;
use buck2_data::ConfiguredTargetLabel;
use buck2_data::TargetLabel;
use buck2_events::BuckEvent;
use buck2_test_api::data::TestStatus;
use buck2_util::collections::ordered_set::OrderedSet;
use buck2_util::commas::commas;
use buck2_util::truncate::truncate;
use dupe::Dupe;
use superconsole::style::Stylize;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;
use thiserror::Error;

use crate::fmt_duration;
use crate::verbosity::Verbosity;

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
) -> anyhow::Result<String> {
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
            format!("{}:{}", package, name)
        })
    } else {
        Err(ParseEventError::InvalidConfiguredTargetLabel.into())
    }
}

pub fn display_anon_target(ctl: &AnonTarget) -> anyhow::Result<String> {
    if let AnonTarget {
        name: Some(TargetLabel { package, name }),
        // We currently never display execution configurations, only normal configurations
        execution_configuration: _,
        hash,
    } = ctl
    {
        Ok(format!("{}:{}@{}", package, name, hash))
    } else {
        Err(ParseEventError::InvalidAnonTarget.into())
    }
}

pub fn display_analysis_target(
    target: &buck2_data::analysis_start::Target,
    opts: TargetDisplayOptions,
) -> anyhow::Result<String> {
    use buck2_data::analysis_start::Target;
    match target {
        Target::StandardTarget(ctl) => display_configured_target_label(ctl, opts),
        Target::AnonTarget(anon) => display_anon_target(anon),
    }
}

pub fn display_bxl_key(ctl: &BxlFunctionKey) -> anyhow::Result<String> {
    if let BxlFunctionKey {
        label: Some(BxlFunctionLabel { bxl_path, name }),
    } = ctl
    {
        Ok(format!("{}:{}", bxl_path, name))
    } else {
        Err(ParseEventError::MissingBxlFunctionLabel.into())
    }
}

pub fn display_action_owner(
    owner: &action_key::Owner,
    opts: TargetDisplayOptions,
) -> anyhow::Result<String> {
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
) -> anyhow::Result<String> {
    if let ActionKey {
        owner: Some(owner), ..
    } = action_key
    {
        display_action_owner(owner, opts)
    } else {
        Err(ParseEventError::MissingActionOwner.into())
    }
}

fn display_action_name_opt(name: Option<&ActionName>) -> String {
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
) -> anyhow::Result<String> {
    let key_string = match action_key {
        Some(key) => display_action_key(key, opts),
        None => Err(ParseEventError::MissingActionKey.into()),
    }?;
    let action_string = match name {
        Some(ActionName {
            category,
            identifier,
        }) if !identifier.is_empty() => format!(" ({} {})", category, identifier),
        Some(ActionName { category, .. }) => format!(" ({})", category),
        None => String::new(),
    };

    Ok(format!("{}{}", key_string, action_string))
}

/// Formats event payloads for display.
pub fn display_event(event: &BuckEvent, opts: TargetDisplayOptions) -> anyhow::Result<String> {
    let res: anyhow::Result<_> = try {
        let data = match event.data() {
            buck2_data::buck_event::Data::SpanStart(ref start) => start.data.as_ref().unwrap(),
            _ => Err(anyhow::Error::from(ParseEventError::UnexpectedEvent))?,
        };

        let res: anyhow::Result<_> = match data {
            Data::ActionExecution(action) => match &action.key {
                Some(key) => {
                    let string = display_action_key(key, opts)?;
                    let action_descriptor = display_action_name_opt(action.name.as_ref());
                    Ok(format!("{} -- action ({})", string, action_descriptor))
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
                Ok(format!("{} -- materializing `{}`", key, path))
            }
            Data::Analysis(analysis) => match &analysis.target {
                Some(target) => {
                    let target = display_analysis_target(target, opts)?;
                    Ok(format!("{} -- running analysis", target))
                }
                None => Err(ParseEventError::MissingConfiguredTargetLabel.into()),
            },
            Data::AnalysisStage(info) => {
                let stage = info.stage.as_ref().context("analysis stage is missing")?;
                let stage = display_analysis_stage(stage);
                Ok(stage.into())
            }
            Data::LoadPackage(load) => Ok(format!("{} -- loading package file tree", load.path)),
            Data::Load(load) => Ok(format!("{} -- evaluating build file", load.module_id)),
            Data::ExecutorStage(info) => {
                let stage = info.stage.as_ref().context("executor stage is missing")?;
                let stage = display_executor_stage(stage).context("unknown executor stage")?;
                Ok(stage.into())
            }
            Data::TestDiscovery(discovery) => Ok(format!(
                "Test {} -- discovering tests",
                discovery.suite_name
            )),
            Data::TestStart(start) => match &start.suite {
                Some(suite) => {
                    let tests = suite.test_names.join(" ");
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
            Data::MatchDepFiles(buck2_data::MatchDepFilesStart {}) => Ok("dep_files".to_owned()),
            Data::SharedTask(..) => Ok("Waiting on task from another command".to_owned()),
            Data::CacheUpload(..) => Ok("upload".to_owned()),
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
                    Ok(format!("Waiting for command [{}] to finish", name))
                } else {
                    Ok("Waiting for dice".to_owned())
                }
            }
            Data::DeferredPreparationStage(prep) => {
                use buck2_data::deferred_preparation_stage_start::Stage;
                match prep.stage.as_ref().context("Missing `stage`")? {
                    Stage::MaterializedArtifacts(_) => Ok("local_materialize_inputs".to_owned()),
                }
            }
            Data::DynamicLambda(lambda) => {
                use buck2_data::dynamic_lambda_start::Owner;

                let label = match lambda.owner.as_ref().context("Missing `owner`")? {
                    Owner::TargetLabel(target_label) => {
                        display_configured_target_label(target_label, opts)
                    }
                    Owner::BxlKey(bxl_key) => display_bxl_key(bxl_key),
                    Owner::AnonTarget(anon_target) => display_anon_target(anon_target),
                }?;

                Ok(format!("{} -- dynamic analysis", label))
            }
            Data::BxlExecution(execution) => {
                Ok(format!("Executing BXL script `{}`", execution.name))
            }
            Data::BxlDiceInvocation(..) => Ok("Waiting for graph computations".to_owned()),
            Data::ReUpload(..) => Ok("re_upload".to_owned()),
            Data::ConnectToInstaller(buck2_data::ConnectToInstallerStart { tcp_port }) => {
                Ok(format!("Connecting to installer on port {}", tcp_port))
            }
            Data::Fake(fake) => Ok(format!("{} -- speak of the devil", fake.caramba)),
            Data::LocalResources(..) => Ok("Local resources setup".to_owned()),
            Data::ReleaseLocalResources(..) => Ok("Releasing local resources".to_owned()),
            Data::BxlEnsureArtifacts(..) => Err(ParseEventError::UnexpectedEvent.into()),
        };

        // This shouldn't really be necessary, but that's how try blocks work :(
        res?
    };

    res.with_context(|| InvalidBuckEvent(Arc::new(event.clone())))
}

fn display_file_watcher(provider: i32) -> &'static str {
    match buck2_data::FileWatcherProvider::from_i32(provider) {
        Some(buck2_data::FileWatcherProvider::Watchman) => "Watchman",
        Some(buck2_data::FileWatcherProvider::RustNotify) => "notify",
        None => "unknown mechanism",
    }
}

pub fn display_analysis_stage(stage: &buck2_data::analysis_stage_start::Stage) -> &'static str {
    use buck2_data::analysis_stage_start::Stage;

    match stage {
        Stage::ResolveQueries(()) => "resolve_queries",
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
            to_print.insert(&x.path);
        }
        for path in to_print.iter().take(MAX_PRINT_MESSAGES) {
            res.push(format!("File changed: {}", path));
        }
        let unprinted_paths =
            // those we have the names of but didn't print
            to_print.len().saturating_sub(MAX_PRINT_MESSAGES) +
            // plus those we didn't get the names for
            (stats.events_processed as usize).saturating_sub(stats.events.len());
        if unprinted_paths > 0 {
            res.push(format!("{} additional file change events", unprinted_paths));
        }

        if let Some(fresh_instance) = &stats.fresh_instance_data {
            let file_watcher = if stats.watchman_version.is_some() {
                "Watchman"
            } else {
                "File Watcher"
            };

            let mut msg = format!("{} fresh instance: ", file_watcher);
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
        Stage::CacheQuery(..) => "re_action_cache",
        Stage::CacheHit(..) => "re_download",
        Stage::Re(re) => {
            use buck2_data::re_stage::Stage;

            match re.stage.as_ref()? {
                Stage::Execute(..) => "re_execute",
                Stage::Download(..) => "re_download",
                Stage::Queue(..) => "re_queued",
                Stage::WorkerDownload(..) => "re_worker_download",
                Stage::WorkerUpload(..) => "re_worker_upload",
                Stage::Unknown(..) => "re_unknown",
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
                Stage::InitializeWorker(_) => "initialize_worker",
                Stage::WorkerExecute(_) => "worker_execute",
            }
        }
    };

    Some(label)
}

#[derive(Error, Debug)]
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

#[derive(Error, Debug)]
#[error("Invalid buck event: `{0:?}`")]
pub struct InvalidBuckEvent(pub Arc<BuckEvent>);

pub fn format_test_result(test_result: &buck2_data::TestResult) -> anyhow::Result<Option<Lines>> {
    let buck2_data::TestResult {
        name,
        status,
        duration,
        details,
        ..
    } = test_result;
    let status = TestStatus::try_from(*status)?;

    // Pass results normally have no details, unless the --print-passing-details is set.
    // Do not display anything for passing tests unless details are present to avoid
    // cluttering the UI with unimportant test results.
    if matches!(&status, TestStatus::PASS | TestStatus::LISTING_SUCCESS) && details.is_empty() {
        return Ok(None);
    }

    let prefix = match status {
        TestStatus::FAIL => Span::new_styled("✗ Fail".to_owned().red()),
        TestStatus::SKIP => Span::new_styled("↷ Skip".to_owned().cyan()),
        TestStatus::OMITTED => Span::new_styled("\u{20E0} Omitted".to_owned().cyan()),
        TestStatus::FATAL => Span::new_styled("⚠ Fatal".to_owned().red()),
        TestStatus::TIMEOUT => Span::new_styled("✉ Timeout".to_owned().cyan()),
        TestStatus::PASS => Span::new_styled("✓ Pass".to_owned().green()),
        TestStatus::LISTING_SUCCESS => Span::new_styled("✓ Listing success".to_owned().green()),
        TestStatus::UNKNOWN => Span::new_styled("? Unknown".to_owned().cyan()),
        TestStatus::RERUN => Span::new_styled("↻ Rerun".to_owned().cyan()),
        TestStatus::LISTING_FAILED => Span::new_styled("⚠ Listing failed".to_owned().red()),
    }?;
    let mut base = Line::from_iter([prefix, Span::new_unstyled(format!(": {}", name,))?]);
    if let Some(duration) = duration {
        if let Ok(duration) = Duration::try_from(duration.clone()) {
            base.push(Span::new_unstyled(format!(
                " ({})",
                // Set time_speed parameter as 1.0 because this is taking the duration of something that was measured somewhere else,
                // so it doesn't make sense to apply the speed adjustment.
                fmt_duration::fmt_duration(duration, 1.0)
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
    pub command: Option<Cow<'a, buck2_data::CommandExecutionDetails>>,
}

impl<'a> ActionErrorDisplay<'a> {
    pub fn to_static(self) -> ActionErrorDisplay<'static> {
        ActionErrorDisplay {
            action_id: self.action_id,
            reason: self.reason,
            command: self.command.map(|c| Cow::Owned(c.into_owned())),
        }
    }
}

pub fn display_action_error<'a>(
    action: &'a buck2_data::ActionExecutionEnd,
    error: &'a buck2_data::action_execution_end::Error,
    opts: TargetDisplayOptions,
) -> anyhow::Result<ActionErrorDisplay<'a>> {
    use buck2_data::action_execution_end::Error;

    let command = action.commands.last().and_then(|c| c.details.as_ref());

    let reason = match error {
        Error::MissingOutputs(missing_outputs) => {
            format!("Required outputs are missing: {}", missing_outputs.message)
        }
        Error::Unknown(error_string) => {
            format!("Internal error: {}", error_string)
        }
        Error::CommandExecutionError(buck2_data::CommandExecutionError {}) => {
            match action.commands.last() {
                Some(c) => failure_reason_for_command_execution(c)?,
                None => "Unexpected command status".to_owned(),
            }
        }
    };

    Ok(ActionErrorDisplay {
        action_id: display_action_identity(action.key.as_ref(), action.name.as_ref(), opts)?,
        reason,
        command: command.map(Cow::Borrowed),
    })
}

fn failure_reason_for_command_execution(
    command_execution: &buck2_data::CommandExecution,
) -> anyhow::Result<String> {
    use buck2_data::command_execution::Cancelled;
    use buck2_data::command_execution::Error;
    use buck2_data::command_execution::Failure;
    use buck2_data::command_execution::Status;
    use buck2_data::command_execution::Success;
    use buck2_data::command_execution::Timeout;
    use buck2_data::command_execution_details::Command;

    let command = command_execution
        .details
        .as_ref()
        .context("CommandExecution did not include a `command`")?;

    let status = command_execution
        .status
        .as_ref()
        .context("CommandExecution did not include a `status`")?;

    let locality = match command.command {
        Some(Command::RemoteCommand(..)) => "Remote ",
        Some(Command::LocalCommand(..)) | Some(Command::OmittedLocalCommand(..)) => "Local ",
        Some(Command::WorkerInitCommand(..)) => "Local Worker Initialization ",
        Some(Command::WorkerCommand(..)) => "Local Worker ",
        None => "",
    };

    Ok(match status {
        Status::Success(Success {}) => "Unexpected command status".to_owned(),
        Status::Failure(Failure {}) => {
            struct OptionalExitCode {
                code: Option<i32>,
            }

            impl fmt::Display for OptionalExitCode {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    match self.code {
                        Some(code) => write!(f, "{}", code),
                        None => write!(f, "<no exit code>"),
                    }
                }
            }

            format!(
                "{}command returned non-zero exit code {}",
                locality,
                OptionalExitCode {
                    code: command
                        .signed_exit_code
                        .or_else(|| command.exit_code.and_then(|e| e.try_into().ok()))
                }
            )
        }
        Status::Timeout(Timeout { duration }) => {
            let duration = duration
                .as_ref()
                .context("Timeout did not include a `duration`")?
                .try_into_duration()
                .context("Timeout `duration` was invalid")?;

            format!("Command timed out after {:.3}s", duration.as_secs_f64(),)
        }
        Status::Error(Error { stage, error }) => {
            format!("Internal error (stage: {}): {}", stage, error)
        }
        Status::Cancelled(Cancelled {}) => "Command was cancelled".to_owned(),
    })
}

pub fn success_stderr<'a>(
    action: &'a buck2_data::ActionExecutionEnd,
    verbosity: Verbosity,
) -> anyhow::Result<Option<&'a str>> {
    if !(verbosity.print_success_stderr() || action.always_print_stderr) {
        return Ok(None);
    }

    let stderr = match action.commands.last() {
        Some(command) => {
            &command
                .details
                .as_ref()
                .context("CommandExecution did not include a `command`")?
                .stderr
        }
        None => return Ok(None),
    };

    if stderr.is_empty() {
        return Ok(None);
    }

    Ok(Some(stderr))
}
