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
use std::time::Duration;

use anyhow::Context as _;
use buck2_data::action_key;
use buck2_data::span_start_event::Data;
use buck2_data::ActionKey;
use buck2_data::ActionName;
use buck2_data::BxlFunctionKey;
use buck2_data::BxlFunctionLabel;
use buck2_data::ConfiguredTargetLabel;
use buck2_data::TargetLabel;
use events::BuckEvent;
use itertools::Itertools;
use superconsole::content::lines_from_multiline_string;
use superconsole::style::Stylize;
use superconsole::Lines;
use superconsole::Span;
use test_api::data::TestStatus;
use thiserror::Error;

pub(crate) fn display_configured_target_label(
    ctl: &ConfiguredTargetLabel,
) -> anyhow::Result<String> {
    if let ConfiguredTargetLabel {
        label: Some(TargetLabel { package, name }),
        ..
    } = ctl
    {
        Ok(format!("{}:{}", package, name))
    } else {
        Err(ParseEventError::MissingTargetLabel.into())
    }
}

pub(crate) fn display_bxl_key(ctl: &BxlFunctionKey) -> anyhow::Result<String> {
    if let BxlFunctionKey {
        label: Some(BxlFunctionLabel { bxl_path, name }),
        args,
    } = ctl
    {
        Ok(format!("{}:{}({})", bxl_path, name, args.iter().join(",")))
    } else {
        Err(ParseEventError::MissingBxlFunctionLabel.into())
    }
}

pub(crate) fn display_action_owner(owner: &action_key::Owner) -> anyhow::Result<String> {
    match owner {
        action_key::Owner::TargetLabel(target_label) => {
            display_configured_target_label(target_label)
        }
        action_key::Owner::BxlKey(bxl_key) => display_bxl_key(bxl_key),
    }
}

pub(crate) fn display_action_key(action_key: &ActionKey) -> anyhow::Result<String> {
    if let ActionKey {
        owner: Some(owner), ..
    } = action_key
    {
        display_action_owner(owner)
    } else {
        Err(ParseEventError::MissingActionOwner.into())
    }
}

pub(crate) fn display_action_identity(
    action_key: Option<&ActionKey>,
    name: Option<&ActionName>,
) -> anyhow::Result<String> {
    let key_string = match action_key {
        Some(key) => display_action_key(key),
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
pub(crate) fn display_event(event: &BuckEvent) -> anyhow::Result<String> {
    let res: anyhow::Result<_> = try {
        let data = match event.data {
            buck2_data::buck_event::Data::SpanStart(ref start) => start.data.as_ref().unwrap(),
            _ => Err(anyhow::Error::from(ParseEventError::UnexpectedEvent))?,
        };

        let res: anyhow::Result<_> = match data {
            Data::ActionExecution(action) => match &action.key {
                Some(key) => {
                    let string = display_action_key(key)?;
                    let action_descriptor = match &action.name {
                        Some(name) if name.identifier.is_empty() => name.category.clone(),
                        Some(name) => format!("{} {}", name.category, name.identifier),
                        _ => "unknown".to_owned(),
                    };
                    Ok(format!(
                        "{} -- running action ({})",
                        string, action_descriptor
                    ))
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
                Some(ctl) => {
                    let target = display_configured_target_label(ctl)?;
                    Ok(format!("{} -- running analysis", target))
                }
                None => Err(ParseEventError::MissingConfiguratedTargetLabel.into()),
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
                let stage = display_executor_stage(stage)?;
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
            Data::Command(..) => Err(ParseEventError::UnexpectedEvent.into()),
            Data::Watchman(..) => Ok("Syncing file changes (via Watchman)".to_owned()),
            Data::MatchDepFiles(buck2_data::MatchDepFilesStart {}) => Ok("dep_files".to_owned()),
            Data::Fake(fake) => Ok(format!("{} -- speak of the devil", fake.caramba)),
        };

        // This shouldn't really be necessary, but that's how try blocks work :(
        res?
    };

    res.with_context(|| InvalidBuckEvent(event.clone()))
}

pub(crate) fn display_analysis_stage(
    stage: &buck2_data::analysis_stage_start::Stage,
) -> &'static str {
    use buck2_data::analysis_stage_start::Stage;

    match stage {
        Stage::ResolveQueries(()) => "resolve_queries",
        Stage::EvaluateRule(()) => "evaluate_rule",
    }
}

pub(crate) fn display_executor_stage(
    stage: &buck2_data::executor_stage_start::Stage,
) -> anyhow::Result<&'static str> {
    use buck2_data::executor_stage_start::Stage;

    let label = match stage {
        Stage::Prepare(..) => "prepare",
        Stage::CacheQuery(..) => "re_action_cache",
        Stage::CacheHit(..) => "re_download",
        Stage::Re(re) => {
            use buck2_data::re_stage::Stage;

            match re.stage.as_ref().context("re stage is missing")? {
                Stage::Upload(..) => "re_upload",
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

            match local.stage.as_ref().context("local stage is missing")? {
                Stage::Queued(..) => "local_queued",
                Stage::Execute(..) => "local_execute",
                Stage::MaterializeInputs(..) => "local_materialize_inputs",
                Stage::PrepareOutputs(_) => "local_prepare_outputs",
            }
        }
    };

    Ok(label)
}

#[derive(Error, Debug)]
pub(crate) enum ParseEventError {
    #[error("Missing configured target label")]
    MissingConfiguratedTargetLabel,
    #[error("Missing target label")]
    MissingTargetLabel,
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
#[error("Invalid buck event: `{:?}`", .0)]
pub(crate) struct InvalidBuckEvent(pub BuckEvent);

pub(crate) fn duration_as_secs_elapsed(elapsed: Duration, time_speed: f64) -> String {
    format!("{:.1}s", elapsed.mul_f64(time_speed).as_secs_f64())
}

pub(crate) fn format_test_result(
    test_result: &buck2_data::TestResult,
) -> anyhow::Result<Option<Lines>> {
    let buck2_data::TestResult {
        name,
        status,
        duration,
        stdout,
        stderr,
        ..
    } = test_result;
    let status = TestStatus::try_from(*status)?;
    let prefix = match status {
        TestStatus::FAIL => Span::new_styled("✗ Fail".to_owned().red()),
        TestStatus::SKIP => Span::new_styled("↷ Skip".to_owned().cyan()),
        TestStatus::OMITTED => Span::new_styled("\u{20E0} Omitted".to_owned().cyan()),
        TestStatus::FATAL => Span::new_styled("⚠ Fatal".to_owned().red()),
        TestStatus::TIMEOUT => Span::new_styled("✉ Timeout".to_owned().cyan()),
        TestStatus::PASS | TestStatus::LISTING_SUCCESS | TestStatus::UNKNOWN => return Ok(None),
        TestStatus::RERUN => Span::new_styled("↻ Rerun".to_owned().cyan()),
        TestStatus::LISTING_FAILED => Span::new_styled("⚠ Listing failed".to_owned().red()),
    }?;
    let mut base = superconsole::line![prefix, Span::new_unstyled(format!(": {}", name,))?];
    if let Some(duration) = duration {
        let duration = match Duration::try_from(duration.clone()) {
            Ok(duration) | Err(duration) => duration,
        };
        base.0.push(Span::new_unstyled(format!(
            " ({})",
            // Set time_speed parameter as 1.0 because this is taking the duration of something that was measured somewhere else,
            // so it doesn't make sense to apply the speed adjustment.
            duration_as_secs_elapsed(duration, 1.0)
        ))?);
    }
    let mut lines = vec![base];

    if matches!(
        status,
        TestStatus::FAIL | TestStatus::FATAL | TestStatus::LISTING_FAILED
    ) {
        let style = Default::default();
        lines.append(&mut lines_from_multiline_string(stderr, style));
        lines.append(&mut lines_from_multiline_string(stdout, style));
    }

    Ok(Some(lines))
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

pub(crate) fn display_action_error<'a>(
    action: &'a buck2_data::ActionExecutionEnd,
    error: &'a buck2_data::action_execution_end::Error,
) -> anyhow::Result<ActionErrorDisplay<'a>> {
    use buck2_data::action_execution_end::Error;

    let reason;
    let mut command = None;

    match error {
        Error::CommandFailed(command_failed) => {
            reason = format!(
                "Command returned non-zero exit code {}",
                command_failed.exit_code
            );
            command = Some(command_failed);
        }
        Error::TimedOut(timed_out) => {
            reason = timed_out.message.clone();
            command = timed_out.command.as_ref();
        }
        Error::MissingOutputs(missing_outputs) => {
            reason = format!("Required outputs are missing: {}", missing_outputs.message);
        }
        Error::Unknown(error_string) => {
            reason = format!("Internal error: {}", error_string);
        }
    }

    Ok(ActionErrorDisplay {
        action_id: display_action_identity(action.key.as_ref(), action.name.as_ref())?,
        reason,
        command: command.map(Cow::Borrowed),
    })
}
