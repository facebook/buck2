/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::io::Write;
use std::time::Duration;

use buck2_client_ctx::client_ctx::BuckSubcommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::event_log_options::EventLogOptions;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ClientIoError;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_error::conversion::from_any_with_tag;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_observer::display;
use buck2_event_observer::display::TargetDisplayOptions;
use serde::Serialize;
use tokio_stream::StreamExt;

use crate::LogCommandOutputFormat;
use crate::LogCommandOutputFormatWithWriter;
use crate::transform_format;

/// Show the critical path for a selected build.
///
/// This produces tab-delimited output listing every node on the critical path.
///
/// It includes the kind of node, its name, category and identifier, as well as total duration
/// (runtime of this node), user duration (duration the user can improve), potential improvement
/// before this node stops being on the critical path, non-critical path time, and start offset.
///
/// All durations are in microseconds. Start offset is in microseconds from the beginning of the build.
#[derive(Debug, clap::Parser)]
pub struct CriticalPathCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
    #[clap(
        long,
        help = "Which output format to use for this command",
        default_value = "tabulated",
        ignore_case = true,
        value_enum
    )]
    format: LogCommandOutputFormat,
}

impl BuckSubcommand for CriticalPathCommand {
    const COMMAND_NAME: &'static str = "log-critical-path";

    async fn exec_impl(
        self,
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        _events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let Self { event_log, format } = self;

        let log_path = event_log.get(&ctx).await?;

        let (invocation, mut events) = log_path.unpack_stream().await?;
        buck2_client_ctx::eprintln!(
            "Showing critical path from: {}",
            invocation.display_command_line()
        )?;

        while let Some(event) = events.try_next().await? {
            match event {
                StreamValue::Event(event) => match event.data {
                    Some(buck2_data::buck_event::Data::Instant(instant)) => match instant.data {
                        Some(buck2_data::instant_event::Data::BuildGraphInfo(build_graph)) => {
                            log_critical_path(&build_graph, format.clone()).await?;
                        }
                        _ => {}
                    },
                    _ => {}
                },
                _ => {}
            }
        }

        ExitResult::success()
    }
}

#[derive(Default)]
struct OptionalDuration {
    inner: Option<Duration>,
}

impl OptionalDuration {
    fn new<T, E>(d: Option<T>) -> Result<Self, E>
    where
        T: TryInto<Duration, Error = E>,
    {
        Ok(Self {
            inner: d.map(|d| d.try_into()).transpose()?,
        })
    }
}

impl fmt::Display for OptionalDuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(inner) = self.inner {
            inner.as_micros().fmt(f)?;
        } else {
            "".fmt(f)?;
        }
        Ok(())
    }
}

impl Serialize for OptionalDuration {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if let Some(micros) = self.inner.map(|d| d.as_micros()) {
            serializer.serialize_some(&micros)
        } else {
            serializer.serialize_none()
        }
    }
}

/// Represents a single entry on the critical path.
///
/// Contains information about the node type, timing information, and metadata.
#[derive(Default, Serialize)]
struct CriticalPathEntry<'a> {
    /// The kind of critical path entry (e.g., "action", "analysis", "materialization").
    kind: &'a str,
    /// The name/label of the entry (e.g., target label, package name).
    /// Empty string for entries without names.
    #[serde(skip_serializing_if = "String::is_empty")]
    name: String,
    /// Optional category (e.g., for actions).
    #[serde(skip_serializing_if = "Option::is_none")]
    category: Option<&'a str>,
    /// Optional identifier (e.g., action identifier, file path for materializations).
    #[serde(skip_serializing_if = "Option::is_none")]
    identifier: Option<&'a str>,
    /// Optional execution kind for actions (e.g., "local", "remote").
    #[serde(skip_serializing_if = "Option::is_none")]
    execution_kind: Option<&'a str>,
    /// Total wall-clock duration of this entry.
    total_duration: OptionalDuration,
    /// User-improvable duration (portion the user can optimize).
    user_duration: OptionalDuration,
    /// Potential improvement duration before this node drops off the critical path.
    potential_improvement_duration: OptionalDuration,
    /// Time spent off the critical path (non-critical path duration).
    non_critical_path_time: OptionalDuration,
    /// Start offset in microseconds from the beginning of the build.
    start_offset: u64,
}

async fn log_critical_path(
    critical_path: &buck2_data::BuildGraphExecutionInfo,
    format: LogCommandOutputFormat,
) -> buck2_error::Result<()> {
    let target_display_options = TargetDisplayOptions::for_log();

    buck2_client_ctx::stdio::print_with_writer::<buck2_error::Error, _>(async move |w| {
        let mut log_writer = transform_format(format, w);

        for entry in &critical_path.critical_path2 {
            use buck2_data::critical_path_entry2::Entry;

            let mut critical_path = CriticalPathEntry::default();

            let entry_data = match &entry.entry {
                Some(entry) => entry,
                None => continue,
            };

            critical_path.name = match entry_data {
                Entry::Analysis(analysis) => {
                    use buck2_data::critical_path_entry2::analysis::Target;

                    critical_path.kind = "analysis";

                    match &analysis.target {
                        Some(Target::StandardTarget(t)) => {
                            display::display_configured_target_label(t, target_display_options)?
                        }
                        None => "unknown".to_owned(),
                    }
                }
                Entry::DynamicAnalysis(analysis) => {
                    use buck2_data::critical_path_entry2::dynamic_analysis::Target;

                    critical_path.kind = "dynamic_analysis";

                    match &analysis.target {
                        Some(Target::StandardTarget(t)) => {
                            display::display_configured_target_label(t, target_display_options)?
                        }
                        None => "anon-unknown".to_owned(),
                    }
                }
                Entry::ActionExecution(action_execution) => {
                    use buck2_data::critical_path_entry2::action_execution::Owner;

                    critical_path.kind = "action";

                    match &action_execution.name {
                        Some(name) => {
                            critical_path.category = Some(&name.category);
                            critical_path.identifier = Some(&name.identifier);
                        }
                        None => {}
                    }

                    critical_path.execution_kind = Some(
                        buck2_data::ActionExecutionKind::try_from(action_execution.execution_kind)
                            .unwrap_or(buck2_data::ActionExecutionKind::NotSet)
                            .as_str_name(),
                    );

                    match &action_execution.owner {
                        Some(Owner::TargetLabel(t)) => {
                            display::display_configured_target_label(t, target_display_options)?
                        }
                        Some(Owner::BxlKey(t)) => display::display_bxl_key(t)?,
                        Some(Owner::AnonTarget(t)) => display::display_anon_target(t)?,
                        None => "unknown".to_owned(),
                    }
                }
                Entry::FinalMaterialization(materialization) => {
                    use buck2_data::critical_path_entry2::final_materialization::Owner;

                    critical_path.kind = "materialization";
                    critical_path.identifier = Some(&materialization.path);

                    match &materialization.owner {
                        Some(Owner::TargetLabel(t)) => {
                            display::display_configured_target_label(t, target_display_options)?
                        }
                        Some(Owner::BxlKey(t)) => display::display_bxl_key(t)?,
                        Some(Owner::AnonTarget(t)) => display::display_anon_target(t)?,
                        None => "unknown".to_owned(),
                    }
                }
                Entry::ComputeCriticalPath(..) => {
                    critical_path.kind = "compute-critical-path";
                    "".to_owned()
                }
                Entry::Load(load) => {
                    critical_path.kind = "load";
                    load.package.clone()
                }
                Entry::Listing(listing) => {
                    critical_path.kind = "listing";
                    listing.package.clone()
                }
                Entry::GenericEntry(generic_entry) => {
                    critical_path.kind = &generic_entry.kind;
                    "".to_owned()
                }
                Entry::Waiting(entry) => {
                    critical_path.kind = "waiting";
                    match &entry.category {
                        Some(category) => category.clone(),
                        None => "".to_owned(),
                    }
                }
                Entry::TestExecution(test_execution) => {
                    critical_path.kind = "test-execution";
                    match &test_execution.target_label {
                        Some(t) => {
                            display::display_configured_target_label(t, target_display_options)?
                        }
                        None => "unknown".to_owned(),
                    }
                }
                Entry::TestListing(test_listing) => {
                    critical_path.kind = "test-listing";
                    match &test_listing.target_label {
                        Some(t) => {
                            display::display_configured_target_label(t, target_display_options)?
                        }
                        None => "unknown".to_owned(),
                    }
                }
            };

            critical_path.total_duration = OptionalDuration::new(entry.total_duration)?;
            critical_path.user_duration = OptionalDuration::new(entry.user_duration)?;
            critical_path.potential_improvement_duration =
                OptionalDuration::new(entry.potential_improvement_duration)?;
            critical_path.non_critical_path_time =
                OptionalDuration::new(entry.non_critical_path_duration)?;
            critical_path.start_offset = entry.start_offset_ns.map(|v| v / 1000).unwrap_or(0);

            let res: Result<(), ClientIoError> = {
                match &mut log_writer {
                    LogCommandOutputFormatWithWriter::Tabulated(writer) => {
                        writeln!(
                            writer,
                            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                            critical_path.kind,
                            critical_path.name,
                            critical_path.category.unwrap_or_default(),
                            critical_path.identifier.unwrap_or_default(),
                            critical_path.execution_kind.unwrap_or_default(),
                            critical_path.total_duration,
                            critical_path.user_duration,
                            critical_path.potential_improvement_duration,
                            critical_path.non_critical_path_time,
                            critical_path.start_offset
                        )?;
                    }
                    LogCommandOutputFormatWithWriter::Json(writer) => {
                        serde_json::to_writer(writer.by_ref(), &critical_path)?;
                        writer.write_all("\n".as_bytes())?;
                    }
                    LogCommandOutputFormatWithWriter::Csv(writer) => {
                        writer
                            .serialize(critical_path)
                            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::LogCmd))?;
                    }
                }
                Ok(())
            };
            res?
        }
        Ok(())
    })
    .await
}
