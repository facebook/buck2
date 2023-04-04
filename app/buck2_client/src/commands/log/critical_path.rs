/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::time::Duration;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::stream_value::StreamValue;
use buck2_client_ctx::subscribers::event_log::options::EventLogOptions;
use buck2_event_observer::display;
use buck2_event_observer::display::TargetDisplayOptions;
use tokio_stream::StreamExt;

/// This command outputs stats about uploads to RE from the selected invocation.
#[derive(Debug, clap::Parser)]
pub struct CriticalPathCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
}

impl CriticalPathCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let Self { event_log } = self;

        let log_path = event_log.get(&ctx)?;

        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async move {
            let (invocation, mut events) = log_path.unpack_stream().await?;
            buck2_client_ctx::eprintln!("Showing critical path from: {}", invocation)?;

            while let Some(event) = events.try_next().await? {
                match event {
                    StreamValue::Event(event) => match event.data {
                        Some(buck2_data::buck_event::Data::Instant(instant)) => {
                            match instant.data {
                                Some(buck2_data::instant_event::Data::BuildGraphInfo(
                                    build_graph,
                                )) => {
                                    log_critical_path(&build_graph)?;
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }

            anyhow::Ok(())
        })?;

        ExitResult::success()
    }
}

fn log_critical_path(critical_path: &buck2_data::BuildGraphExecutionInfo) -> anyhow::Result<()> {
    let target_display_options = TargetDisplayOptions::for_log();

    for entry in &critical_path.critical_path2 {
        use buck2_data::critical_path_entry2::Entry;

        let kind;
        let name;
        let mut category = "";
        let mut identifier = "";

        match &entry.entry {
            Some(Entry::Analysis(analysis)) => {
                use buck2_data::critical_path_entry2::analysis::Target;

                kind = "analysis";

                name = match &analysis.target {
                    Some(Target::StandardTarget(t)) => {
                        display::display_configured_target_label(t, target_display_options)?
                    }
                    None => continue,
                };
            }
            Some(Entry::ActionExecution(action_execution)) => {
                use buck2_data::critical_path_entry2::action_execution::Owner;

                kind = "action";

                name = match &action_execution.owner {
                    Some(Owner::TargetLabel(t)) => {
                        display::display_configured_target_label(t, target_display_options)?
                    }
                    Some(Owner::BxlKey(t)) => display::display_bxl_key(t)?,
                    Some(Owner::AnonTarget(t)) => display::display_anon_target(t)?,
                    None => continue,
                };

                match &action_execution.name {
                    Some(name) => {
                        category = &name.category;
                        identifier = &name.identifier;
                    }
                    None => {}
                }
            }
            Some(Entry::Materialization(materialization)) => {
                use buck2_data::critical_path_entry2::materialization::Owner;

                kind = "materialization";

                name = match &materialization.owner {
                    Some(Owner::TargetLabel(t)) => {
                        display::display_configured_target_label(t, target_display_options)?
                    }
                    Some(Owner::BxlKey(t)) => display::display_bxl_key(t)?,
                    Some(Owner::AnonTarget(t)) => display::display_anon_target(t)?,
                    None => continue,
                };

                identifier = &materialization.path;
            }
            Some(Entry::ComputeCriticalPath(..)) => {
                kind = "compute-critical-path";
                name = "".to_owned();
            }
            None => continue,
        }

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
                    write!(f, "{}", inner.as_micros())?;
                }
                Ok(())
            }
        }

        buck2_client_ctx::println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}",
            kind,
            name,
            category,
            identifier,
            OptionalDuration::new(entry.total_duration.clone())?,
            OptionalDuration::new(entry.user_duration.clone())?,
            OptionalDuration::new(entry.potential_improvement_duration.clone())?,
        )?;
    }

    Ok(())
}
