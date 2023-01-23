/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::sync::Arc;
use std::time::Duration;
use std::time::SystemTime;
use std::time::SystemTimeError;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::stream_value::StreamValue;
use buck2_client_ctx::subscribers::event_log::options::EventLogOptions;
use buck2_client_ctx::subscribers::subscriber_unpack::UnpackingEventSubscriber;
use buck2_client_ctx::subscribers::superconsole::timed_list::TimedList;
use buck2_client_ctx::subscribers::superconsole::SessionInfoComponent;
use buck2_client_ctx::subscribers::superconsole::StatefulSuperConsole;
use buck2_client_ctx::subscribers::superconsole::CUTOFFS;
use buck2_client_ctx::verbosity::Verbosity;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_events::BuckEvent;
use superconsole::components::splitting::SplitKind;
use superconsole::components::Split;
use superconsole::Component;
use superconsole::Direction;
use tokio::runtime;
use tokio_stream::StreamExt;

/// Show the spans that were open when the log ended
#[derive(Debug, clap::Parser)]
pub struct WhatUpCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,

    /// Show spans after X amount of miliseconds
    #[clap(
        long,
        help = "Print the actions that where open after certain amount of miliseconds",
        value_name = "NUMBER"
    )]
    pub after: Option<u64>,
}

impl WhatUpCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let Self { event_log, after } = self;
        let cutoff_time = after.map(Duration::from_millis);

        let log_path = event_log.get(&ctx)?;

        // Create space for a very big console
        let mut components: Vec<Box<dyn Component>> = vec![box SessionInfoComponent];
        components.push(box TimedList::new(1000000, CUTOFFS, String::new()));
        let console_root = box Split::new(components, Direction::Vertical, SplitKind::Adaptive);

        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async move {
            // Get events
            let (_, mut events) = log_path.unpack_stream().await?;

            //Create new superconsole
            let mut console = StatefulSuperConsole::new_with_root_forced(
                console_root,
                Verbosity::Default,
                true,
                None,
                Some(Box::new(io::stdout())),
                Default::default(),
                FileNameBuf::unchecked_new("placeholder"),
            )?;
            let mut first_timestamp = None;
            let mut should_render = true;
            // Ignore any events that are truncated, hence unreadable
            while let Ok(Some(event)) = events.try_next().await {
                match event {
                    StreamValue::Event(event) => {
                        let e = BuckEvent::try_from(event)?;
                        match cutoff_time {
                            Some(cutoff_time) => {
                                if should_stop_reading(
                                    cutoff_time,
                                    e.timestamp(),
                                    *first_timestamp.get_or_insert(e.timestamp()),
                                )? {
                                    break;
                                }
                            }
                            _ => (),
                        }

                        console.handle_event(&Arc::new(e)).await.unwrap();
                    }
                    StreamValue::Result(result) => {
                        console.handle_command_result(&result).await.unwrap();
                        should_render = false;
                    }
                }
            }
            if should_render {
                console.render_final_normal_console()?;
            } else {
                buck2_client_ctx::eprintln!("No open spans to render when log ended")?;
            }
            anyhow::Ok(())
        })?;

        ExitResult::success()
    }
}

fn should_stop_reading(
    after: Duration,
    event: SystemTime,
    first: SystemTime,
) -> Result<bool, SystemTimeError> {
    let elapsed = event.duration_since(first)?;
    if elapsed > after {
        return Ok(true);
    }
    Ok(false)
}
