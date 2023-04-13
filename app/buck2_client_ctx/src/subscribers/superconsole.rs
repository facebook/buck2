/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt::Debug;
use std::io::Write;
use std::iter;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_data::CommandExecutionDetails;
use buck2_event_observer::display;
use buck2_event_observer::display::display_file_watcher_end;
use buck2_event_observer::display::TargetDisplayOptions;
use buck2_event_observer::event_observer::DebugEventObserverExtra;
use buck2_event_observer::session_info::SessionInfo;
use buck2_event_observer::verbosity::Verbosity;
use buck2_event_observer::what_ran;
use buck2_event_observer::what_ran::local_command_to_string;
use buck2_event_observer::what_ran::WhatRanOptions;
use buck2_events::BuckEvent;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use gazebo::prelude::*;
use superconsole::components::DrawVertical;
use superconsole::style::Attribute;
use superconsole::style::Color;
use superconsole::style::ContentStyle;
use superconsole::style::StyledContent;
use superconsole::style::Stylize;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;
use superconsole::State;
pub(crate) use superconsole::SuperConsole;

use crate::subscribers::simpleconsole::SimpleConsole;
use crate::subscribers::subscriber::Tick;
use crate::subscribers::subscriber_unpack::UnpackingEventSubscriber;
use crate::subscribers::superconsole::commands::CommandsComponent;
use crate::subscribers::superconsole::debug_events::DebugEventsComponent;
use crate::subscribers::superconsole::debugger::StarlarkDebuggerComponent;
use crate::subscribers::superconsole::dice::DiceComponent;
use crate::subscribers::superconsole::io::IoHeader;
use crate::subscribers::superconsole::re::ReHeader;
use crate::subscribers::superconsole::test::TestHeader;
use crate::subscribers::superconsole::timed_list::Cutoffs;
use crate::subscribers::superconsole::timed_list::TimedList;

mod commands;
mod common;
pub(crate) mod debug_events;
mod debugger;
pub(crate) mod dice;
mod io;
mod re;
pub mod test;
pub mod timed_list;

const SUPERCONSOLE_WIDTH: usize = 150;

pub const CUTOFFS: Cutoffs = Cutoffs {
    inform: Duration::from_secs(4),
    warn: Duration::from_secs(8),
    _notable: Duration::from_millis(200),
};

pub struct StatefulSuperConsole {
    header: String,
    state: SuperConsoleState,
    super_console: Option<SuperConsole>,
    verbosity: Verbosity,
}

#[derive(Copy, Clone, Dupe, Debug)]
struct TimeSpeed {
    speed: f64,
}

const TIMESPEED_DEFAULT: f64 = 1.0;

impl TimeSpeed {
    pub(crate) fn new(speed_value: Option<f64>) -> anyhow::Result<Self> {
        let speed = speed_value.unwrap_or(TIMESPEED_DEFAULT);

        if speed <= 0.0 {
            return Err(anyhow::anyhow!("Time speed cannot be negative!"));
        }
        Ok(TimeSpeed { speed })
    }

    pub(crate) fn speed(self) -> f64 {
        self.speed
    }
}

pub struct SuperConsoleState {
    pub current_tick: Tick,
    time_speed: TimeSpeed,
    /// This contains the SpanTracker, which is why it's part of the SuperConsoleState.
    simple_console: SimpleConsole<DebugEventObserverExtra>,
    config: SuperConsoleConfig,
}

#[derive(Clone)]
pub struct SuperConsoleConfig {
    pub enable_dice: bool,
    pub enable_debug_events: bool,
    pub enable_detailed_re: bool,
    pub enable_io: bool,
    pub enable_commands: bool,
    pub display_platform: bool,
    /// Two lines for root events with single child event.
    pub two_lines: bool,
    pub max_lines: usize,
}

impl Default for SuperConsoleConfig {
    fn default() -> Self {
        Self {
            enable_dice: false,
            enable_debug_events: false,
            enable_detailed_re: false,
            enable_io: false,
            enable_commands: false,
            display_platform: false,
            two_lines: false,
            max_lines: 10,
        }
    }
}

struct BuckRootComponent<'s> {
    header: &'s str,
    state: &'s SuperConsoleState,
}

impl<'s> Component for BuckRootComponent<'s> {
    fn draw_unchecked(
        &self,
        state: &State,
        dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        // bound all components to our recommended grapheme-width
        let dimensions = dimensions.intersect(Dimensions {
            width: SUPERCONSOLE_WIDTH,
            height: usize::MAX,
        });

        let mut draw = DrawVertical::new(dimensions);
        draw.draw(
            &SessionInfoComponent {
                session_info: self.state.session_info(),
            },
            state,
            mode,
        )?;
        draw.draw(
            &ReHeader {
                super_console_config: &self.state.config,
                re_state: self.state.simple_console.observer.re_state(),
            },
            state,
            mode,
        )?;
        draw.draw(
            &IoHeader {
                super_console_config: &self.state.config,
                io_state: self.state.simple_console.observer.io_state(),
            },
            state,
            mode,
        )?;
        draw.draw(&TestHeader, state, mode)?;
        draw.draw(
            &DebugEventsComponent {
                super_console_config: &self.state.config,
                debug_events_state: self.state.simple_console.observer.extra().debug_events(),
            },
            state,
            mode,
        )?;
        draw.draw(
            &DiceComponent {
                super_console_config: &self.state.config,
                dice_state: self.state.simple_console.observer.extra().dice_state(),
            },
            state,
            mode,
        )?;
        draw.draw(
            &StarlarkDebuggerComponent {
                starlark_debugger_state: self
                    .state
                    .simple_console
                    .observer
                    .starlark_debugger_state(),
            },
            state,
            mode,
        )?;
        draw.draw(
            &CommandsComponent {
                super_console_config: &self.state.config,
                action_stats: self.state.simple_console.observer.action_stats(),
            },
            state,
            mode,
        )?;
        draw.draw(
            &TimedList::new(&CUTOFFS, self.header, self.state),
            state,
            mode,
        )?;

        Ok(draw.finish())
    }
}

impl StatefulSuperConsole {
    pub const FALLBACK_SIZE: Dimensions = Dimensions {
        width: 100,
        height: 40,
    };

    pub(crate) fn new_with_root_forced(
        trace_id: TraceId,
        command_name: &str,
        verbosity: Verbosity,
        show_waiting_message: bool,
        replay_speed: Option<f64>,
        stream: Option<Box<dyn Write + Send + 'static + Sync>>,
        config: SuperConsoleConfig,
        isolation_dir: FileNameBuf,
    ) -> anyhow::Result<Self> {
        let mut builder = Self::console_builder();
        if let Some(stream) = stream {
            builder.write_to(stream);
        }
        Self::new(
            command_name,
            trace_id,
            builder.build_forced(Self::FALLBACK_SIZE)?,
            verbosity,
            show_waiting_message,
            replay_speed,
            config,
            isolation_dir,
        )
    }

    pub(crate) fn new_with_root(
        trace_id: TraceId,
        command_name: &str,
        verbosity: Verbosity,
        show_waiting_message: bool,
        replay_speed: Option<f64>,
        config: SuperConsoleConfig,
        isolation_dir: FileNameBuf,
    ) -> anyhow::Result<Option<Self>> {
        match Self::console_builder().build()? {
            None => Ok(None),
            Some(sc) => Ok(Some(Self::new(
                command_name,
                trace_id,
                sc,
                verbosity,
                show_waiting_message,
                replay_speed,
                config,
                isolation_dir,
            )?)),
        }
    }

    pub(crate) fn new(
        command_name: &str,
        trace_id: TraceId,
        super_console: SuperConsole,
        verbosity: Verbosity,
        show_waiting_message: bool,
        replay_speed: Option<f64>,
        config: SuperConsoleConfig,
        isolation_dir: FileNameBuf,
    ) -> anyhow::Result<Self> {
        let header = format!("Command: `{}`.", command_name);
        Ok(Self {
            header,
            state: SuperConsoleState::new(
                replay_speed,
                trace_id,
                isolation_dir,
                verbosity,
                show_waiting_message,
                config,
            )?,
            super_console: Some(super_console),
            verbosity,
        })
    }

    /// Construct a console suitable for use by the Buck2 CLI. We use non-blocking output here
    /// because we do all our event processing on a single thread, so that if stderr is blocked
    /// (e.g.  because the client is using a resumable remote terminal and they've temporarily
    /// disconnected), we don't delay ingesting new events.
    ///
    /// This ensures we a) don't have to catch up when the client reconnects, b) don't buffer
    /// events (though we might buffer output), and c) can show an accurate "time elapsed" when the
    /// client returns (if we wait until the client returns to resume, we'll always report that the
    /// command finished "just now", because we'll have some events to catch up on).
    pub fn console_builder() -> ::superconsole::Builder {
        let mut builder = ::superconsole::Builder::new();
        builder.non_blocking();
        builder
    }

    /// Render the console for a final time, but use the Normal draw mode.
    /// Fails if there isn't a superconsole.
    pub fn render_final_normal_console(self) -> anyhow::Result<()> {
        match self.super_console {
            Some(sc) => sc.finalize_with_mode(
                &BuckRootComponent {
                    header: &self.header,
                    state: &self.state,
                },
                &self.state.state(),
                DrawMode::Normal,
            ),
            None => Err(anyhow::anyhow!("Cannot render non-existent superconsole")),
        }
    }

    pub fn render_result_errors(result: &buck2_cli_proto::CommandResult) -> Lines {
        let mut lines = Lines::new();
        if let buck2_cli_proto::CommandResult {
            result: Some(buck2_cli_proto::command_result::Result::Error(e)),
        } = result
        {
            let style = ContentStyle {
                foreground_color: Some(Color::DarkRed),
                ..Default::default()
            };
            for message in &e.messages {
                lines
                    .0
                    .extend(Lines::from_multiline_string(message, style).0);
            }
        }
        lines
    }
}

impl SuperConsoleState {
    pub fn new(
        replay_speed: Option<f64>,
        trace_id: TraceId,
        isolation_dir: FileNameBuf,
        verbosity: Verbosity,
        show_waiting_message: bool,
        config: SuperConsoleConfig,
    ) -> anyhow::Result<SuperConsoleState> {
        Ok(SuperConsoleState {
            current_tick: Tick::now(),
            time_speed: TimeSpeed::new(replay_speed)?,
            simple_console: SimpleConsole::with_tty(
                trace_id,
                isolation_dir,
                verbosity,
                show_waiting_message,
            ),
            config,
        })
    }

    pub fn update_event_observer(
        &mut self,
        receive_time: Instant,
        event: &Arc<BuckEvent>,
    ) -> anyhow::Result<()> {
        self.simple_console
            .update_event_observer(receive_time, event)
    }

    // Collect all state to send to super console. Note that the SpanTracker state is held in the
    // SimpleConsole so that if we downgrade to the SimpleConsole, we don't lose tracked spans.
    pub fn state(&self) -> superconsole::State {
        let observer = self.simple_console.observer();

        superconsole::state![
            &self.config,
            &self.current_tick,
            &self.time_speed,
            observer.spans(),
            observer.action_stats(),
            observer.test_state(),
            observer.session_info(),
            observer.re_state(),
            observer.io_state(),
            observer.extra().dice_state(),
            observer.extra().debug_events(),
            observer.starlark_debugger_state(),
        ]
    }

    pub fn session_info(&self) -> &SessionInfo {
        self.simple_console.observer.session_info()
    }
}

impl StatefulSuperConsole {
    async fn toggle(
        &mut self,
        what: &str,
        key: char,
        var: impl FnOnce(&mut Self) -> &mut bool,
    ) -> anyhow::Result<()> {
        let var = var(self);
        *var = !*var;
        let on_off = match *var {
            true => "on",
            false => "off",
        };
        self.handle_stderr(&format!("{what}: {on_off}, press `{key}` to revert"))
            .await
    }
}

// TODO(brasselsprouts): after deprecating filetailers, simplify these code paths
#[async_trait]
impl UnpackingEventSubscriber for StatefulSuperConsole {
    async fn handle_event(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        match &mut self.super_console {
            Some(_) => {
                self.handle_inner_event(event)
                    .await
                    .with_context(|| display::InvalidBuckEvent(event.clone()))?;
                self.state
                    .update_event_observer(self.state.current_tick.start_time, event)?;
            }
            None => {
                self.state.simple_console.handle_event(event).await?;
            }
        }

        if self.verbosity.print_all_commands() {
            // This is a bit messy. It would be better for this to go in the branch above, but we
            // can't do that, because we call a method on `self` in a branch that takes a mutable
            // borrow of the SuperConsole there. That works *only* if we don't use the console we
            // borrowed.
            if let Some(console) = &mut self.super_console {
                what_ran::emit_event_if_relevant(
                    event.parent_id().into(),
                    event.data(),
                    self.state.simple_console.observer().spans(),
                    console,
                    &WhatRanOptions::default(),
                )?;
            }
        }

        Ok(())
    }

    async fn handle_stderr(&mut self, msg: &str) -> anyhow::Result<()> {
        match &mut self.super_console {
            Some(super_console) => {
                super_console.emit(msg.lines().map(Line::sanitized).collect());
                Ok(())
            }
            None => self.state.simple_console.handle_stderr(msg).await,
        }
    }

    async fn handle_structured_error(
        &mut self,
        err: &buck2_data::StructuredError,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if err.quiet {
            return Ok(());
        }
        match &mut self.super_console {
            Some(super_console) => {
                super_console.emit(
                    err.payload
                        .lines()
                        .map(|line| {
                            Line::from_iter([Span::new_colored_lossy(line, Color::DarkYellow)])
                        })
                        .collect(),
                );
                Ok(())
            }
            None => {
                self.state
                    .simple_console
                    .handle_structured_error(err, event)
                    .await
            }
        }
    }

    async fn handle_file_watcher_end(
        &mut self,
        file_watcher: &buck2_data::FileWatcherEnd,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        match &mut self.super_console {
            Some(super_console) => {
                super_console.emit(Lines(
                    display_file_watcher_end(file_watcher).into_map(|x| Line::sanitized(&x)),
                ));
                Ok(())
            }
            None => {
                self.state
                    .simple_console
                    .handle_file_watcher_end(file_watcher, event)
                    .await
            }
        }
    }

    async fn handle_output(&mut self, raw_output: &[u8]) -> anyhow::Result<()> {
        if let Some(super_console) = self.super_console.take() {
            super_console.finalize(
                &BuckRootComponent {
                    header: &self.header,
                    state: &self.state,
                },
                &self.state.state(),
            )?;
        }

        self.state.simple_console.handle_output(raw_output).await
    }

    async fn handle_console_interaction(&mut self, c: char) -> anyhow::Result<()> {
        if c == 'd' {
            self.toggle("DICE component", 'd', |s| &mut s.state.config.enable_dice)
                .await?;
        } else if c == 'e' {
            self.toggle("Debug events component", 'e', |s| {
                &mut s.state.config.enable_debug_events
            })
            .await?;
        } else if c == '2' {
            self.toggle("Two lines mode", '2', |s| &mut s.state.config.two_lines)
                .await?;
        } else if c == 'r' {
            self.toggle("Detailed RE", 'r', |s| {
                &mut s.state.config.enable_detailed_re
            })
            .await?;
        } else if c == 'i' {
            self.toggle("I/O counters", 'i', |s| &mut s.state.config.enable_io)
                .await?;
        } else if c == 'p' {
            self.toggle("Display target configurations", 'p', |s| {
                &mut s.state.config.display_platform
            })
            .await?;
        } else if c == 'c' {
            self.toggle("Commands", 'c', |s| &mut s.state.config.enable_commands)
                .await?;
        } else if c == '+' {
            self.state.config.max_lines = self.state.config.max_lines.saturating_add(1);
        } else if c == '-' {
            self.state.config.max_lines = self.state.config.max_lines.saturating_sub(1);
        } else if c == '?' || c == 'h' {
            self.handle_stderr(
                "Help:\n\
                `d` = toggle DICE\n\
                `e` = toggle debug events\n\
                `2` = toggle two lines mode\n\
                `r` = toggle detailed RE\n\
                `i` = toggle I/O counters\n\
                `p` = display target configurations\n\
                `+` = show more lines\n\
                `-` = show fewer lines\n\
                `h` = show this help",
            )
            .await?;
        }

        Ok(())
    }

    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        match self.super_console.take() {
            Some(mut super_console) => {
                let lines = Self::render_result_errors(result);
                super_console.emit(lines);
                super_console.finalize(
                    &BuckRootComponent {
                        header: &self.header,
                        state: &self.state,
                    },
                    &self.state.state(),
                )
            }
            None => {
                self.state
                    .simple_console
                    .handle_command_result(result)
                    .await
            }
        }
    }

    async fn tick(&mut self, tick: &Tick) -> anyhow::Result<()> {
        self.state.simple_console.detect_hangs().await?;
        match &mut self.super_console {
            Some(super_console) => {
                self.state.current_tick = tick.dupe();
                super_console.render(
                    &BuckRootComponent {
                        header: &self.header,
                        state: &self.state,
                    },
                    &self.state.state(),
                )
            }
            None => Ok(()),
        }
    }

    async fn handle_error(&mut self, _error: &anyhow::Error) -> anyhow::Result<()> {
        match self.super_console.take() {
            Some(super_console) => super_console.finalize(
                &BuckRootComponent {
                    header: &self.header,
                    state: &self.state,
                },
                &self.state.state(),
            ),
            None => Ok(()),
        }
    }

    async fn handle_console_message(
        &mut self,
        message: &buck2_data::ConsoleMessage,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        // TODO(nmj): Maybe better handling of messages that have color data in them. Right now
        //            they're just stripped
        match &mut self.super_console {
            Some(super_console) => {
                super_console.emit(Lines::from_multiline_string(
                    &message.message,
                    ContentStyle::default(),
                ));
                Ok(())
            }
            None => {
                self.state
                    .simple_console
                    .handle_console_message(message, event)
                    .await
            }
        }
    }

    async fn handle_action_execution_end(
        &mut self,
        action: &buck2_data::ActionExecutionEnd,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        let super_console = match &mut self.super_console {
            Some(super_console) => super_console,
            None => {
                return self
                    .state
                    .simple_console
                    .handle_action_execution_end(action, event)
                    .await;
            }
        };

        let mut lines = vec![];
        let display_platform = self.state.config.display_platform;
        match action.error.as_ref() {
            Some(error) => {
                let display::ActionErrorDisplay {
                    action_id,
                    reason,
                    command,
                } = display::display_action_error(
                    action,
                    error,
                    TargetDisplayOptions::for_console(display_platform),
                )?;

                lines.push(Line::from_iter([Span::new_styled_lossy(
                    StyledContent::new(
                        ContentStyle {
                            foreground_color: Some(Color::White),
                            attributes: Attribute::Bold.into(),
                            ..Default::default()
                        },
                        format!("Action failed: {}", action_id,),
                    ),
                )]));

                lines.push(Line::from_iter([Span::new_styled_lossy(
                    reason.with(Color::DarkRed),
                )]));

                if let Some(command) = command {
                    lines_for_command_details(&command, self.verbosity, &mut lines);
                }
            }
            None => {
                if let Some(stderr) = display::success_stderr(action, self.verbosity)? {
                    let action_id = StyledContent::new(
                        ContentStyle {
                            foreground_color: Some(Color::White),
                            attributes: Attribute::Bold.into(),
                            ..Default::default()
                        },
                        format!(
                            "stderr for {}:",
                            display::display_action_identity(
                                action.key.as_ref(),
                                action.name.as_ref(),
                                TargetDisplayOptions::for_console(display_platform),
                            )?
                        ),
                    );
                    lines.push(Line::from_iter([Span::new_styled_lossy(action_id)]));
                    lines.extend(Lines::from_colored_multiline_string(stderr));
                }
            }
        }

        super_console.emit(Lines(lines));

        Ok(())
    }

    async fn handle_test_result(
        &mut self,
        result: &buck2_data::TestResult,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if let Some(super_console) = &mut self.super_console {
            if let Some(msg) = display::format_test_result(result)? {
                super_console.emit(msg);
            }
        }

        Ok(())
    }

    async fn handle_console_preferences(
        &mut self,
        prefs: &buck2_data::ConsolePreferences,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.state.config.max_lines = prefs.max_lines.try_into()?;

        Ok(())
    }

    // Our state snapshot handles those for us.

    async fn handle_command_start(
        &mut self,
        _command: &buck2_data::CommandStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_command_end(
        &mut self,
        _command: &buck2_data::CommandEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_test_discovery(
        &mut self,
        _test_info: &buck2_data::TestDiscovery,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

fn lines_for_command_details(
    command_failed: &CommandExecutionDetails,
    verbosity: Verbosity,
    lines: &mut Vec<Line>,
) {
    use buck2_data::command_execution_details::Command;

    match command_failed.command.as_ref() {
        Some(Command::LocalCommand(local_command)) => {
            let command = local_command_to_string(local_command);
            let command = command.as_str();
            let command = if verbosity.print_failure_full_command() {
                Cow::Borrowed(command)
            } else {
                match truncate(command) {
                    None => Cow::Borrowed(command),
                    Some(short) => Cow::Owned(format!(
                        "{} (run `buck2 log what-failed` to get the full command)",
                        short
                    )),
                }
            };

            lines.push(Line::from_iter([Span::new_styled_lossy(
                format!("Reproduce locally: `{}`", command).with(Color::DarkRed),
            )]));
        }
        Some(Command::RemoteCommand(remote_command)) => {
            if !buck2_core::is_open_source() {
                lines.push(Line::from_iter([Span::new_styled_lossy(
                    format!(
                        "Reproduce locally: `frecli cas download-action {}`",
                        remote_command.action_digest
                    )
                    .with(Color::DarkRed),
                )]));
            }
        }
        Some(Command::OmittedLocalCommand(..)) | None => {
            // Nothing to show in this case.
        }
    };

    lines.push(Line::from_iter([Span::new_styled_lossy(
        "stdout:"
            .to_owned()
            .with(Color::DarkRed)
            .attribute(Attribute::Bold),
    )]));
    lines.extend(Lines::from_multiline_string(
        &command_failed.stdout,
        color(Color::DarkRed),
    ));
    lines.push(Line::from_iter([Span::new_styled_lossy(
        "stderr:"
            .to_owned()
            .with(Color::DarkRed)
            .attribute(Attribute::Bold),
    )]));
    lines.extend(Lines::from_colored_multiline_string(&command_failed.stderr));
}

// Truncates a string to a reasonable number characters, or returns None if it doesn't need truncating.
fn truncate(contents: &str) -> Option<String> {
    const MAX_LENGTH: usize = 200;
    const BUFFER: usize = " ...<omitted>... ".len();
    if contents.len() > MAX_LENGTH + BUFFER {
        Some(format!(
            "{} ...<omitted>... {}",
            &contents[0..MAX_LENGTH / 2],
            &contents[contents.len() - MAX_LENGTH / 2..contents.len()]
        ))
    } else {
        None
    }
}

fn color(color: Color) -> ContentStyle {
    ContentStyle {
        foreground_color: Some(color),
        ..Default::default()
    }
}

/// This component is used to display session information for a command e.g. RE session ID
pub struct SessionInfoComponent<'s> {
    pub session_info: &'s SessionInfo,
}

impl<'s> Component for SessionInfoComponent<'s> {
    fn draw_unchecked(
        &self,
        _state: &State,
        dimensions: Dimensions,
        _mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let mut headers = Lines::new();
        let mut ids = vec![];
        if cfg!(fbcode_build) {
            headers.push(Line::unstyled("Buck UI:")?);
            ids.push(Span::new_unstyled(format!(
                "https://www.internalfb.com/buck2/{}",
                self.session_info.trace_id
            ))?);
        } else {
            headers.push(Line::unstyled("Build ID:")?);
            ids.push(Span::new_unstyled(&self.session_info.trace_id)?);
        }
        if let Some(buck2_data::TestSessionInfo { info }) = &self.session_info.test_session {
            headers.push(Line::unstyled("Test UI:")?);
            ids.push(Span::new_unstyled(info)?);
        }
        // pad all headers to the max width.
        headers.justify();
        headers.pad_lines_right(1);

        let max_len = headers
            .iter()
            .zip(ids.iter())
            .map(|(header, id)| header.len() + id.len())
            .max()
            .unwrap_or(0);

        let lines = if max_len > dimensions.width {
            headers
                .into_iter()
                .zip(ids.into_iter())
                .flat_map(|(header, id)| iter::once(header).chain(iter::once(Line(vec![id]))))
                .collect()
        } else {
            headers
                .iter_mut()
                .zip(ids.into_iter())
                .for_each(|(header, id)| header.0.push(id));
            headers
        };

        let max_len = lines.iter().map(|line| line.len()).max().unwrap_or(0);

        Ok(if max_len > dimensions.width {
            Lines(vec![Line::unstyled(
                "<Terminal too small for build details>",
            )?])
        } else {
            lines
        })
    }
}

#[cfg(test)]
mod tests {
    use std::time::SystemTime;

    use buck2_cli_proto::CommandResult;
    use buck2_cli_proto::GenericResponse;
    use buck2_data::LoadBuildFileEnd;
    use buck2_data::LoadBuildFileStart;
    use buck2_data::SpanEndEvent;
    use buck2_data::SpanStartEvent;
    use buck2_events::span::SpanId;
    use buck2_wrapper_common::invocation_id::TraceId;
    use superconsole::testing::frame_contains;
    use superconsole::testing::test_console;
    use superconsole::testing::SuperConsoleTestingExt;

    use super::*;

    #[tokio::test]
    async fn test_transfer_state_to_simpleconsole() {
        let trace_id = TraceId::new();
        let mut console = StatefulSuperConsole::new_with_root_forced(
            trace_id.dupe(),
            "test",
            Verbosity::Default,
            true,
            None,
            None,
            Default::default(),
            FileNameBuf::unchecked_new("placeholder"),
        )
        .unwrap();

        // start a new event.
        let id = SpanId::new();
        let event = Arc::new(BuckEvent::new(
            SystemTime::now(),
            trace_id,
            Some(id),
            None,
            buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                data: Some(buck2_data::span_start_event::Data::Load(
                    LoadBuildFileStart {
                        module_id: "foo".to_owned(),
                        cell: "bar".to_owned(),
                    },
                )),
            }),
        ));
        console.handle_event(&event).await.unwrap();

        // drop into simple console
        console
            .handle_command_result(&CommandResult {
                result: Some(buck2_cli_proto::command_result::Result::GenericResponse(
                    GenericResponse {},
                )),
            })
            .await
            .unwrap();

        // finish the event from before
        // expect to successfully close event.
        let event = Arc::new(BuckEvent::new(
            SystemTime::now(),
            TraceId::new(),
            Some(id),
            None,
            buck2_data::buck_event::Data::SpanEnd(SpanEndEvent {
                data: Some(buck2_data::span_end_event::Data::Load(LoadBuildFileEnd {
                    module_id: "foo".to_owned(),
                    cell: "bar".to_owned(),
                    error: None,
                })),
                stats: None,
                duration: None,
            }),
        ));
        assert!(console.handle_event(&event).await.is_ok());
    }

    #[tokio::test]
    async fn test_default_layout() -> anyhow::Result<()> {
        let trace_id = TraceId::new();
        let now = SystemTime::now();
        let tick = Tick::now();

        let mut console = StatefulSuperConsole::new(
            "build",
            trace_id.dupe(),
            test_console(),
            Verbosity::Default,
            true,
            Default::default(),
            Default::default(),
            FileNameBuf::unchecked_new("placeholder"),
        )?;

        console
            .handle_event(&Arc::new(BuckEvent::new(
                now,
                trace_id.dupe(),
                Some(SpanId::new()),
                None,
                buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                    data: Some(
                        buck2_data::CommandStart {
                            metadata: Default::default(),
                            data: Some(buck2_data::BuildCommandStart {}.into()),
                        }
                        .into(),
                    ),
                }),
            )))
            .await?;

        console
            .handle_event(&Arc::new(BuckEvent::new(
                now,
                trace_id.dupe(),
                None,
                None,
                buck2_data::InstantEvent {
                    data: Some(
                        buck2_data::RemoteExecutionSessionCreated {
                            session_id: "reSessionID-123".to_owned(),
                            experiment_name: "".to_owned(),
                        }
                        .into(),
                    ),
                }
                .into(),
            )))
            .await?;

        console
            .handle_event(&Arc::new(BuckEvent::new(
                now,
                trace_id.dupe(),
                Some(SpanId::new()),
                None,
                SpanStartEvent {
                    data: Some(
                        LoadBuildFileStart {
                            module_id: "foo".to_owned(),
                            cell: "bar".to_owned(),
                        }
                        .into(),
                    ),
                }
                .into(),
            )))
            .await?;

        console.tick(&tick).await?;

        let frame = console
            .super_console
            .as_mut()
            .context("Console was downgraded")?
            .test_output_mut()?
            .frames
            .pop()
            .context("No frame was emitted")?;

        // Verify we have the right output on intermediate frames
        if cfg!(fbcode_build) {
            assert!(frame_contains(&frame, "Buck UI:"));
        } else {
            assert!(frame_contains(&frame, "Build ID:"));
        }
        assert!(frame_contains(&frame, "RE: reSessionID-123"));
        assert!(frame_contains(&frame, "In progress"));

        console
            .handle_command_result(&buck2_cli_proto::CommandResult { result: None })
            .await?;

        Ok(())
    }

    #[test]
    fn test_session_info() -> anyhow::Result<()> {
        let info = SessionInfo {
            trace_id: TraceId::null(),
            test_session: Some(buck2_data::TestSessionInfo {
                info: (0..100).map(|_| "a").collect(),
            }),
        };

        let full = SessionInfoComponent {
            session_info: &info,
        }
        .draw_unchecked(
            &superconsole::State::new(),
            Dimensions {
                // Enough to print everything on one line (we need 109 in fbcode and 110 in OSS)
                width: 110,
                height: 1,
            },
            DrawMode::Normal,
        )?;

        assert_eq!(full.len(), 2);

        let multiline = SessionInfoComponent {
            session_info: &info,
        }
        .draw_unchecked(
            &superconsole::State::new(),
            Dimensions {
                // Just long enough to print each on one line.
                width: 100,
                height: 1,
            },
            DrawMode::Normal,
        )?;

        assert_eq!(multiline.len(), 4);

        let too_small = SessionInfoComponent {
            session_info: &info,
        }
        .draw_unchecked(
            &superconsole::State::new(),
            Dimensions {
                width: 1,
                height: 1,
            },
            DrawMode::Normal,
        )?;

        assert_eq!(too_small.len(), 1);

        Ok(())
    }
}
