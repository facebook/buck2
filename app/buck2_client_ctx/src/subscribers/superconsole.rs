/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::io::Write;
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use buck2_data::CommandExecutionDetails;
use buck2_error::BuckErrorContext;
use buck2_event_observer::action_sub_error_display::ActionSubErrorDisplay;
use buck2_event_observer::display;
use buck2_event_observer::display::TargetDisplayOptions;
use buck2_event_observer::display::display_file_watcher_end;
use buck2_event_observer::event_observer::DebugEventObserverExtra;
use buck2_event_observer::session_info::SessionInfo;
use buck2_event_observer::unpack_event::VisitorError;
use buck2_event_observer::unpack_event::unpack_event;
use buck2_event_observer::verbosity::Verbosity;
use buck2_event_observer::what_ran::command_to_string;
use buck2_event_observer::what_ran::worker_command_as_fallback_to_string;
use buck2_events::BuckEvent;
use buck2_health_check::report::DisplayReport;
use buck2_wrapper_common::invocation_id::TraceId;
use gazebo::prelude::*;
use strum::IntoEnumIterator;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;
pub(crate) use superconsole::SuperConsole;
use superconsole::components::DrawVertical;
use superconsole::style::Attribute;
use superconsole::style::Color;
use superconsole::style::ContentStyle;
use superconsole::style::StyledContent;
use superconsole::style::Stylize;
use tokio::sync::mpsc::Receiver;

use crate::console_interaction_stream::SuperConsoleToggle;
use crate::subscribers::emit_event::emit_event_if_relevant;
use crate::subscribers::simpleconsole::SimpleConsole;
use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::superconsole::commands::CommandsComponent;
use crate::subscribers::superconsole::debug_events::DebugEventsComponent;
use crate::subscribers::superconsole::debugger::StarlarkDebuggerComponent;
use crate::subscribers::superconsole::dice::DiceComponent;
use crate::subscribers::superconsole::header::TasksHeader;
use crate::subscribers::superconsole::io::IoHeader;
use crate::subscribers::superconsole::re::ReHeader;
use crate::subscribers::superconsole::session_info::SessionInfoComponent;
use crate::subscribers::superconsole::system_warning::SystemWarningComponent;
use crate::subscribers::superconsole::test::TestHeader;
use crate::subscribers::superconsole::timed_list::Cutoffs;
use crate::subscribers::superconsole::timed_list::TimedList;
use crate::subscribers::superconsole::timekeeper::Timekeeper;
use crate::ticker::Tick;

mod commands;
mod common;
pub(crate) mod debug_events;
mod debugger;
pub(crate) mod dice;
mod header;
pub(crate) mod io;
mod message_renderer;
mod re;
pub mod session_info;
pub(crate) mod system_warning;
pub mod timekeeper;

pub mod test;
pub mod timed_list;

const SUPERCONSOLE_WIDTH: usize = 300;

pub const CUTOFFS: Cutoffs = Cutoffs {
    inform: Duration::from_secs(4),
    warn: Duration::from_secs(8),
    _notable: Duration::from_millis(200),
};

#[allow(clippy::large_enum_variant)]
pub enum StatefulSuperConsole {
    Running(StatefulSuperConsoleImpl),
    /// After receiving the command output, any stdout, or an event stream error, the superconsole
    /// will be "finalized" and any further events are handled by simpleconsole.
    Finalized(SimpleConsole<DebugEventObserverExtra>),
}

pub struct StatefulSuperConsoleImpl {
    header: String,
    state: SuperConsoleState,
    super_console: SuperConsole,
    verbosity: Verbosity,
}

pub struct SuperConsoleState {
    timekeeper: Timekeeper,
    /// This contains the SpanTracker, which is why it's part of the SuperConsoleState.
    simple_console: SimpleConsole<DebugEventObserverExtra>,
    config: SuperConsoleConfig,
    active_warnings: Option<Vec<DisplayReport>>,
}

impl SuperConsoleState {
    pub fn extra(&self) -> &DebugEventObserverExtra {
        self.simple_console.observer.extra()
    }
}

#[derive(Clone)]
pub struct SuperConsoleConfig {
    pub enable_dice: bool,
    pub enable_debug_events: bool,
    pub enable_detailed_re: bool,
    pub enable_io: bool,
    pub enable_commands: bool,
    pub display_platform: bool,
    pub expanded_progress: bool,
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
            expanded_progress: true,
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

impl Component for BuckRootComponent<'_> {
    type Error = buck2_error::Error;

    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> buck2_error::Result<Lines> {
        // bound all components to our recommended grapheme-width
        let dimensions = dimensions.intersect(Dimensions {
            width: SUPERCONSOLE_WIDTH,
            height: usize::MAX,
        });

        let mut draw = DrawVertical::new(dimensions);

        let last_snapshot = self
            .state
            .simple_console
            .observer
            .two_snapshots()
            .last
            .as_ref()
            .map(|s| &s.1);
        let system_info = self.state.simple_console.observer.system_info();
        let health_check_reports = self.state.active_warnings.as_ref();
        {
            draw.draw(
                &SystemWarningComponent {
                    last_snapshot,
                    system_info,
                    health_check_reports,
                },
                mode,
            )?;
        }

        draw.draw(
            &SessionInfoComponent {
                session_info: self.state.session_info(),
            },
            mode,
        )?;
        draw.draw(
            &ReHeader {
                super_console_config: &self.state.config,
                re_state: self.state.simple_console.observer.re_state(),
                two_snapshots: self.state.simple_console.observer.two_snapshots(),
            },
            mode,
        )?;
        draw.draw(
            &IoHeader {
                super_console_config: &self.state.config,
                two_snapshots: self.state.simple_console.observer.two_snapshots(),
            },
            mode,
        )?;
        draw.draw(
            &TestHeader {
                session_info: self.state.session_info(),
                test_state: self.state.simple_console.observer.test_state(),
            },
            mode,
        )?;
        draw.draw(
            &DebugEventsComponent {
                super_console_config: &self.state.config,
                debug_events_state: self.state.simple_console.observer.extra().debug_events(),
            },
            mode,
        )?;
        draw.draw(
            &DiceComponent {
                super_console_config: &self.state.config,
                dice_state: self.state.simple_console.observer.dice_state(),
            },
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
            mode,
        )?;
        draw.draw(
            &CommandsComponent {
                super_console_config: &self.state.config,
                action_stats: self.state.simple_console.observer.action_stats(),
            },
            mode,
        )?;
        draw.draw(&TasksHeader::new(&self.header, self.state), mode)?;
        draw.draw(&TimedList::new(&CUTOFFS, self.state), mode)?;

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
        expect_spans: bool,
        timekeeper: Timekeeper,
        stream: Option<Box<dyn Write + Send + 'static + Sync>>,
        config: SuperConsoleConfig,
        health_check_reports_receiver: Option<Receiver<Vec<DisplayReport>>>,
    ) -> buck2_error::Result<Self> {
        let mut builder = Self::console_builder();
        if let Some(stream) = stream {
            builder.write_to(stream);
        }
        Self::new(
            command_name,
            trace_id,
            builder.build_forced(Self::FALLBACK_SIZE)?,
            verbosity,
            expect_spans,
            timekeeper,
            config,
            health_check_reports_receiver,
        )
    }

    pub(crate) fn new(
        command_name: &str,
        trace_id: TraceId,
        super_console: SuperConsole,
        verbosity: Verbosity,
        expect_spans: bool,
        timekeeper: Timekeeper,
        config: SuperConsoleConfig,
        health_check_reports_receiver: Option<Receiver<Vec<DisplayReport>>>,
    ) -> buck2_error::Result<Self> {
        let header = format!("Command: {command_name}.");
        Ok(Self::Running(StatefulSuperConsoleImpl {
            header,
            state: SuperConsoleState::new(
                timekeeper,
                trace_id,
                verbosity,
                expect_spans,
                config,
                health_check_reports_receiver,
            )?,
            super_console,
            verbosity,
        }))
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
            lines
                .0
                .extend(Lines::from_multiline_string_raw(&e.message, style).0);
        }
        lines
    }

    async fn handle_event(&mut self, ev: &Arc<BuckEvent>) -> buck2_error::Result<()> {
        match self {
            Self::Running(c) => c.handle_event(ev).await,
            Self::Finalized(c) => c.handle_event(ev).await,
        }
    }

    fn finalize(&mut self) -> buck2_error::Result<()> {
        let mut res = Ok(());
        take_mut::take(self, |this| match this {
            Self::Running(super_console) => {
                let (state, err) = super_console.finalize();
                if let Some(err) = err {
                    res = Err(err);
                }
                Self::Finalized(state.simple_console)
            }
            v => v,
        });

        res.map_err(Into::into)
    }
}

impl SuperConsoleState {
    pub fn new(
        timekeeper: Timekeeper,
        trace_id: TraceId,
        verbosity: Verbosity,
        expect_spans: bool,
        config: SuperConsoleConfig,
        health_check_reports_receiver: Option<Receiver<Vec<DisplayReport>>>,
    ) -> buck2_error::Result<SuperConsoleState> {
        Ok(SuperConsoleState {
            timekeeper,
            simple_console: SimpleConsole::with_tty(
                trace_id,
                verbosity,
                expect_spans,
                health_check_reports_receiver,
            ),
            config,
            active_warnings: None,
        })
    }

    pub async fn update_event_observer(
        &mut self,
        event: &Arc<BuckEvent>,
    ) -> buck2_error::Result<()> {
        self.simple_console.update_event_observer(event).await
    }

    pub fn session_info(&self) -> &SessionInfo {
        self.simple_console.observer.session_info()
    }

    pub fn tick(&mut self, tick: Tick) {
        self.timekeeper.tick(tick);
    }
}

pub(crate) const BUCK_NO_INTERACTIVE_CONSOLE: &str = "BUCK_NO_INTERACTIVE_CONSOLE";

impl StatefulSuperConsoleImpl {
    async fn toggle(
        &mut self,
        what: &str,
        key: char,
        var: impl FnOnce(&mut Self) -> &mut bool,
    ) -> buck2_error::Result<()> {
        let var = var(self);
        *var = !*var;
        let on_off = match *var {
            true => "on",
            false => "off",
        };
        self.handle_stderr(&format!("{what}: {on_off}, press `{key}` to revert"))
            .await
    }

    async fn handle_event(&mut self, event: &Arc<BuckEvent>) -> buck2_error::Result<()> {
        self.state.update_event_observer(event).await?;

        self.handle_inner_event(event)
            .await
            .with_buck_error_context(|| display::InvalidBuckEvent(event.clone()).to_string())?;

        if self.verbosity.print_all_commands() {
            emit_event_if_relevant(
                event.parent_id().into(),
                event.data(),
                self.state.simple_console.observer().spans(),
                &mut self.super_console,
            )?;
        }
        Ok(())
    }

    async fn handle_inner_event(&mut self, event: &BuckEvent) -> buck2_error::Result<()> {
        match unpack_event(event)? {
            buck2_event_observer::unpack_event::UnpackedBuckEvent::SpanStart(_, _, _) => Ok(()),
            buck2_event_observer::unpack_event::UnpackedBuckEvent::SpanEnd(_, _, data) => {
                match data {
                    buck2_data::span_end_event::Data::ActionExecution(action) => {
                        self.handle_action_execution_end(action).await
                    }
                    buck2_data::span_end_event::Data::FileWatcher(file_watcher) => {
                        self.handle_file_watcher_end(file_watcher).await
                    }
                    _ => Ok(()),
                }
            }
            buck2_event_observer::unpack_event::UnpackedBuckEvent::Instant(_, _, data) => {
                match data {
                    buck2_data::instant_event::Data::ConsoleMessage(message) => {
                        self.handle_console_message(message).await
                    }
                    buck2_data::instant_event::Data::ConsoleWarning(message) => {
                        self.handle_console_warning(message).await
                    }
                    buck2_data::instant_event::Data::StructuredError(err) => {
                        self.handle_structured_error(err).await
                    }
                    buck2_data::instant_event::Data::TestResult(result) => {
                        self.handle_test_result(result).await
                    }
                    buck2_data::instant_event::Data::ConsolePreferences(preferences) => {
                        self.handle_console_preferences(preferences).await
                    }
                    buck2_data::instant_event::Data::ActionError(error) => {
                        self.handle_action_error(error).await
                    }
                    buck2_data::instant_event::Data::StreamingOutput(message) => {
                        self.handle_streaming_output(message).await
                    }
                    _ => Ok(()),
                }
            }
            buck2_event_observer::unpack_event::UnpackedBuckEvent::UnrecognizedSpanStart(_, _)
            | buck2_event_observer::unpack_event::UnpackedBuckEvent::UnrecognizedSpanEnd(_, _)
            | buck2_event_observer::unpack_event::UnpackedBuckEvent::UnrecognizedInstant(_, _) => {
                Err(VisitorError::MissingField(event.clone()).into())
            }
        }
    }

    async fn handle_stderr(&mut self, msg: &str) -> buck2_error::Result<()> {
        self.super_console
            .emit(msg.lines().map(Line::sanitized).collect());
        Ok(())
    }

    async fn handle_structured_error(
        &mut self,
        err: &buck2_data::StructuredError,
    ) -> buck2_error::Result<()> {
        if err.quiet {
            return Ok(());
        }

        self.super_console.emit(
            err.payload
                .lines()
                .map(|line| Line::from_iter([Span::new_colored_lossy(line, Color::DarkYellow)]))
                .collect(),
        );
        Ok(())
    }

    async fn handle_file_watcher_end(
        &mut self,
        file_watcher: &buck2_data::FileWatcherEnd,
    ) -> buck2_error::Result<()> {
        if self.verbosity.print_status() {
            self.super_console.emit(Lines(
                display_file_watcher_end(file_watcher).into_map(|x| Line::sanitized(&x)),
            ));
        }

        Ok(())
    }

    async fn handle_console_message(
        &mut self,
        message: &buck2_data::ConsoleMessage,
    ) -> buck2_error::Result<()> {
        // TODO(nmj): Maybe better handling of messages that have color data in them. Right now
        //            they're just stripped
        self.super_console.emit(Lines::from_multiline_string_raw(
            &message.message,
            ContentStyle::default(),
        ));
        Ok(())
    }

    async fn handle_streaming_output(
        &mut self,
        message: &buck2_data::StdoutStreamingOutput,
    ) -> buck2_error::Result<()> {
        self.super_console
            .emit_aux(Lines::from_multiline_string_raw(
                &message.message,
                ContentStyle::default(),
            ));
        Ok(())
    }

    async fn handle_console_warning(
        &mut self,
        message: &buck2_data::ConsoleWarning,
    ) -> buck2_error::Result<()> {
        let style = ContentStyle {
            foreground_color: Some(Color::Yellow),
            ..Default::default()
        };
        self.super_console
            .emit(Lines::from_multiline_string_raw(&message.message, style));
        Ok(())
    }

    async fn handle_action_execution_end(
        &mut self,
        action: &buck2_data::ActionExecutionEnd,
    ) -> buck2_error::Result<()> {
        if action.error.is_some() {
            // Don't handle action errors here. We deal with them as a part of a separate
            // `ActionError` event
            return Ok(());
        }

        if let Some(stderr) = display::success_stderr(action, self.verbosity)? {
            let mut lines = vec![];
            let display_platform = self.state.config.display_platform;
            let action_id = StyledContent::new(
                ContentStyle {
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

            self.super_console.emit(Lines(lines));
        }

        Ok(())
    }

    async fn handle_action_error(
        &mut self,
        error: &buck2_data::ActionError,
    ) -> buck2_error::Result<()> {
        let mut lines = vec![];
        let display_platform = self.state.config.display_platform;

        let display::ActionErrorDisplay {
            action_id,
            reason,
            command,
            error_diagnostics,
        } = display::display_action_error(
            error,
            TargetDisplayOptions::for_console(display_platform),
        )?;

        lines.push(Line::from_iter([Span::new_styled_lossy(
            StyledContent::new(
                ContentStyle {
                    attributes: Attribute::Bold.into(),
                    foreground_color: Some(Color::Red),
                    ..Default::default()
                },
                format!("Action failed: {action_id}",),
            ),
        )]));

        lines.extend(
            reason.lines().map(|l| {
                Line::from_iter([Span::new_styled_lossy(l.to_owned().with(Color::DarkRed))])
            }),
        );

        if let Some(command) = command {
            lines_for_command_details(&command, self.verbosity, &mut lines);
        }

        if let Some(error_diagnostics) = error_diagnostics {
            match error_diagnostics.data.as_ref().unwrap() {
                buck2_data::action_error_diagnostics::Data::SubErrors(sub_errors) => {
                    let sub_errors = &sub_errors.sub_errors;
                    if !sub_errors.is_empty() {
                        for sub_error in sub_errors {
                            // Display errors based on show_in_stderr flag is true
                            if sub_error.show_in_stderr {
                                if let Some(display_msg) = sub_error.display() {
                                    lines.push(Line::from_iter([Span::new_styled_lossy(
                                        display_msg.with(Color::DarkCyan),
                                    )]))
                                }
                            }
                        }
                    }
                }
                buck2_data::action_error_diagnostics::Data::HandlerInvocationError(error) => {
                    let colored_error = error.clone().with(Color::DarkRed).to_string();
                    lines.extend(Lines::from_colored_multiline_string(&colored_error));
                }
            };
        }

        self.super_console.emit(Lines(lines));

        Ok(())
    }

    async fn handle_test_result(
        &mut self,
        result: &buck2_data::TestResult,
    ) -> buck2_error::Result<()> {
        if let Some(msg) = display::format_test_result(result, self.verbosity)? {
            self.super_console.emit(msg);
        }

        Ok(())
    }

    async fn handle_console_preferences(
        &mut self,
        prefs: &buck2_data::ConsolePreferences,
    ) -> buck2_error::Result<()> {
        self.state.config.max_lines = prefs.max_lines.try_into()?;

        Ok(())
    }

    async fn handle_console_interaction(
        &mut self,
        c: &Option<SuperConsoleToggle>,
    ) -> buck2_error::Result<()> {
        match c {
            Some(c) => match c {
                SuperConsoleToggle::Dice => {
                    self.toggle(c.description(), c.key(), |s| {
                        &mut s.state.config.enable_dice
                    })
                    .await?
                }
                SuperConsoleToggle::DebugEvents => {
                    self.toggle(c.description(), c.key(), |s| {
                        &mut s.state.config.enable_debug_events
                    })
                    .await?
                }
                SuperConsoleToggle::TwoLinesMode => {
                    self.toggle(c.description(), c.key(), |s| &mut s.state.config.two_lines)
                        .await?
                }
                SuperConsoleToggle::DetailedRE => {
                    self.toggle(c.description(), c.key(), |s| {
                        &mut s.state.config.enable_detailed_re
                    })
                    .await?
                }
                SuperConsoleToggle::Io => {
                    self.toggle(c.description(), c.key(), |s| &mut s.state.config.enable_io)
                        .await?
                }
                SuperConsoleToggle::TargetConfigurations => {
                    self.toggle(c.description(), c.key(), |s| {
                        &mut s.state.config.display_platform
                    })
                    .await?
                }
                SuperConsoleToggle::ExpandedProgress => {
                    self.toggle(c.description(), c.key(), |s| {
                        &mut s.state.config.expanded_progress
                    })
                    .await?
                }
                SuperConsoleToggle::Commands => {
                    self.toggle(c.description(), c.key(), |s| {
                        &mut s.state.config.enable_commands
                    })
                    .await?
                }
                SuperConsoleToggle::IncrLines => {
                    self.state.config.max_lines = self.state.config.max_lines.saturating_add(1)
                }
                SuperConsoleToggle::DecrLines => {
                    self.state.config.max_lines = self.state.config.max_lines.saturating_sub(1)
                }
                SuperConsoleToggle::IncreaseReplaySpeed => {
                    if let Some(message) = self.state.timekeeper.scale_speed(1.5).await {
                        self.handle_stderr(&message).await?;
                    }
                }
                SuperConsoleToggle::DecreaseReplaySpeed => {
                    if let Some(message) = self.state.timekeeper.scale_speed(1.0 / 1.5).await {
                        self.handle_stderr(&message).await?;
                    }
                }
                SuperConsoleToggle::PauseReplay => {
                    if let Some(message) = self.state.timekeeper.toggle_pause().await {
                        self.handle_stderr(&message).await?;
                    }
                }
                SuperConsoleToggle::Help => {
                    let help_message = SuperConsoleToggle::iter()
                        .map(|t| format!("`{}` = toggle {}", t.key(), t.description()))
                        .collect::<Vec<_>>()
                        .join("\n");
                    self.handle_stderr(&format!(
                        "Help:\n{}\nenv var {}=true disables interactive console",
                        help_message, BUCK_NO_INTERACTIVE_CONSOLE
                    ))
                    .await?
                }
            },
            None => {}
        }

        Ok(())
    }

    fn try_update_active_warnings(&mut self) {
        let reports = self
            .state
            .simple_console
            .try_recv_health_check_display_reports();
        if reports.is_some() {
            self.state.active_warnings = reports;
        }
    }
    async fn tick(&mut self, tick: &Tick) -> buck2_error::Result<()> {
        self.state.timekeeper.tick(*tick);
        self.try_update_active_warnings();
        self.super_console.render(&BuckRootComponent {
            header: &self.header,
            state: &self.state,
        })?;
        Ok(())
    }

    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> buck2_error::Result<()> {
        let lines = StatefulSuperConsole::render_result_errors(result);
        self.super_console.emit(lines);
        Ok(())
    }

    fn finalize(
        self,
    ) -> (
        SuperConsoleState,
        Option<superconsole::Error<buck2_error::Error>>,
    ) {
        let err = self
            .super_console
            .finalize(&BuckRootComponent {
                header: &self.header,
                state: &self.state,
            })
            .err();
        (self.state, err)
    }
}

#[async_trait]
impl EventSubscriber for StatefulSuperConsole {
    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> buck2_error::Result<()> {
        for ev in events {
            self.handle_event(ev).await?;
        }
        Ok(())
    }

    async fn handle_output(&mut self, raw_output: &[u8]) -> buck2_error::Result<()> {
        self.finalize()?;
        match self {
            Self::Running(_) => unreachable!(),
            Self::Finalized(c) => c.handle_output(raw_output).await,
        }
    }

    async fn handle_tailer_stderr(&mut self, stderr: &str) -> buck2_error::Result<()> {
        match self {
            StatefulSuperConsole::Running(c) => c.handle_stderr(stderr).await,
            StatefulSuperConsole::Finalized(c) => c.handle_stderr(stderr).await,
        }
    }

    async fn handle_console_interaction(
        &mut self,
        c: &Option<SuperConsoleToggle>,
    ) -> buck2_error::Result<()> {
        if let Self::Running(super_console) = self {
            super_console.handle_console_interaction(c).await?;
        }

        Ok(())
    }

    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> buck2_error::Result<()> {
        match self {
            Self::Running(c) => c.handle_command_result(result).await?,
            Self::Finalized(c) => c.handle_command_result(result).await?,
        }
        self.finalize()?;
        Ok(())
    }

    async fn tick(&mut self, tick: &Tick) -> buck2_error::Result<()> {
        if let Self::Running(super_console) = self {
            super_console.tick(tick).await?;
        }
        Ok(())
    }

    async fn handle_error(&mut self, _error: &buck2_error::Error) -> buck2_error::Result<()> {
        self.finalize()?;
        Ok(())
    }
}

fn lines_for_command_details(
    command_failed: &CommandExecutionDetails,
    verbosity: Verbosity,
    lines: &mut Vec<Line>,
) {
    if let Some(command_kind) = command_failed.command_kind.as_ref() {
        use buck2_data::command_execution_kind::Command;

        match command_kind.command.as_ref() {
            Some(Command::LocalCommand(local_command)) => {
                let command = command_to_string(local_command);
                let command = command.as_str();
                let command = if verbosity.print_failure_full_command() {
                    Cow::Borrowed(command)
                } else {
                    match truncate(command) {
                        None => Cow::Borrowed(command),
                        Some(short) => Cow::Owned(format!(
                            "{short} (run `buck2 log what-failed` to get the full command)"
                        )),
                    }
                };

                lines.push(Line::from_iter([Span::new_unstyled_lossy(format!(
                    "Reproduce locally: `{command}`"
                ))]));
            }
            Some(Command::RemoteCommand(remote_command)) => {
                let help_message = if buck2_core::is_open_source() {
                    format!("Remote action digest: `{}`", remote_command.action_digest)
                } else {
                    format!(
                        "Reproduce locally: `frecli cas download-action {}`",
                        remote_command.action_digest
                    )
                };

                lines.push(Line::from_iter([Span::new_styled_lossy(
                    help_message.with(Color::DarkRed),
                )]));
            }
            Some(Command::OmittedLocalCommand(..)) | None => {
                // Nothing to show in this case.
            }
            Some(Command::WorkerInitCommand(worker_init_command)) => {
                let command = command_to_string(worker_init_command);
                let command = command.as_str();
                let command = if verbosity.print_failure_full_command() {
                    Cow::Borrowed(command)
                } else {
                    match truncate(command) {
                        None => Cow::Borrowed(command),
                        Some(short) => Cow::Owned(format!(
                            "{short} (run `buck2 log what-failed` to get the full command)"
                        )),
                    }
                };

                lines.push(Line::from_iter([Span::new_unstyled_lossy(format!(
                    "Reproduce locally: `{command}`"
                ))]));
            }
            Some(Command::WorkerCommand(worker_command)) => {
                let command = worker_command_as_fallback_to_string(worker_command);
                let command = command.as_str();
                let command = if verbosity.print_failure_full_command() {
                    Cow::Borrowed(command)
                } else {
                    match truncate(command) {
                        None => Cow::Borrowed(command),
                        Some(short) => Cow::Owned(format!(
                            "{short} (run `buck2 log what-failed` to get the full command)"
                        )),
                    }
                };

                lines.push(Line::from_iter([Span::new_unstyled_lossy(format!(
                    "Reproduce locally: `{command}`"
                ))]));
            }
        };
    }

    lines.push(Line::from_iter([Span::new_styled_lossy(
        "stdout:"
            .to_owned()
            .with(Color::DarkRed)
            .attribute(Attribute::Bold),
    )]));
    lines.extend(Lines::from_colored_multiline_string(
        &command_failed.cmd_stdout,
    ));
    lines.push(Line::from_iter([Span::new_styled_lossy(
        "stderr:"
            .to_owned()
            .with(Color::DarkRed)
            .attribute(Attribute::Bold),
    )]));
    lines.extend(Lines::from_colored_multiline_string(
        &command_failed.cmd_stderr,
    ));

    if let Some(additional_message) = &command_failed.additional_message {
        if !additional_message.is_empty() {
            lines.push(Line::from_iter([Span::new_styled_lossy(
                "info:"
                    .to_owned()
                    .with(Color::DarkRed)
                    .attribute(Attribute::Bold),
            )]));
            lines.extend(Lines::from_colored_multiline_string(additional_message));
        }
    }
}

// Truncates a string to a reasonable number characters, or returns None if it doesn't need truncating.
fn truncate(contents: &str) -> Option<String> {
    const MAX_LENGTH: usize = 200;
    const BUFFER: usize = " ...<omitted>... ".len();
    if contents.len() > MAX_LENGTH + BUFFER {
        Some(format!(
            "{} ...<omitted>... {}",
            &contents[0..contents.ceil_char_boundary(MAX_LENGTH / 2)],
            &contents
                [contents.floor_char_boundary(contents.len() - MAX_LENGTH / 2)..contents.len()]
        ))
    } else {
        None
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
    use buck2_event_observer::span_tracker::EventTimestamp;
    use buck2_events::span::SpanId;
    use dupe::Dupe;
    use superconsole::testing::SuperConsoleTestingExt;
    use superconsole::testing::assert_frame_contains;
    use superconsole::testing::test_console;

    use super::*;
    use crate::subscribers::superconsole::timekeeper::RealtimeClock;

    #[tokio::test]
    async fn test_transfer_state_to_simpleconsole() -> buck2_error::Result<()> {
        let trace_id = TraceId::new();
        let mut console = StatefulSuperConsole::new_with_root_forced(
            trace_id.dupe(),
            "test",
            Verbosity::default(),
            true,
            Timekeeper::new(
                Box::new(RealtimeClock),
                EventTimestamp(SystemTime::now().into()),
            ),
            None,
            Default::default(),
            None,
        )
        .unwrap();

        // start a new event.
        let id = SpanId::next();
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
                    starlark_peak_allocated_bytes: Some(0),
                    cpu_instruction_count: None,
                    target_count: Some(10),
                })),
                stats: None,
                duration: None,
            }),
        ));
        assert!(console.handle_event(&event).await.is_ok());
        Ok(())
    }

    #[tokio::test]
    async fn test_default_layout() -> buck2_error::Result<()> {
        let trace_id = TraceId::new();
        let now = SystemTime::now();
        let tick = Tick::now();

        let mut console = StatefulSuperConsole::new(
            "build",
            trace_id.dupe(),
            test_console(),
            Verbosity::default(),
            true,
            Timekeeper::new(
                Box::new(RealtimeClock),
                EventTimestamp(SystemTime::now().into()),
            ),
            Default::default(),
            None,
        )?;

        console
            .handle_event(&Arc::new(BuckEvent::new(
                now,
                trace_id.dupe(),
                Some(SpanId::next()),
                None,
                buck2_data::buck_event::Data::SpanStart(SpanStartEvent {
                    data: Some(
                        buck2_data::CommandStart {
                            data: Some(buck2_data::BuildCommandStart {}.into()),
                            ..Default::default()
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
                            persistent_cache_mode: None,
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
                Some(SpanId::next()),
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

        let frame = match &mut console {
            StatefulSuperConsole::Running(c) => c
                .super_console
                .test_output_mut()
                .frames
                .pop()
                .buck_error_context("No frame was emitted")?,
            StatefulSuperConsole::Finalized(_) => {
                panic!("Console was downgraded");
            }
        };

        // Verify we have the right output on intermediate frames
        if cfg!(fbcode_build) {
            assert_frame_contains(&frame, "Buck UI:");
        } else {
            assert_frame_contains(&frame, "Build ID:");
        }
        assert_frame_contains(&frame, "Network:");
        assert_frame_contains(&frame, "(reSessionID-123)");
        assert_frame_contains(&frame, "Remaining");

        console
            .handle_command_result(&buck2_cli_proto::CommandResult { result: None })
            .await?;

        Ok(())
    }

    #[test]
    fn test_session_info() -> buck2_error::Result<()> {
        let info = SessionInfo {
            trace_id: TraceId::null(),
            test_session: Some(buck2_data::TestSessionInfo {
                info: (0..100).map(|_| "a").collect(),
            }),
            legacy_dice: false,
        };

        let full = SessionInfoComponent {
            session_info: &info,
        }
        .draw_unchecked(
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
            Dimensions {
                width: 1,
                height: 1,
            },
            DrawMode::Normal,
        )?;

        assert_eq!(too_small.len(), 1);

        Ok(())
    }

    #[tokio::test]
    async fn test_tailer_stderr() -> buck2_error::Result<()> {
        let trace_id = TraceId::new();
        let tick = Tick::now();

        let mut console = StatefulSuperConsole::new(
            "build",
            trace_id.dupe(),
            test_console(),
            Verbosity::default(),
            true,
            Timekeeper::new(
                Box::new(RealtimeClock),
                EventTimestamp(SystemTime::now().into()),
            ),
            Default::default(),
            None,
        )?;

        console.handle_tailer_stderr("some stderr output").await?;
        console.tick(&tick).await?;

        let frame = match &mut console {
            StatefulSuperConsole::Running(c) => c
                .super_console
                .test_output_mut()
                .frames
                .pop()
                .buck_error_context("No frame was emitted")?,
            StatefulSuperConsole::Finalized(_) => {
                panic!("Console was downgraded");
            }
        };

        assert_frame_contains(&frame, "some stderr output");
        Ok(())
    }
}
