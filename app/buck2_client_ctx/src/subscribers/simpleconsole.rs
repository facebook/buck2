/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt::Display;
use std::io::Write;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use async_trait::async_trait;
use buck2_data::CommandExecutionDetails;
use buck2_events::BuckEvent;
use gazebo::prelude::*;
use lsp_server::Message;
use superconsole::DrawMode;
use superconsole::SuperConsole;
use termwiz::escape::Action;
use termwiz::escape::ControlCode;

use crate::subscribers::display;
use crate::subscribers::display::TargetDisplayOptions;
use crate::subscribers::humanized_bytes::HumanizedBytes;
use crate::subscribers::io::io_in_flight_non_zero_counters;
use crate::subscribers::io::IoState;
use crate::subscribers::last_command_execution_kind::get_last_command_execution_kind;
use crate::subscribers::last_command_execution_kind::LastCommandExecutionKind;
use crate::subscribers::re_panel::RePanel;
use crate::subscribers::span_tracker::SpanTracker;
use crate::subscribers::subscriber::Tick;
use crate::subscribers::subscriber_unpack::UnpackingEventSubscriber;
use crate::subscribers::two_snapshots::TwoSnapshots;
use crate::verbosity::Verbosity;
use crate::what_ran;
use crate::what_ran::local_command_to_string;
use crate::what_ran::WhatRanCommandConsoleFormat;
use crate::what_ran::WhatRanOptions;
use crate::what_ran::WhatRanOutputCommand;
use crate::what_ran::WhatRanOutputWriter;

/// buck2 daemon info is printed to stderr if there are no other updates available
/// within this duration.
const KEEPALIVE_TIME_LIMIT: Duration = Duration::from_secs(7);

fn now_display() -> impl Display {
    chrono::Local::now().to_rfc3339_opts(::chrono::SecondsFormat::Millis, false)
}

fn echo_impl(message: &str) -> anyhow::Result<()> {
    let now = now_display();
    for line in message.lines() {
        if line.is_empty() {
            // patternlint-disable-next-line buck2-cli-simpleconsole-echo
            crate::eprintln!("[{}]", now)?;
        } else {
            // patternlint-disable-next-line buck2-cli-simpleconsole-echo
            crate::eprintln!("[{}] {}", now, line)?;
        }
    }
    Ok(())
}

// Echoes a message to stderr, along with a timestamp.
macro_rules! echo {
    () => {
        {
            // patternlint-disable-next-line buck2-cli-simpleconsole-echo
            crate::eprintln!("[{}]", now_display())
        }
    };
    ($fmt:expr $(, $args:expr)*) => {
        {
            let message = format!($fmt $(, $args)*);
            echo_impl(&message)
        }
    };
}

#[derive(Copy, Clone, Dupe, Debug, PartialEq)]
enum TtyMode {
    Enabled,
    Disabled,
}

struct ActionError {
    display: display::ActionErrorDisplay<'static>,
}

impl ActionError {
    fn print(&self, tty_mode: TtyMode) -> anyhow::Result<()> {
        echo!("Action failed: {}", self.display.action_id)?;
        echo!("{}", self.display.reason)?;
        if let Some(command) = &self.display.command {
            eprint_command_details(command, tty_mode)?;
        }
        Ok(())
    }
}

fn eprint_command_details(
    command_failed: &CommandExecutionDetails,
    tty_mode: TtyMode,
) -> anyhow::Result<()> {
    use buck2_data::command_execution_details::Command;

    match command_failed.command.as_ref() {
        Some(Command::LocalCommand(local_command)) => {
            echo!("Local command: {}", local_command_to_string(local_command))?;
        }
        Some(Command::RemoteCommand(remote_command)) => {
            echo!(
                "Remote action, reproduce with: `frecli cas download-action {}`",
                remote_command.action_digest
            )?;
        }
        Some(Command::OmittedLocalCommand(..)) | None => {
            // Nothing to show in this case.
        }
    };
    let (stdout, stderr) = if tty_mode == TtyMode::Disabled {
        (
            SimpleConsole::sanitize_output_colors(command_failed.stdout.as_bytes()),
            SimpleConsole::sanitize_output_colors(command_failed.stderr.as_bytes()),
        )
    } else {
        (command_failed.stdout.clone(), command_failed.stderr.clone())
    };
    echo!("Stdout: {}", stdout)?;
    echo!("Stderr: {}", stderr)?;
    Ok(())
}

/// Records the number of actions depending on how they executed.
/// There's no overlap between the actions - summing them all up
/// gives the total number of actions. `local_actions` + `remote_actions`
/// provides the total number of actually executed actions while
/// `cached_actions` provides number of actions which we found
/// in the action cache.
///
/// These stats only track executions/commands.
#[derive(Default)]
pub(crate) struct ActionStats {
    pub(crate) local_actions: u64,
    pub(crate) remote_actions: u64,
    pub(crate) cached_actions: u64,
}

impl ActionStats {
    pub(crate) fn action_cache_hit_percentage(&self) -> u8 {
        // We want special semantics for the return value: the terminal values (0% and 100%)
        // should _only_ be used when there are exactly no cache hits and full cache hits.
        // So, even if we have 99.6% cache hits, we want to display 99% and conversely,
        // if the value is 0.1% cache hits, we want to display 1%.
        //
        // This allows us to have special meaning for 0% and 100%: complete cache-divergence
        // and fully cacheable builds.
        let total_actions = self.total_executed_and_cached_actions();
        if total_actions == 0 || self.cached_actions == total_actions {
            100
        } else if self.cached_actions == 0 {
            0
        } else {
            let hit_percent = ((self.cached_actions as f64) / (total_actions as f64)) * 100f64;
            let integral_percent = (hit_percent.round()) as u8;
            std::cmp::max(1, std::cmp::min(integral_percent, 99))
        }
    }

    pub(crate) fn total_executed_and_cached_actions(&self) -> u64 {
        self.local_actions + self.remote_actions + self.cached_actions
    }

    pub(crate) fn update(&mut self, action: &buck2_data::ActionExecutionEnd) {
        match get_last_command_execution_kind(action) {
            LastCommandExecutionKind::Local => {
                self.local_actions += 1;
            }
            LastCommandExecutionKind::Cached => {
                self.cached_actions += 1;
            }
            LastCommandExecutionKind::Remote => {
                self.remote_actions += 1;
            }
            LastCommandExecutionKind::NoCommand => {}
        }
    }

    pub(crate) fn log_stats(&self) -> bool {
        self.total_executed_and_cached_actions() > 0
    }
}

/// Just repeats stdout and stderr to client process.
pub(crate) struct SimpleConsole {
    tty_mode: TtyMode,
    verbosity: Verbosity,
    // Whether to show "Waiting for daemon..." when no root spans are received
    show_waiting_message: bool,
    span_tracker: SpanTracker,
    action_errors: Vec<ActionError>,
    last_print_time: Instant,
    test_session: Option<String>,
    action_stats: ActionStats,
    re_panel: RePanel,
    pub(crate) io_state: IoState,
    two_snapshots: TwoSnapshots,
}

impl SimpleConsole {
    pub(crate) fn with_tty(verbosity: Verbosity, show_waiting_message: bool) -> Self {
        SimpleConsole {
            tty_mode: TtyMode::Enabled,
            verbosity,
            show_waiting_message,
            span_tracker: SpanTracker::new(),
            action_errors: Vec::new(),
            last_print_time: Instant::now(),
            test_session: None,
            action_stats: ActionStats::default(),
            re_panel: RePanel::new(),
            io_state: IoState::default(),
            two_snapshots: TwoSnapshots::default(),
        }
    }

    pub(crate) fn without_tty(verbosity: Verbosity, show_waiting_message: bool) -> Self {
        SimpleConsole {
            tty_mode: TtyMode::Disabled,
            verbosity,
            show_waiting_message,
            span_tracker: SpanTracker::new(),
            action_errors: Vec::new(),
            last_print_time: Instant::now(),
            test_session: None,
            action_stats: ActionStats::default(),
            re_panel: RePanel::new(),
            io_state: IoState::default(),
            two_snapshots: TwoSnapshots::default(),
        }
    }

    /// Create a SimpleConsole that auto detects whether it has a TTY or not.
    pub(crate) fn autodetect(verbosity: Verbosity, show_waiting_message: bool) -> Self {
        match SuperConsole::compatible() {
            true => Self::with_tty(verbosity, show_waiting_message),
            false => Self::without_tty(verbosity, show_waiting_message),
        }
    }

    pub(crate) fn spans(&self) -> &SpanTracker {
        &self.span_tracker
    }

    pub(crate) fn action_stats(&self) -> &ActionStats {
        &self.action_stats
    }

    pub(crate) fn action_stats_mut(&mut self) -> &mut ActionStats {
        &mut self.action_stats
    }

    pub(crate) fn re_panel(&self) -> &RePanel {
        &self.re_panel
    }

    pub(crate) fn re_panel_mut(&mut self) -> &mut RePanel {
        &mut self.re_panel
    }

    pub(crate) fn update_span_tracker(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.span_tracker
            .handle_event(event)
            .context("Error tracking event")
    }

    fn sanitize_output_colors(stderr: &[u8]) -> String {
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

    fn notify_printed(&mut self) {
        self.last_print_time = Instant::now();
    }

    fn print_stats_while_waiting(&mut self) -> anyhow::Result<()> {
        if let Some(h) = self.re_panel.render_header(DrawMode::Normal) {
            echo!("{}", h)?;
        }

        {
            let mut parts = Vec::with_capacity(2);
            if let Some((_, snapshot)) = &self.two_snapshots.last {
                if snapshot.buck2_rss != 0 {
                    parts.push(format!("RSS: {}", HumanizedBytes::new(snapshot.buck2_rss)));
                }
            }
            if let Some(cpu) = self.two_snapshots.cpu_percents() {
                parts.push(format!("CPU: {}%", cpu));
            }
            if !parts.is_empty() {
                echo!("Resource usage: {}", parts.join(" "))?;
            }
        }

        {
            if let Some((_, snapshot)) = &self.two_snapshots.last {
                let mut parts = Vec::new();
                for (key, value) in io_in_flight_non_zero_counters(snapshot) {
                    parts.push(format!("{:?}: {}", key, value));
                }
                if !parts.is_empty() {
                    echo!("IO: {}", parts.join(" "))?;
                } else {
                    echo!("IO: none")?;
                }
            }
        }

        Ok(())
    }
}

#[async_trait]
impl UnpackingEventSubscriber for SimpleConsole {
    async fn handle_output(&mut self, raw_output: &str) -> anyhow::Result<()> {
        crate::print!("{}", raw_output)?;
        self.notify_printed();
        Ok(())
    }

    async fn handle_stderr(&mut self, stderr: &str) -> anyhow::Result<()> {
        echo!("{}", stderr)?;
        self.notify_printed();
        Ok(())
    }

    async fn handle_event(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.update_span_tracker(event)?;
        self.handle_inner_event(event)
            .await
            .with_context(|| display::InvalidBuckEvent(event.dupe()))?;

        if self.verbosity.print_all_commands() {
            what_ran::emit_event_if_relevant(
                event.parent_id().into(),
                event.data(),
                self.spans(),
                &mut PrintDebugCommandToStderr,
                &WhatRanOptions::default(),
            )?;
        }

        Ok(())
    }

    async fn handle_command_start(
        &mut self,
        _command: &buck2_data::CommandStart,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        echo!("Build ID: {}", event.trace_id()?)?;
        self.notify_printed();
        Ok(())
    }

    async fn handle_command_result(
        &mut self,
        result: &cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        if let cli_proto::CommandResult {
            result: Some(cli_proto::command_result::Result::Error(e)),
        } = result
        {
            echo!("Command failed: ")?;
            for message in &e.messages {
                echo!("{}", message)?;
            }
            self.notify_printed();
        }

        let errors = std::mem::take(&mut self.action_errors);

        if !errors.is_empty() {
            echo!()?;
            echo!("BUILD ERRORS ({})", errors.len())?;
            echo!("The following actions failed during the execution of this command:")?;
            for error in errors.iter() {
                error.print(self.tty_mode)?;
            }
            echo!()?;
            self.notify_printed();
        }

        Ok(())
    }

    async fn handle_command_end(
        &mut self,
        _command: &buck2_data::CommandEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if self.verbosity.print_status() && self.action_stats.log_stats() {
            let cache_hit_percentage = self.action_stats.action_cache_hit_percentage();
            echo!("Cache hits: {}%", cache_hit_percentage)?;
            echo!(
                "Commands: {} (cached: {}, remote: {}, local: {})",
                self.action_stats.total_executed_and_cached_actions(),
                self.action_stats.cached_actions,
                self.action_stats.remote_actions,
                self.action_stats.local_actions
            )?;
        }

        if let Some(re) = &self.re_panel.render_header(DrawMode::Final) {
            echo!("{}", re)?;
        }

        if let Some(info) = &self.test_session {
            echo!("Test session: {}", info)?;
        }

        Ok(())
    }

    async fn handle_console_message(
        &mut self,
        message: &buck2_data::ConsoleMessage,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.handle_stderr(&message.message).await
    }

    async fn handle_re_session_created(
        &mut self,
        session: &buck2_data::RemoteExecutionSessionCreated,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.re_panel_mut().add_re_session(session);
        let message = format!("RE Session: {}", session.session_id);
        self.handle_stderr(&message).await
    }

    async fn handle_action_execution_end(
        &mut self,
        action: &buck2_data::ActionExecutionEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.action_stats.update(action);

        let action_id = display::display_action_identity(
            action.key.as_ref(),
            action.name.as_ref(),
            TargetDisplayOptions::for_log(),
        )?;

        if self.verbosity.print_status() {
            let complete = self.span_tracker.roots_completed();
            let incomplete = self.span_tracker.roots_ongoing();
            echo!("{} / {}: {}", complete, complete + incomplete, action_id)?;
            if let Some(stderr) = display::success_stderr(action, self.verbosity)? {
                // TODO(nmj): Factor out behavior here so that handling ttymode isn't ad hoc.  i.e. write a method that formats text based on tty mode
                match self.tty_mode {
                    TtyMode::Enabled => {
                        // Add the extra control character so that users' stderr messages can't
                        // mess up the terminal
                        echo!("stderr:{}\x1b[0m", stderr)?;
                    }
                    TtyMode::Disabled => {
                        echo!(
                            "stderr:\n{}",
                            Self::sanitize_output_colors(stderr.as_bytes())
                        )?;
                    }
                }
            }
        }

        if let Some(error) = &action.error {
            let action_error = ActionError {
                display: display::display_action_error(
                    action,
                    error,
                    TargetDisplayOptions::for_log(),
                )?
                .to_static(),
            };

            action_error.print(self.tty_mode)?;
            self.action_errors.push(action_error);
        }
        self.notify_printed();

        Ok(())
    }

    async fn handle_test_discovery(
        &mut self,
        test_info: &buck2_data::TestDiscovery,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if let Some(data) = &test_info.data {
            match data {
                buck2_data::test_discovery::Data::Session(buck2_data::TestSessionInfo { info }) => {
                    echo!("Test session: {}", info)?;
                    self.test_session = Some(info.clone());
                    self.notify_printed();
                }
                buck2_data::test_discovery::Data::Tests(..) => {}
            }
        }

        Ok(())
    }

    async fn handle_panic(
        &mut self,
        panic: &buck2_data::Panic,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        echo!("{:?}", panic)?;
        self.notify_printed();
        Ok(())
    }

    async fn handle_test_result(
        &mut self,
        result: &buck2_data::TestResult,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if let Some(msg) = display::format_test_result(result)? {
            let mut buffer: Vec<u8> = Vec::new();

            for line in msg {
                line.render(&mut buffer)?;
            }
            //Printing the test output in multiple lines. It makes easier for the user to read.
            echo!("{}", Self::sanitize_output_colors(&buffer))?;
        }

        Ok(())
    }

    async fn tick(&mut self, _: &Tick) -> anyhow::Result<()> {
        if self.verbosity.print_status() && self.last_print_time.elapsed() > KEEPALIVE_TIME_LIMIT {
            let mut roots = self.span_tracker.iter_roots();
            let sample_event = roots.next();
            match sample_event {
                Some(sample_event) => {
                    let child = match sample_event.children().next() {
                        Some(c) => Cow::Owned(format!(
                            " [{}]",
                            display::display_event(
                                &c.info().event,
                                TargetDisplayOptions::for_log()
                            )?
                        )),
                        None => Cow::Borrowed(""),
                    };

                    echo!(
                        "Waiting on {}{}, and {} other actions",
                        display::display_event(
                            &sample_event.info().event,
                            TargetDisplayOptions::for_log()
                        )?,
                        child,
                        roots.len()
                    )?
                }
                None => {
                    if self.show_waiting_message {
                        echo!("Waiting on buck2 daemon...")?
                    }
                }
            }
            // roots must be dropped here because it mutably borrows `self`
            // and doesn't get dropped until the end of this scope otherwise.
            std::mem::drop(roots);

            if self.show_waiting_message {
                self.print_stats_while_waiting()?;
            }

            self.notify_printed();
        }

        Ok(())
    }

    async fn handle_lsp_result(&mut self, msg: &buck2_data::LspResult) -> anyhow::Result<()> {
        let lsp_message: Message = serde_json::from_str(&msg.lsp_json)?;

        let stdout = std::io::stdout();
        let mut stdout = stdout.lock();
        lsp_message.write(&mut stdout)?;
        stdout.flush()?;
        Ok(())
    }

    async fn handle_snapshot(
        &mut self,
        update: &buck2_data::Snapshot,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.re_panel_mut().update(event.timestamp(), update);
        self.io_state.update(event.timestamp(), update);
        self.two_snapshots.update(event.timestamp(), update);
        Ok(())
    }
}

struct PrintDebugCommandToStderr;

impl WhatRanOutputWriter for PrintDebugCommandToStderr {
    fn emit_command(&mut self, command: WhatRanOutputCommand<'_>) -> anyhow::Result<()> {
        echo!(
            "{}",
            WhatRanCommandConsoleFormat {
                reason: command.reason(),
                identity: command.identity(),
                repro: command.repro(),
            }
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::SimpleConsole;

    #[test]
    fn removes_color_characters() {
        let message = "\x1b[0mFoo\t\x1b[34mBar\n\x1b[DBaz\r\nQuz";

        let sanitized = SimpleConsole::sanitize_output_colors(message.as_bytes());

        assert_eq!("Foo\tBar\nBaz\r\nQuz", sanitized);
    }
}
