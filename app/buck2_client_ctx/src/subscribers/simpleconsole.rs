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
use std::fmt::Write as _;
use std::process::Stdio;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_data::CommandExecutionDetails;
use buck2_event_observer::display;
use buck2_event_observer::display::display_file_watcher_end;
use buck2_event_observer::display::TargetDisplayOptions;
use buck2_event_observer::event_observer::EventObserver;
use buck2_event_observer::event_observer::EventObserverExtra;
use buck2_event_observer::humanized::HumanizedBytes;
use buck2_event_observer::io_state::io_in_flight_non_zero_counters;
use buck2_event_observer::verbosity::Verbosity;
use buck2_event_observer::what_ran;
use buck2_event_observer::what_ran::local_command_to_string;
use buck2_event_observer::what_ran::WhatRanCommandConsoleFormat;
use buck2_event_observer::what_ran::WhatRanOptions;
use buck2_event_observer::what_ran::WhatRanOutputCommand;
use buck2_event_observer::what_ran::WhatRanOutputWriter;
use buck2_events::BuckEvent;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use superconsole::DrawMode;
use superconsole::SuperConsole;
use termwiz::escape::Action;
use termwiz::escape::ControlCode;

use crate::subscribers::subscriber::Tick;
use crate::subscribers::subscriber_unpack::UnpackingEventSubscriber;

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
                "Remote action{}, reproduce with: `frecli cas download-action {}`",
                if remote_command.cache_hit {
                    " cache hit"
                } else {
                    ""
                },
                remote_command.action_digest
            )?;
        }
        Some(Command::OmittedLocalCommand(..)) | None => {
            // Nothing to show in this case.
        }
    };
    let (stdout, stderr) = if tty_mode == TtyMode::Disabled {
        (
            sanitize_output_colors(command_failed.stdout.as_bytes()),
            sanitize_output_colors(command_failed.stderr.as_bytes()),
        )
    } else {
        (command_failed.stdout.clone(), command_failed.stderr.clone())
    };
    echo!("Stdout: {}", stdout)?;
    echo!("Stderr: {}", stderr)?;
    Ok(())
}

/// Just repeats stdout and stderr to client process.
pub(crate) struct SimpleConsole<E> {
    tty_mode: TtyMode,
    verbosity: Verbosity,
    // Whether to show "Waiting for daemon..." when no root spans are received
    show_waiting_message: bool,
    pub(crate) observer: EventObserver<E>,
    action_errors: Vec<ActionError>,
    last_print_time: Instant,
    last_had_open_spans: Instant, // Used to detect hangs
    already_raged: bool,
    isolation_dir: FileNameBuf,
    last_shown_snapshot_ts: Option<SystemTime>,
}

impl<E> SimpleConsole<E>
where
    E: EventObserverExtra,
{
    pub(crate) fn with_tty(
        trace_id: TraceId,
        isolation_dir: FileNameBuf,
        verbosity: Verbosity,
        show_waiting_message: bool,
    ) -> Self {
        SimpleConsole {
            tty_mode: TtyMode::Enabled,
            verbosity,
            show_waiting_message,
            observer: EventObserver::new(trace_id),
            action_errors: Vec::new(),
            last_print_time: Instant::now(),
            last_had_open_spans: Instant::now(),
            already_raged: false,
            isolation_dir,
            last_shown_snapshot_ts: None,
        }
    }

    pub(crate) fn without_tty(
        trace_id: TraceId,
        isolation_dir: FileNameBuf,
        verbosity: Verbosity,
        show_waiting_message: bool,
    ) -> Self {
        SimpleConsole {
            tty_mode: TtyMode::Disabled,
            verbosity,
            show_waiting_message,
            observer: EventObserver::new(trace_id),
            action_errors: Vec::new(),
            last_print_time: Instant::now(),
            last_had_open_spans: Instant::now(),
            already_raged: false,
            isolation_dir,
            last_shown_snapshot_ts: None,
        }
    }

    /// Create a SimpleConsole that auto detects whether it has a TTY or not.
    pub(crate) fn autodetect(
        trace_id: TraceId,
        isolation_dir: FileNameBuf,
        verbosity: Verbosity,
        show_waiting_message: bool,
    ) -> Self {
        match SuperConsole::compatible() {
            true => Self::with_tty(trace_id, isolation_dir, verbosity, show_waiting_message),
            false => Self::without_tty(trace_id, isolation_dir, verbosity, show_waiting_message),
        }
    }

    pub(crate) fn observer(&self) -> &EventObserver<E> {
        &self.observer
    }

    pub(crate) fn update_event_observer(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.observer
            .observe(Instant::now(), event)
            .context("Error tracking event")
    }

    fn notify_printed(&mut self) {
        self.last_print_time = Instant::now();
    }

    fn print_stats_while_waiting(&mut self) -> anyhow::Result<()> {
        if let Some(h) = self.observer().re_state().render_header(DrawMode::Normal) {
            echo!("{}", h)?;
        }

        let snapshots = self.observer().two_snapshots();
        let last_snapshot_ts = snapshots.last.as_ref().map(|(ts, _)| *ts);

        // We normally send snapshots more often than we print this, so we'd expect the
        // snapshot to change on every call.
        let is_snapshot_stale = match (&self.last_shown_snapshot_ts, last_snapshot_ts) {
            (Some(x), Some(y)) => *x == y,
            _ => false,
        };

        if is_snapshot_stale {
            echo!("Resource usage: <snapshot is stale>")?;
        } else {
            let mut parts = Vec::with_capacity(2);
            if let Some((_, snapshot)) = &snapshots.last {
                if let Some(buck2_rss) = snapshot.buck2_rss {
                    parts.push(format!("RSS: {}", HumanizedBytes::new(buck2_rss)));
                }
            }
            if let Some(cpu) = snapshots.cpu_percents() {
                parts.push(format!("CPU: {}%", cpu));
            }
            if !parts.is_empty() {
                echo!("Resource usage: {}", parts.join(" "))?;
            }

            if let Some((_ts, snapshot)) = &snapshots.last {
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

            self.last_shown_snapshot_ts = last_snapshot_ts;
        }

        Ok(())
    }

    pub(crate) async fn detect_hangs(&mut self) -> anyhow::Result<()> {
        if !self.show_waiting_message {
            // No open spans are to be expected in this case (D37658796)
            return Ok(());
        };
        if self.observer().spans().iter_roots().len() > 0 {
            self.last_had_open_spans = Instant::now();
            return Ok(());
        }
        // Do nothing if less than 1 minute since last open span
        if self.last_had_open_spans.elapsed().as_secs() < 60 {
            return Ok(());
        }
        // When command is stuck we call `rage` to gather debugging information
        if !self.already_raged {
            self.already_raged = true;
            spawn_rage(
                self.isolation_dir.clone(),
                self.observer().session_info().trace_id.dupe(),
            );
        }
        Ok(())
    }
}

#[async_trait]
impl<E> UnpackingEventSubscriber for SimpleConsole<E>
where
    E: EventObserverExtra,
{
    async fn handle_output(&mut self, raw_output: &[u8]) -> anyhow::Result<()> {
        // We expect output that gets here to already have been buffered if possible (because it
        // primarily gets to us through a GRPC layer that already needs buffering), so we
        // unconditionally flush it.
        crate::stdio::print_bytes(raw_output)?;
        crate::stdio::flush()?;
        self.notify_printed();
        Ok(())
    }

    async fn handle_stderr(&mut self, stderr: &str) -> anyhow::Result<()> {
        echo!("{}", stderr)?;
        self.notify_printed();
        Ok(())
    }

    async fn handle_structured_error(
        &mut self,
        err: &buck2_data::StructuredError,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if err.quiet {
            return Ok(());
        }
        echo!("{}", err.payload)?;
        self.notify_printed();
        Ok(())
    }

    async fn handle_file_watcher_end(
        &mut self,
        file_watcher: &buck2_data::FileWatcherEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        for x in display_file_watcher_end(file_watcher) {
            echo!("{}", x)?;
        }
        self.notify_printed();
        Ok(())
    }

    async fn handle_event(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.update_event_observer(event)?;
        self.handle_inner_event(event)
            .await
            .with_context(|| display::InvalidBuckEvent(event.dupe()))?;

        if self.verbosity.print_all_commands() {
            what_ran::emit_event_if_relevant(
                event.parent_id().into(),
                event.data(),
                self.observer().spans(),
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
        if cfg!(fbcode_build) {
            echo!(
                "Buck UI: https://www.internalfb.com/buck2/{}",
                event.trace_id()?
            )?;
        } else {
            echo!("Build ID: {}", event.trace_id()?)?;
        }
        self.notify_printed();
        Ok(())
    }

    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        if let buck2_cli_proto::CommandResult {
            result: Some(buck2_cli_proto::command_result::Result::Error(e)),
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
        if self.verbosity.print_status() && self.observer().action_stats().log_stats() {
            let cache_hit_percentage = self.observer().action_stats().action_cache_hit_percentage();
            echo!("Cache hits: {}%", cache_hit_percentage)?;
            echo!(
                "Commands: {} (cached: {}, remote: {}, local: {})",
                self.observer()
                    .action_stats()
                    .total_executed_and_cached_actions(),
                self.observer().action_stats().cached_actions,
                self.observer().action_stats().remote_actions,
                self.observer().action_stats().local_actions
            )?;
            if self.observer().action_stats().fallback_actions > 0 {
                echo!(
                    "Fallback: {}/{}",
                    self.observer().action_stats().fallback_actions,
                    self.observer().action_stats().total_executed_actions()
                )?;
            }
        }

        if let Some(re) = &self.observer().re_state().render_header(DrawMode::Final) {
            echo!("{}", re)?;
        }

        if let Some(test_session) = &self.observer().session_info().test_session {
            echo!("Test session: {}", test_session.info)?;
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
        let message = format!("RE Session: {}", session.session_id);
        self.handle_stderr(&message).await
    }

    async fn handle_action_execution_end(
        &mut self,
        action: &buck2_data::ActionExecutionEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        let action_id = display::display_action_identity(
            action.key.as_ref(),
            action.name.as_ref(),
            TargetDisplayOptions::for_log(),
        )?;

        let stderr = display::success_stderr(action, self.verbosity)?;

        if self.verbosity.print_all_actions() || stderr.is_some() {
            let complete = self.observer().spans().roots_completed();
            let incomplete = self.observer().spans().roots_ongoing();
            echo!("{} / {}: {}", complete, complete + incomplete, action_id)?;
            if let Some(stderr) = stderr {
                // TODO(nmj): Factor out behavior here so that handling ttymode isn't ad hoc.  i.e. write a method that formats text based on tty mode
                match self.tty_mode {
                    TtyMode::Enabled => {
                        // Add the extra control character so that users' stderr messages can't
                        // mess up the terminal
                        echo!("stderr:{}\x1b[0m", stderr)?;
                    }
                    TtyMode::Disabled => {
                        echo!("stderr:\n{}", sanitize_output_colors(stderr.as_bytes()))?;
                    }
                }
            }
            self.notify_printed();
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
            self.notify_printed();
        }

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
                    self.notify_printed();
                }
                buck2_data::test_discovery::Data::Tests(..) => {}
            }
        }

        Ok(())
    }

    async fn handle_test_result(
        &mut self,
        result: &buck2_data::TestResult,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if let Some(msg) = display::format_test_result(result)? {
            let mut buffer = String::new();

            for line in msg {
                writeln!(buffer, "{}", line.to_unstyled())?;
            }
            //Printing the test output in multiple lines. It makes easier for the user to read.
            echo!("{}", buffer)?;
        }

        Ok(())
    }

    async fn tick(&mut self, _: &Tick) -> anyhow::Result<()> {
        self.detect_hangs().await?;
        if self.verbosity.print_status() && self.last_print_time.elapsed() > KEEPALIVE_TIME_LIMIT {
            let mut show_stats = self.show_waiting_message;

            let mut roots = self.observer().spans().iter_roots();
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
                    )?;

                    show_stats = self.verbosity.always_print_stats_in_status();
                }
                None => {
                    if self.show_waiting_message {
                        echo!("Waiting on buck2 daemon...")?;
                    }
                }
            }
            // roots must be dropped here because it mutably borrows `self`
            // and doesn't get dropped until the end of this scope otherwise.
            std::mem::drop(roots);

            if show_stats {
                self.print_stats_while_waiting()?;
            }

            self.notify_printed();
        }

        Ok(())
    }

    async fn handle_error(&mut self, _error: &anyhow::Error) -> anyhow::Result<()> {
        // We don't need to do any cleanup to exit.
        Ok(())
    }

    async fn handle_console_preferences(
        &mut self,
        _prefs: &buck2_data::ConsolePreferences,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        // Those are only used by the Superconsole at the moment.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn removes_color_characters() {
        let message = "\x1b[0mFoo\t\x1b[34mBar\n\x1b[DBaz\r\nQuz";

        let sanitized = sanitize_output_colors(message.as_bytes());

        assert_eq!("Foo\tBar\nBaz\r\nQuz", sanitized);
    }
}

fn spawn_rage(isolation_dir: FileNameBuf, trace_id: TraceId) {
    match spawn_rage_impl(isolation_dir, trace_id) {
        Ok(_) => {}
        Err(e) => tracing::warn!("Error calling buck2 rage: {:#}", e),
    };
}

fn spawn_rage_impl(isolation_dir: FileNameBuf, trace_id: TraceId) -> anyhow::Result<()> {
    let current_exe = std::env::current_exe().context("Not current_exe")?;
    let mut command = buck2_util::process::async_background_command(current_exe);
    command
        .args(["--isolation-dir", isolation_dir.as_str()])
        .arg("rage")
        .arg("--timeout")
        .arg("3600")
        .arg("--no-paste")
        .args(["--origin", "hang-detector"])
        .args(["--invocation-id", &trace_id.to_string()]);
    let mut spawned = command
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()?;

    // Running rage command in an async task. We don't want to block
    // the main thread while we wait for rage command to finish.
    tokio::spawn(async move { spawned.wait().await });
    Ok(())
}
