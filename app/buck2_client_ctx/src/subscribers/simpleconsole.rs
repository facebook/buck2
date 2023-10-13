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
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use std::time::SystemTime;

use anyhow::Context;
use async_trait::async_trait;
use buck2_data::TagEvent;
use buck2_event_observer::display;
use buck2_event_observer::display::display_file_watcher_end;
use buck2_event_observer::display::TargetDisplayOptions;
use buck2_event_observer::event_observer::EventObserver;
use buck2_event_observer::event_observer::EventObserverExtra;
use buck2_event_observer::humanized::HumanizedBytes;
use buck2_event_observer::verbosity::Verbosity;
use buck2_event_observer::what_ran;
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
use crate::subscribers::superconsole::io::io_in_flight_non_zero_counters;

/// buck2 daemon info is printed to stderr if there are no other updates available
/// within this duration.
const KEEPALIVE_TIME_LIMIT: Duration = Duration::from_secs(7);

fn now_display() -> impl Display {
    chrono::Local::now().to_rfc3339_opts(::chrono::SecondsFormat::Millis, false)
}

fn with_timestamps(message: &str) -> anyhow::Result<String> {
    let mut s = String::new();
    let now = now_display();
    for line in message.lines() {
        if line.is_empty() {
            writeln!(s, "[{}]", now)?;
        } else {
            writeln!(s, "[{}] {}", now, line)?;
        }
    }
    // Remove the trailing newline
    s.pop();
    Ok(s)
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
            let message = with_timestamps(&message)?;
            // patternlint-disable-next-line buck2-cli-simpleconsole-echo
            crate::eprintln!("{}", message)
        }
    };
}

#[derive(Copy, Clone, Dupe, Debug, PartialEq)]
enum TtyMode {
    Enabled,
    Disabled,
}

/// Just repeats stdout and stderr to client process.
pub(crate) struct SimpleConsole<E> {
    tty_mode: TtyMode,
    verbosity: Verbosity,
    // Whether to show "Waiting for daemon..." when no root spans are received
    expect_spans: bool,
    pub(crate) observer: EventObserver<E>,
    action_errors: Vec<buck2_data::ActionError>,
    last_print_time: Instant,
    last_shown_snapshot_ts: Option<SystemTime>,
}

impl<E> SimpleConsole<E>
where
    E: EventObserverExtra,
{
    pub(crate) fn with_tty(trace_id: TraceId, verbosity: Verbosity, expect_spans: bool) -> Self {
        SimpleConsole {
            tty_mode: TtyMode::Enabled,
            verbosity,
            expect_spans,
            observer: EventObserver::new(trace_id),
            action_errors: Vec::new(),
            last_print_time: Instant::now(),
            last_shown_snapshot_ts: None,
        }
    }

    pub(crate) fn without_tty(trace_id: TraceId, verbosity: Verbosity, expect_spans: bool) -> Self {
        SimpleConsole {
            tty_mode: TtyMode::Disabled,
            verbosity,
            expect_spans,
            observer: EventObserver::new(trace_id),
            action_errors: Vec::new(),
            last_print_time: Instant::now(),
            last_shown_snapshot_ts: None,
        }
    }

    /// Create a SimpleConsole that auto detects whether it has a TTY or not.
    pub(crate) fn autodetect(trace_id: TraceId, verbosity: Verbosity, expect_spans: bool) -> Self {
        match SuperConsole::compatible() {
            true => Self::with_tty(trace_id, verbosity, expect_spans),
            false => Self::without_tty(trace_id, verbosity, expect_spans),
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
        let snapshots = self.observer().two_snapshots();

        if let Some(h) = self
            .observer()
            .re_state()
            .render_header(snapshots, DrawMode::Normal)
        {
            echo!("{}", h)?;
        }

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

    fn print_action_error(&mut self, error: &buck2_data::ActionError) -> anyhow::Result<()> {
        let display =
            display::display_action_error(error, TargetDisplayOptions::for_log())?.to_static();
        let message = display.simple_format(Some(with_timestamps))?;
        if self.tty_mode == TtyMode::Disabled {
            // patternlint-disable-next-line buck2-cli-simpleconsole-echo
            crate::eprintln!("{}", sanitize_output_colors(message.as_bytes()))?;
        } else {
            // patternlint-disable-next-line buck2-cli-simpleconsole-echo
            crate::eprintln!("{}", message)?;
        }
        self.notify_printed();
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
        if self.verbosity.print_status() {
            for x in display_file_watcher_end(file_watcher) {
                echo!("{}", x)?;
            }
            self.notify_printed();
        }
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
                self.print_action_error(error)?;
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
        let snapshots = self.observer().two_snapshots();

        if self.verbosity.print_status() && self.observer().action_stats().log_stats() {
            let cache_hit_percentage = self.observer().action_stats().total_cache_hit_percentage();
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

        if let Some(re) = &self
            .observer()
            .re_state()
            .render_header(snapshots, DrawMode::Final)
        {
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

        Ok(())
    }

    async fn handle_action_error(&mut self, error: &buck2_data::ActionError) -> anyhow::Result<()> {
        self.print_action_error(error)?;
        self.action_errors.push(error.clone());
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

    async fn handle_tags(&mut self, tags: &TagEvent) -> anyhow::Result<()> {
        if tags.tags.contains(&"which-dice:Modern".to_owned()) {
            self.handle_stderr("Note: using experimental modern dice.")
                .await?;
        }

        Ok(())
    }

    async fn tick(&mut self, _: &Tick) -> anyhow::Result<()> {
        if self.verbosity.print_status() && self.last_print_time.elapsed() > KEEPALIVE_TIME_LIMIT {
            let mut show_stats = self.expect_spans;

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

                    let remaining = match roots.len() {
                        0 => String::new(),
                        x => format!(", and {x} other actions"),
                    };
                    echo!(
                        "Waiting on {}{}{}",
                        display::display_event(
                            &sample_event.info().event,
                            TargetDisplayOptions::for_log()
                        )?,
                        child,
                        remaining
                    )?;

                    show_stats = self.verbosity.always_print_stats_in_status();
                }
                None => {
                    if self.expect_spans {
                        echo!(
                            "Waiting on buck2 daemon {}...",
                            self.observer.session_info().trace_id
                        )?;
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
