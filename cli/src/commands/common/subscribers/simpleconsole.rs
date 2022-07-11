/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt;
use std::io::Write;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use async_trait::async_trait;
use buck2_data::CommandExecutionDetails;
use events::subscriber::EventSubscriber;
use events::subscriber::Tick;
use events::BuckEvent;
use gazebo::prelude::*;
use lsp_server::Message;
use superconsole::SuperConsole;
use termwiz::escape::Action;
use termwiz::escape::ControlCode;

use crate::commands::common::subscribers::display;
use crate::commands::common::subscribers::display::TargetDisplayOptions;
use crate::commands::common::subscribers::re::ReState;
use crate::commands::common::subscribers::span_tracker::SpanTracker;
use crate::commands::common::verbosity::Verbosity;
use crate::commands::common::what_ran;
use crate::commands::common::what_ran::local_command_to_string;
use crate::commands::common::what_ran::CommandReproducer;
use crate::commands::common::what_ran::WhatRanOptions;
use crate::commands::common::what_ran::WhatRanOutputCommand;
use crate::commands::common::what_ran::WhatRanOutputWriter;

const KEEPALIVE_TIME_LIMIT: Duration = Duration::from_secs(7);

// Echoes a message to stderr, along with a timestamp.
macro_rules! echo {
    ($($tts:tt)*) => {
        {
            // patternlint-disable-next-line buck2-cli-simpleconsole-echo
            crate::eprint!("[{}] ", ::chrono::Local::now().to_rfc3339_opts(::chrono::SecondsFormat::Millis, false))?;
            // patternlint-disable-next-line buck2-cli-simpleconsole-echo
            crate::eprintln!($($tts)*)
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
    pub local_actions: u64,
    pub remote_actions: u64,
    pub cached_actions: u64,
}

impl ActionStats {
    #[cfg(test)]
    pub(crate) fn new_with_local_remote_cached(
        local_actions: u64,
        remote_actions: u64,
        cached_actions: u64,
    ) -> Self {
        Self {
            local_actions,
            remote_actions,
            cached_actions,
        }
    }

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
        // NOTE: This currently shows the commands that actually produced a result (or an error!),
        // but not commands that fell back and were retried.
        use buck2_data::command_execution_details::Command;

        let last_command = action
            .commands
            .last()
            .and_then(|c| c.details.as_ref())
            .and_then(|c| c.command.as_ref());

        match last_command {
            Some(Command::LocalCommand(..)) | Some(Command::OmittedLocalCommand(..)) => {
                self.local_actions += 1;
            }
            Some(Command::RemoteCommand(buck2_data::RemoteCommand {
                cache_hit: true, ..
            })) => {
                self.cached_actions += 1;
            }
            Some(Command::RemoteCommand(buck2_data::RemoteCommand {
                cache_hit: false, ..
            })) => {
                self.remote_actions += 1;
            }
            None => {
                // no command
            }
        };
    }

    pub(crate) fn log_stats(&self) -> bool {
        self.total_executed_and_cached_actions() > 0
    }
}

/// Just repeats stdout and stderr to client process.
pub(crate) struct SimpleConsole {
    tty_mode: TtyMode,
    verbosity: Verbosity,
    span_tracker: SpanTracker,
    action_errors: Vec<ActionError>,
    last_print_time: Instant,
    test_session: Option<String>,
    action_stats: ActionStats,
    re_state: ReState,
}

impl SimpleConsole {
    pub(crate) fn with_tty(verbosity: Verbosity) -> Self {
        SimpleConsole {
            tty_mode: TtyMode::Enabled,
            verbosity,
            span_tracker: SpanTracker::new(),
            action_errors: Vec::new(),
            last_print_time: Instant::now(),
            test_session: None,
            action_stats: ActionStats::default(),
            re_state: ReState::new(),
        }
    }

    pub(crate) fn without_tty(verbosity: Verbosity) -> Self {
        SimpleConsole {
            tty_mode: TtyMode::Disabled,
            verbosity,
            span_tracker: SpanTracker::new(),
            action_errors: Vec::new(),
            last_print_time: Instant::now(),
            test_session: None,
            action_stats: ActionStats::default(),
            re_state: ReState::new(),
        }
    }

    /// Create a SimpleConsole that auto detects whether it has a TTY or not.
    pub(crate) fn autodetect(verbosity: Verbosity) -> Self {
        match SuperConsole::compatible() {
            true => Self::with_tty(verbosity),
            false => Self::without_tty(verbosity),
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

    pub(crate) fn re_state(&self) -> &ReState {
        &self.re_state
    }

    pub(crate) fn re_state_mut(&mut self) -> &mut ReState {
        &mut self.re_state
    }

    pub(crate) async fn update_span_tracker(&mut self, event: &BuckEvent) -> anyhow::Result<()> {
        self.span_tracker
            .handle_event(event)
            .await
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
}

#[async_trait]
impl EventSubscriber for SimpleConsole {
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

    async fn handle_event(&mut self, event: &BuckEvent) -> anyhow::Result<()> {
        self.update_span_tracker(event).await?;
        self.handle_inner_event(event)
            .await
            .with_context(|| display::InvalidBuckEvent(event.clone()))?;

        if self.verbosity.print_all_commands() {
            what_ran::emit_event_if_relevant(
                event.parent_id.into(),
                &event.data,
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
        echo!("Build ID: {}", event.trace_id)?;
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

        if let Some(re) = &self.re_state.render() {
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
        self.re_state_mut().add_re_session(session);
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
        let msg = display::format_test_result(result)?;
        let mut buffer: Vec<u8> = Vec::new();

        for line in msg {
            line.render(&mut buffer)?;
        }
        //Printing the test output in multiple lines. It makes easier for the user to read.
        echo!("{}", Self::sanitize_output_colors(&buffer))?;

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
                None => echo!("Waiting on daemon...")?,
            }
            // roots must be dropped here because it mutably borrows `self`
            // and doesn't get dropped until the end of this scope otherwise.
            std::mem::drop(roots);

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
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.re_state_mut().update(update);
        Ok(())
    }
}

/// A consistent format for printing that we are about to run an action.
pub(crate) struct WhatRanCommandConsoleFormat<'a, 'b> {
    pub reason: &'a str,
    pub identity: &'a str,
    pub repro: CommandReproducer<'b>,
}

impl<'a, 'b> fmt::Display for WhatRanCommandConsoleFormat<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Running action: {} ({}), {} executor: {}",
            self.identity,
            self.reason,
            self.repro.executor(),
            self.repro.as_human_readable()
        )
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
