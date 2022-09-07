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

use gazebo::dupe::Dupe;
use superconsole::Line;
use superconsole::Span;
use superconsole::SuperConsole;

use crate::subscribers::display;
use crate::subscribers::display::TargetDisplayOptions;

/// Options controlling what WhatRan produces.
#[derive(Debug, Default, clap::Parser)]
pub struct WhatRanOptions {
    #[clap(long)]
    pub emit_cache_queries: bool,
    #[clap(long)]
    pub skip_cache_hits: bool,
    #[clap(long)]
    pub skip_remote_executions: bool,
    #[clap(long)]
    pub skip_local_executions: bool,
}

/// An action that makes sense to use to contextualize a command we ran.
pub enum WhatRanRelevantAction<'a> {
    ActionExecution(&'a buck2_data::ActionExecutionStart),
    TestDiscovery(&'a buck2_data::TestDiscoveryStart),
    TestRun(&'a buck2_data::TestRunStart),
}

impl<'a> WhatRanRelevantAction<'a> {
    /// Extract a relevant action from an event's data, if we can find one.
    pub fn from_buck_data(data: &'a buck2_data::buck_event::Data) -> Option<Self> {
        match data {
            buck2_data::buck_event::Data::SpanStart(span) => match &span.data {
                Some(buck2_data::span_start_event::Data::ActionExecution(action)) => {
                    Some(Self::ActionExecution(action))
                }
                Some(buck2_data::span_start_event::Data::TestDiscovery(suite)) => {
                    Some(Self::TestDiscovery(suite))
                }
                Some(buck2_data::span_start_event::Data::TestStart(test)) => {
                    Some(Self::TestRun(test))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

pub struct WhatRanOutputCommand<'a> {
    reason: &'a str,
    identity: &'a str,
    repro: CommandReproducer<'a>,
    extra: Option<WhatRanOutputCommandExtra<'a>>,
}

impl WhatRanOutputCommand<'_> {
    pub fn reason(&self) -> &str {
        self.reason
    }
    pub fn identity(&self) -> &str {
        self.identity
    }
    pub fn repro(&self) -> CommandReproducer<'_> {
        self.repro
    }
    pub fn extra(&self) -> Option<WhatRanOutputCommandExtra<'_>> {
        self.extra
    }
}

#[derive(Clone, Copy, Dupe)]
pub enum WhatRanOutputCommandExtra<'a> {
    TestCases(&'a [String]),
}

/// Output to log commands that ran. The expectation is that we can use this to print out events.
pub trait WhatRanOutputWriter {
    fn emit_command(&mut self, command: WhatRanOutputCommand<'_>) -> anyhow::Result<()>;
}

/// Storage provided for events. The expectations is that any previously event that would qualify
/// as a WhatRanRelevantAction was captured in this and will be returned.
pub trait WhatRanState<T> {
    fn get(&self, span_id: T) -> Option<WhatRanRelevantAction<'_>>;
}

/// Presented with an event and its containing span, emit it to the output if it's relevant. The
/// state is used to associate the parent with something meaningful. This does not take the parent
/// directly because *most* events are *not* relevant so we save the lookup in that case.
pub fn emit_event_if_relevant<T: fmt::Display + Copy>(
    parent_span_id: T,
    data: &buck2_data::buck_event::Data,
    state: &impl WhatRanState<T>,
    output: &mut impl WhatRanOutputWriter,
    options: &WhatRanOptions,
) -> anyhow::Result<()> {
    match data {
        buck2_data::buck_event::Data::SpanStart(span) => match &span.data {
            Some(buck2_data::span_start_event::Data::ExecutorStage(executor_stage)) => {
                match &executor_stage.stage {
                    Some(buck2_data::executor_stage_start::Stage::CacheQuery(cache_hit))
                        if options.emit_cache_queries =>
                    {
                        emit(
                            parent_span_id,
                            CommandReproducer::CacheQuery(cache_hit),
                            state,
                            output,
                        )?;
                    }
                    Some(buck2_data::executor_stage_start::Stage::CacheHit(cache_hit))
                        if !options.skip_cache_hits =>
                    {
                        emit(
                            parent_span_id,
                            CommandReproducer::CacheHit(cache_hit),
                            state,
                            output,
                        )?;
                    }
                    Some(buck2_data::executor_stage_start::Stage::Re(re_stage))
                        if !options.skip_remote_executions =>
                    {
                        match &re_stage.stage {
                            Some(buck2_data::re_stage::Stage::Execute(execute)) => {
                                emit(
                                    parent_span_id,
                                    CommandReproducer::ReExecute(execute),
                                    state,
                                    output,
                                )?;
                            }
                            _ => {}
                        }
                    }
                    Some(buck2_data::executor_stage_start::Stage::Local(local_stage)) => {
                        if !options.skip_local_executions {
                            match &local_stage.stage {
                                Some(buck2_data::local_stage::Stage::Execute(local_execute)) => {
                                    emit(
                                        parent_span_id,
                                        CommandReproducer::LocalExecute(local_execute),
                                        state,
                                        output,
                                    )?;
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        },
        _ => {}
    };

    Ok(())
}

/// Find and format the parent span (if any), then emit the relevant command.
fn emit<T: fmt::Display + Copy>(
    parent_span_id: T,
    repro: CommandReproducer<'_>,
    state: &impl WhatRanState<T>,
    output: &mut impl WhatRanOutputWriter,
) -> anyhow::Result<()> {
    let action = state.get(parent_span_id);

    let (reason, identity, extra) = match action {
        Some(WhatRanRelevantAction::ActionExecution(action)) => (
            "build",
            Cow::Owned(display::display_action_identity(
                action.key.as_ref(),
                action.name.as_ref(),
                TargetDisplayOptions::for_log(),
            )?),
            None,
        ),
        Some(WhatRanRelevantAction::TestDiscovery(test)) => {
            ("test.discovery", Cow::Borrowed(&test.suite_name), None)
        }
        Some(WhatRanRelevantAction::TestRun(test)) => match test.suite.as_ref() {
            Some(suite) => (
                "test.run",
                Cow::Borrowed(&suite.suite_name),
                Some(WhatRanOutputCommandExtra::TestCases(&suite.test_names)),
            ),
            None => (
                "test.run",
                Cow::Owned(format!("unknown test suite (span id: {})", parent_span_id)),
                None,
            ),
        },
        None => (
            "unknown",
            Cow::Owned(format!("unknown action (span id: {})", parent_span_id)),
            None,
        ),
    };

    output.emit_command(WhatRanOutputCommand {
        reason,
        identity: &identity,
        repro,
        extra,
    })?;

    Ok(())
}

/// The reproduction details for this command.
#[derive(Clone, Copy, Dupe)]
pub enum CommandReproducer<'a> {
    CacheQuery(&'a buck2_data::CacheQuery),
    CacheHit(&'a buck2_data::CacheHit),
    ReExecute(&'a buck2_data::ReExecute),
    LocalExecute(&'a buck2_data::LocalExecute),
}

impl<'a> CommandReproducer<'a> {
    pub fn executor(&self) -> &'static str {
        match self {
            Self::CacheQuery(..) => "cache_query",
            Self::CacheHit(..) => "cache",
            Self::ReExecute(..) => "re",
            Self::LocalExecute(..) => "local",
        }
    }

    /// Human-readable representation of this repro instruction
    pub fn as_human_readable(&self) -> HumanReadableCommandReproducer<'a> {
        HumanReadableCommandReproducer { command: *self }
    }
}

/// A wrapper type to output CommandReproducer as a human readable string.
pub struct HumanReadableCommandReproducer<'a> {
    command: CommandReproducer<'a>,
}

impl<'a> fmt::Display for HumanReadableCommandReproducer<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match &self.command {
            CommandReproducer::CacheQuery(re_action_cache) => {
                write!(formatter, "{}", re_action_cache.action_digest)
            }
            CommandReproducer::CacheHit(re_action_cache) => {
                write!(formatter, "{}", re_action_cache.action_digest)
            }
            CommandReproducer::ReExecute(re_action_cache) => {
                write!(formatter, "{}", re_action_cache.action_digest)
            }
            CommandReproducer::LocalExecute(local_execute) => {
                if let Some(command) = &local_execute.command {
                    write!(formatter, "{}", local_command_to_string(command))
                } else {
                    Ok(())
                }
            }
        }
    }
}

pub fn local_command_to_string(command: &buck2_data::LocalCommand) -> String {
    let mut cmd = vec![];

    if !command.env.is_empty() {
        cmd.push(Cow::Borrowed("env"));
        cmd.push(Cow::Borrowed("--"));
        for entry in command.env.iter() {
            cmd.push(Cow::Owned(format!("{}={}", entry.key, entry.value)))
        }
    }

    for arg in command.argv.iter() {
        cmd.push(Cow::Borrowed(arg));
    }

    shlex::join(cmd.iter().map(|e| e.as_ref()))
}

impl WhatRanOutputWriter for SuperConsole {
    fn emit_command(&mut self, command: WhatRanOutputCommand<'_>) -> anyhow::Result<()> {
        let msg = WhatRanCommandConsoleFormat {
            reason: command.reason(),
            identity: command.identity(),
            repro: command.repro(),
        }
        .to_string();
        self.emit(vec![Line::from_iter([Span::sanitized(msg)])]);
        Ok(())
    }
}

/// A consistent format for printing that we are about to run an action.
pub struct WhatRanCommandConsoleFormat<'a, 'b> {
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
