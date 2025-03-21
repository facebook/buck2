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
use std::fmt::Display;
use std::fmt::Formatter;

use buck2_data::re_platform::Property;
use buck2_data::ActionName;
use buck2_events::span::SpanId;
use dupe::Dupe;
use regex::Regex;
use superconsole::Line;
use superconsole::Lines;
use superconsole::SuperConsole;

use crate::display;
use crate::display::TargetDisplayOptions;

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
    #[clap(long)]
    /// Regular expression to filter commands by given action category (i.e. type of of actions that are
    /// similar but operate on different inputs, such as invocations of a C++
    /// compiler (whose category would be `cxx_compile`)). Matches by full string.
    pub filter_category: Option<String>,
}

pub struct WhatRanOptionsRegex<'a> {
    pub options: &'a WhatRanOptions,
    filter_category_regex: Option<Regex>,
}
impl<'a> WhatRanOptionsRegex<'a> {
    pub fn from_options(options: &'a WhatRanOptions) -> buck2_error::Result<Self> {
        let filter_category_regex = match &options.filter_category {
            Some(filter_category) => Some(Regex::new(&format!(r"^{}$", filter_category))?),
            None => None,
        };
        Ok(Self {
            options,
            filter_category_regex,
        })
    }
}

/// An action that makes sense to use to contextualize a command we ran.
#[derive(Clone)]
pub enum WhatRanRelevantAction {
    ActionExecution(buck2_data::ActionExecutionStart),
    TestDiscovery(buck2_data::TestDiscoveryStart),
    TestRun(buck2_data::TestRunStart),
    SetupLocalResources(buck2_data::SetupLocalResourcesStart),
}

impl WhatRanRelevantAction {
    /// Extract a relevant action from an event's data, if we can find one.
    pub fn from_buck_data(data: &buck2_data::buck_event::Data) -> Option<Self> {
        match data {
            buck2_data::buck_event::Data::SpanStart(span) => match &span.data {
                Some(buck2_data::span_start_event::Data::ActionExecution(action)) => {
                    Some(Self::ActionExecution(action.clone()))
                }
                Some(buck2_data::span_start_event::Data::TestDiscovery(suite)) => {
                    Some(Self::TestDiscovery(suite.clone()))
                }
                Some(buck2_data::span_start_event::Data::TestStart(test)) => {
                    Some(Self::TestRun(test.clone()))
                }
                Some(buck2_data::span_start_event::Data::LocalResources(setup)) => {
                    Some(Self::SetupLocalResources(setup.clone()))
                }
                _ => None,
            },
            _ => None,
        }
    }
}

pub struct WhatRanOutputCommand<'a> {
    pub reason: &'a str,
    pub identity: &'a str,
    pub repro: CommandReproducer,
    pub extra: Option<WhatRanOutputCommandExtra<'a>>,
    pub std_err: Option<&'a str>,
    pub duration: Option<std::time::Duration>,
}

impl<'a> WhatRanOutputCommand<'a> {
    pub fn as_tabulated_reproducer(&self) -> impl fmt::Display + '_ {
        WhatRanOutputCommandHeader { cmd: self }
    }
}

struct WhatRanOutputCommandHeader<'r, 'a> {
    cmd: &'r WhatRanOutputCommand<'a>,
}

impl Display for WhatRanOutputCommandHeader<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\t{}\t{}\t{}",
            self.cmd.reason,
            self.cmd.identity,
            self.cmd.repro.executor(),
            self.cmd.repro,
        )
    }
}
#[derive(Clone, Copy, Dupe)]
pub enum WhatRanOutputCommandExtra<'a> {
    TestCases(&'a [String]),
}

/// Output to log commands that ran. The expectation is that we can use this to print out events.
pub trait WhatRanOutputWriter {
    fn emit_command(&mut self, command: WhatRanOutputCommand<'_>) -> buck2_error::Result<()>;
}

/// Storage provided for events. The expectations is that any previously event that would qualify
/// as a WhatRanRelevantAction was captured in this and will be returned.
pub trait WhatRanState {
    fn get(&self, span_id: SpanId) -> Option<WhatRanRelevantAction>;
}

pub fn matches_category(action: Option<&WhatRanRelevantAction>, pattern: &Regex) -> bool {
    match action {
        Some(WhatRanRelevantAction::ActionExecution(action)) => match action.name.as_ref() {
            Some(ActionName { category, .. }) => pattern.is_match(category),
            _ => false,
        },
        _ => false,
    }
}

pub fn emit_what_ran_entry(
    action: Option<&WhatRanRelevantAction>,
    repro: CommandReproducer,
    output: &mut impl WhatRanOutputWriter,
    options: &WhatRanOptionsRegex,
    std_err: Option<&str>,
    duration: Option<std::time::Duration>,
) -> buck2_error::Result<()> {
    let should_emit = options
        .filter_category_regex
        .as_ref()
        .map_or(true, |category| matches_category(action, category));

    if !should_emit {
        return Ok(());
    }
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
        Some(WhatRanRelevantAction::TestDiscovery(test)) => (
            "test.discovery",
            Cow::Borrowed(test.suite_name.as_str()),
            None,
        ),
        Some(WhatRanRelevantAction::TestRun(test)) => match test.suite.as_ref() {
            Some(suite) => (
                "test.run",
                Cow::Borrowed(suite.suite_name.as_str()),
                Some(WhatRanOutputCommandExtra::TestCases(&suite.test_names)),
            ),
            None => ("test.run", Cow::Borrowed("unknown test suite"), None),
        },
        Some(WhatRanRelevantAction::SetupLocalResources(setup)) => (
            "test.local_resource_setup",
            if let Some(target_label) = &setup.target_label {
                Cow::Owned(display::display_configured_target_label(
                    target_label,
                    TargetDisplayOptions::for_log(),
                )?)
            } else {
                Cow::Borrowed("")
            },
            None,
        ),
        None => ("unknown", Cow::Borrowed("unknown action"), None),
    };

    output.emit_command(WhatRanOutputCommand {
        reason,
        identity: &identity,
        repro,
        extra,
        std_err,
        duration,
    })?;

    Ok(())
}

/// The reproduction details for this command.
#[derive(Clone)]
pub enum CommandReproducer {
    CacheQuery(buck2_data::CacheQuery),
    CacheHit(buck2_data::CacheHit),
    ReExecute(buck2_data::ReExecute),
    LocalExecute(buck2_data::LocalExecute),
    WorkerExecute(buck2_data::WorkerExecute),
    WorkerInit(buck2_data::WorkerInit),
}

impl CommandReproducer {
    pub fn executor(&self) -> String {
        match self {
            Self::CacheQuery(..) => "cache_query".to_owned(),
            Self::CacheHit(cache) => {
                let cache_type = cache.cache_type;
                match buck2_data::CacheHitType::try_from(cache_type) {
                    Ok(buck2_data::CacheHitType::RemoteDepFileCache) => {
                        "re_dep_file_cache".to_owned()
                    }
                    _ => "cache".to_owned(),
                }
            }
            Self::ReExecute(execute) => executor_with_platform(execute),
            Self::LocalExecute(..) => "local".to_owned(),
            Self::WorkerExecute(..) => "worker".to_owned(),
            Self::WorkerInit(..) => "worker_init".to_owned(),
        }
    }

    pub fn from_buck_data(
        data: &buck2_data::buck_event::Data,
        options: &WhatRanOptions,
    ) -> Option<Self> {
        match data {
            buck2_data::buck_event::Data::SpanStart(span) => match &span.data {
                Some(buck2_data::span_start_event::Data::ExecutorStage(executor_stage)) => {
                    match &executor_stage.stage {
                        Some(buck2_data::executor_stage_start::Stage::CacheQuery(cache_hit))
                            if options.emit_cache_queries =>
                        {
                            return Some(CommandReproducer::CacheQuery(cache_hit.clone()));
                        }
                        Some(buck2_data::executor_stage_start::Stage::CacheHit(cache_hit))
                            if !options.skip_cache_hits =>
                        {
                            return Some(CommandReproducer::CacheHit(cache_hit.clone()));
                        }
                        Some(buck2_data::executor_stage_start::Stage::Re(re_stage))
                            if !options.skip_remote_executions =>
                        {
                            match &re_stage.stage {
                                Some(buck2_data::re_stage::Stage::Execute(execute)) => {
                                    return Some(CommandReproducer::ReExecute(execute.clone()));
                                }
                                _ => {}
                            }
                        }
                        Some(buck2_data::executor_stage_start::Stage::Local(local_stage)) => {
                            if !options.skip_local_executions {
                                match &local_stage.stage {
                                    Some(buck2_data::local_stage::Stage::Execute(
                                        local_execute,
                                    )) => {
                                        return Some(CommandReproducer::LocalExecute(
                                            local_execute.clone(),
                                        ));
                                    }
                                    Some(buck2_data::local_stage::Stage::WorkerExecute(
                                        worker_execute,
                                    )) => {
                                        return Some(CommandReproducer::WorkerExecute(
                                            worker_execute.clone(),
                                        ));
                                    }
                                    Some(buck2_data::local_stage::Stage::WorkerInit(
                                        worker_init,
                                    )) => {
                                        return Some(CommandReproducer::WorkerInit(
                                            worker_init.clone(),
                                        ));
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

        None
    }
}

impl fmt::Display for CommandReproducer {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            CommandReproducer::CacheQuery(cache_query) => {
                write!(formatter, "{}", cache_query.action_digest)
            }
            CommandReproducer::CacheHit(cache_hit) => {
                write!(formatter, "{}", cache_hit.action_digest)
            }
            CommandReproducer::ReExecute(re_execute) => {
                write!(formatter, "{}", re_execute.action_digest)
            }
            CommandReproducer::LocalExecute(local_execute) => {
                if let Some(command) = &local_execute.command {
                    write!(formatter, "{}", command_to_string(command))
                } else {
                    Ok(())
                }
            }
            CommandReproducer::WorkerExecute(worker_execute) => {
                if let Some(command) = &worker_execute.command {
                    write!(
                        formatter,
                        "{}",
                        worker_command_as_fallback_to_string(command)
                    )
                } else {
                    Ok(())
                }
            }
            CommandReproducer::WorkerInit(worker_init) => {
                if let Some(command) = &worker_init.command {
                    write!(formatter, "{}", command_to_string(command))
                } else {
                    Ok(())
                }
            }
        }
    }
}

pub struct Command<'a> {
    env: &'a Vec<buck2_data::EnvironmentEntry>,
    argv: &'a Vec<String>,
}

impl<'a> From<&'a buck2_data::LocalCommand> for Command<'a> {
    fn from(command: &'a buck2_data::LocalCommand) -> Self {
        Command {
            env: &command.env,
            argv: &command.argv,
        }
    }
}

impl<'a> From<&'a buck2_data::WorkerCommand> for Command<'a> {
    fn from(command: &'a buck2_data::WorkerCommand) -> Self {
        Command {
            env: &command.env,
            argv: &command.argv,
        }
    }
}

impl<'a> From<&'a buck2_data::WorkerInitCommand> for Command<'a> {
    fn from(command: &'a buck2_data::WorkerInitCommand) -> Self {
        Command {
            env: &command.env,
            argv: &command.argv,
        }
    }
}

pub fn worker_command_as_fallback_to_string(command: &buck2_data::WorkerCommand) -> String {
    let mut argv = command.fallback_exe.to_vec();
    argv.extend(command.argv.to_vec());
    command_to_string(Command {
        env: &command.env,
        argv: &argv,
    })
}

pub fn command_to_string<'a>(command: impl Into<Command<'a>>) -> String {
    let command = command.into();
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
    shlex::try_join(cmd.iter().map(|e| e.as_ref())).expect("Null byte unexpected")
}

impl WhatRanOutputWriter for SuperConsole {
    fn emit_command(&mut self, command: WhatRanOutputCommand<'_>) -> buck2_error::Result<()> {
        // TODO: Change this API to just produce a String.
        let msg = WhatRanCommandConsoleFormat {
            reason: command.reason,
            identity: command.identity,
            repro: command.repro,
        }
        .to_string();
        self.emit(Lines(vec![Line::sanitized(&msg)]));
        Ok(())
    }
}

/// A consistent format for printing that we are about to run an action.
pub struct WhatRanCommandConsoleFormat<'a> {
    pub reason: &'a str,
    pub identity: &'a str,
    pub repro: CommandReproducer,
}

impl<'a> fmt::Display for WhatRanCommandConsoleFormat<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Running action: {} ({}), {} executor: {}",
            self.identity,
            self.reason,
            self.repro.executor(),
            self.repro
        )
    }
}

fn executor_with_platform(execute: &buck2_data::ReExecute) -> String {
    if let Some(platform) = &execute.platform {
        let platform = platform
            .properties
            .iter()
            .map(|Property { name, value }| format!("{}={}", name, value))
            .collect::<Vec<String>>()
            .join(",");
        format!("re({})", platform)
    } else {
        "re".to_owned()
    }
}

#[cfg(test)]
mod tests {
    use buck2_data::ReExecute;
    use buck2_data::RePlatform;

    use super::*;

    #[test]
    fn test_executor_with_platform() {
        let execute = ReExecute {
            action_digest: "placeholder".to_owned(),
            platform: Some(RePlatform {
                properties: vec![
                    Property {
                        name: "platform".to_owned(),
                        value: "linux-remote-execution".to_owned(),
                    },
                    Property {
                        name: "name1".to_owned(),
                        value: "value1".to_owned(),
                    },
                ],
            }),
            action_key: None,
            use_case: "".to_owned(),
        };
        let result = executor_with_platform(&execute);
        assert_eq!(
            result,
            "re(platform=linux-remote-execution,name1=value1)".to_owned()
        );
    }

    #[test]
    fn test_executor_with_platform_no_platform() {
        let execute = buck2_data::ReExecute::default();
        let result = executor_with_platform(&execute);
        assert_eq!(result, "re".to_owned());
    }
}
