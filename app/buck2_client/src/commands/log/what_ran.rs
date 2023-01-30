/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashMap;

use async_trait::async_trait;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::stream_value::StreamValue;
use buck2_client_ctx::subscribers::event_log::options::EventLogOptions;
use buck2_event_observer::what_ran;
use buck2_event_observer::what_ran::CommandReproducer;
use buck2_event_observer::what_ran::WhatRanOptions;
use buck2_event_observer::what_ran::WhatRanOutputCommand;
use buck2_event_observer::what_ran::WhatRanOutputCommandExtra;
use buck2_event_observer::what_ran::WhatRanOutputWriter;
use buck2_event_observer::what_ran::WhatRanRelevantAction;
use buck2_event_observer::what_ran::WhatRanState;
use dupe::Dupe;
use futures::stream::Stream;
use futures::TryStreamExt;
use indexmap::IndexMap;
use tokio::runtime;

#[derive(
    Debug,
    serde::Serialize,
    serde::Deserialize,
    Clone,
    Dupe,
    clap::ArgEnum
)]
#[clap(rename_all = "snake_case")]
pub enum WhatRanSubcommandOutput {
    Tabulated,
    Json,
}

/// This command outputs everything the last invocation of Buck2 ran. Other invocations can be
/// targeted using the flags.
///
///
/// The output is presented as a series of tab-deliminated records with the following structure:
///
/// The reason for executing a given command. That's either to build or to test.
///
/// The identity of this command. This will include the target that ran required it.
///
/// The executor for this command. This will either be RE or local.
///
/// Details to reproduce it. For RE, that's the action digest. For local, the command.
///
///
/// To reproduce an action that ran on RE, use the following command then follow the instructions.
/// The DIGEST is of the form `hash:size`.
///
/// frecli cas download-action DIGEST
///
///
/// To reproduce an action that ran locally, make sure your working directory is the project root
/// (if unsure, use `buck2 root --kind project` to find it), then run the command. The command is
/// already shell-quoted.
#[derive(Debug, clap::Parser)]
pub struct WhatRanCommand {
    #[clap(flatten)]
    pub common: WhatRanCommandCommon,

    /// Show only commands that failed
    #[clap(long)]
    pub failed: bool,
}

#[derive(Debug, clap::Parser)]
pub struct WhatRanCommandCommon {
    #[clap(flatten)]
    event_log: EventLogOptions,

    #[clap(
        long = "--format",
        help = "Which output format to use for this command",
        default_value = "tabulated",
        ignore_case = true,
        arg_enum
    )]
    pub output: WhatRanSubcommandOutput,

    #[clap(flatten)]
    pub options: WhatRanOptions,
}

impl WhatRanCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let Self {
            common:
                WhatRanCommandCommon {
                    event_log,
                    mut output,
                    options,
                },
            failed,
        } = self;

        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async move {
            let log_path = event_log.get(&ctx)?;
            let (invocation, events) = log_path.unpack_stream().await?;

            buck2_client_ctx::eprintln!(
                "Showing commands from: {}",
                shlex::join(invocation.command_line_args.iter().map(|e| e.as_str()))
            )?;

            if failed {
                WhatFailedImpl::execute(events, &mut output, &options).await?;
            } else {
                WhatRanImpl::execute(events, &mut output, &options).await?;
            };

            anyhow::Ok(())
        })?;

        ExitResult::success()
    }
}

#[async_trait]
trait WhatRanComandImplementation: Default {
    fn event(
        &mut self,
        event: Box<buck2_data::BuckEvent>,
        output: &mut impl WhatRanOutputWriter,
        options: &WhatRanOptions,
    ) -> anyhow::Result<()>;

    async fn execute(
        mut events: impl Stream<Item = anyhow::Result<StreamValue>> + Unpin + Send,
        output: &mut (impl WhatRanOutputWriter + Send),
        options: &WhatRanOptions,
    ) -> anyhow::Result<()> {
        let mut cmd = Self::default();

        while let Some(event) = events.try_next().await? {
            match event {
                StreamValue::Event(event) => cmd.event(event, output, options)?,
                _ => {}
            }
        }

        Ok(())
    }
}

/// The state for a WhatRan command. This is all the events we have seen that are
/// WhatRanRelevantActions. This emits the actions immediately.
#[derive(Default)]
pub struct WhatRanImpl {
    /// Maps action spans to their details.
    known_actions: HashMap<u64, Box<buck2_data::BuckEvent>>,
}

impl WhatRanState<u64> for WhatRanImpl {
    fn get(&self, span_id: u64) -> Option<WhatRanRelevantAction<'_>> {
        self.known_actions
            .get(&span_id)
            .and_then(|e| e.data.as_ref())
            .and_then(WhatRanRelevantAction::from_buck_data)
    }
}

impl WhatRanComandImplementation for WhatRanImpl {
    /// Receive a new event. We start by emiting it if it's relevant (since that only takes a
    /// borrow), and then if it's relevant as a parent, we store it for latter use. Note that in
    /// practice we don't expect the event to be *both* relevant to emit *and* a
    /// WhatRanRelevantAction, but it doesn't hurt to always check both.
    fn event(
        &mut self,
        event: Box<buck2_data::BuckEvent>,
        output: &mut impl WhatRanOutputWriter,
        options: &WhatRanOptions,
    ) -> anyhow::Result<()> {
        if let Some(data) = &event.data {
            what_ran::emit_event_if_relevant(event.parent_id, data, &*self, output, options)?;

            if WhatRanRelevantAction::from_buck_data(data).is_some() {
                self.known_actions.insert(event.span_id, event);
            }
        }

        Ok(())
    }
}

/// The state for a WhatRan command when only showing actions that failed. This stores all the events
/// we have seen that are WhatRanRelevantActions, and the CommandReproducer associated with them.
#[derive(Default)]
pub struct WhatFailedImpl {
    /// Maps action spans to their details.
    known_actions: HashMap<u64, WhatFailedEntry>,
}

#[allow(clippy::vec_box)]
struct WhatFailedEntry {
    /// Knwon to be a WhatRanRelevantAction.
    event: Box<buck2_data::BuckEvent>,

    /// Known to be a CommandReproducer.
    reproducers: Vec<Box<buck2_data::BuckEvent>>,
}

impl WhatRanState<u64> for WhatFailedImpl {
    fn get(&self, span_id: u64) -> Option<WhatRanRelevantAction<'_>> {
        self.known_actions
            .get(&span_id)
            .and_then(|e| e.event.data.as_ref())
            .and_then(WhatRanRelevantAction::from_buck_data)
    }
}

impl WhatRanComandImplementation for WhatFailedImpl {
    /// Receive a new event. We start by emiting it if it's relevant (since that only takes a
    /// borrow), and then if it's relevant as a parent, we store it for latter use. Note that in
    /// practice we don't expect the event to be *both* relevant to emit *and* a
    /// WhatRanRelevantAction, but it doesn't hurt to always check both.
    fn event(
        &mut self,
        event: Box<buck2_data::BuckEvent>,
        output: &mut impl WhatRanOutputWriter,
        options: &WhatRanOptions,
    ) -> anyhow::Result<()> {
        if let Some(data) = &event.data {
            if WhatRanRelevantAction::from_buck_data(data).is_some() {
                self.known_actions.insert(
                    event.span_id,
                    WhatFailedEntry {
                        event,
                        reproducers: Default::default(),
                    },
                );
                return Ok(());
            }

            if CommandReproducer::from_buck_data(data, options).is_some() {
                if let Some(entry) = self.known_actions.get_mut(&event.parent_id) {
                    entry.reproducers.push(event);
                }
                return Ok(());
            }

            match data {
                buck2_data::buck_event::Data::SpanEnd(span) => match &span.data {
                    Some(buck2_data::span_end_event::Data::ActionExecution(action))
                        if action.failed =>
                    {
                        if let Some(entry) = self.known_actions.remove(&event.span_id) {
                            let action = WhatRanRelevantAction::from_buck_data(
                                entry.event.data.as_ref().expect("Checked above"),
                            );

                            for repro in entry.reproducers.iter() {
                                what_ran::emit_reproducer(
                                    action,
                                    CommandReproducer::from_buck_data(
                                        repro.data.as_ref().expect("Checked above"),
                                        options,
                                    )
                                    .expect("Checked above"),
                                    output,
                                )?;
                            }
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        Ok(())
    }
}

/// An output that writes to stdout in a tabulated format.
impl WhatRanOutputWriter for WhatRanSubcommandOutput {
    fn emit_command(&mut self, command: WhatRanOutputCommand<'_>) -> anyhow::Result<()> {
        match self {
            Self::Tabulated => {
                buck2_client_ctx::println!(
                    "{}\t{}\t{}\t{}",
                    command.reason(),
                    command.identity(),
                    command.repro().executor(),
                    command.repro().as_human_readable()
                )?;
            }
            Self::Json => {
                let reproducer = match command.repro() {
                    CommandReproducer::CacheQuery(cache_hit) => JsonReproducer::CacheQuery {
                        digest: &cache_hit.action_digest,
                    },
                    CommandReproducer::CacheHit(cache_hit) => JsonReproducer::Cache {
                        digest: &cache_hit.action_digest,
                    },
                    CommandReproducer::ReExecute(re_execute) => JsonReproducer::Re {
                        digest: &re_execute.action_digest,
                    },
                    CommandReproducer::LocalExecute(local_execute) => JsonReproducer::Local {
                        command: local_execute.command.as_ref().map_or_else(
                            || Cow::Owned(Vec::new()),
                            |command| Cow::Borrowed(command.argv.as_ref()),
                        ),
                        env: local_execute
                            .command
                            .as_ref()
                            .into_iter()
                            .flat_map(|command| command.env.iter())
                            .map(|entry| (entry.key.as_ref(), entry.value.as_ref()))
                            .collect(),
                    },
                };

                let command = JsonCommand {
                    reason: command.reason(),
                    identity: command.identity(),
                    reproducer,
                    extra: command.extra().map(Into::into),
                };

                let serialized_command = serde_json::to_string(&command)?;
                buck2_client_ctx::println!("{}", serialized_command)?;
            }
        };

        Ok(())
    }
}

#[derive(serde::Serialize)]
struct JsonCommand<'a> {
    reason: &'a str,
    identity: &'a str,
    reproducer: JsonReproducer<'a>,
    #[serde(skip_serializing_if = "Option::is_none")]
    extra: Option<JsonExtra<'a>>,
}

#[derive(serde::Serialize)]
#[serde(tag = "executor", content = "details")]
enum JsonReproducer<'a> {
    CacheQuery {
        digest: &'a str,
    },
    Cache {
        digest: &'a str,
    },
    Re {
        digest: &'a str,
    },
    Local {
        command: Cow<'a, [String]>,
        env: IndexMap<&'a str, &'a str>,
    },
}

#[derive(serde::Serialize)]
#[serde(rename_all = "lowercase")]
enum JsonExtra<'a> {
    TestCases(&'a [String]),
}

impl<'a> From<WhatRanOutputCommandExtra<'a>> for JsonExtra<'a> {
    fn from(extra: WhatRanOutputCommandExtra<'a>) -> JsonExtra<'a> {
        match extra {
            WhatRanOutputCommandExtra::TestCases(cases) => JsonExtra::TestCases(cases),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_base_command() -> JsonCommand<'static> {
        let command = Cow::Owned(vec!["some".to_owned(), "command".to_owned()]);
        let mut env = IndexMap::new();
        env.insert("KEY", "val");

        JsonCommand {
            reason: "test.run",
            identity: "some/target",
            reproducer: JsonReproducer::Local { command, env },
            extra: None,
        }
    }

    #[test]
    fn serialize_what_ran_command_no_extr() -> anyhow::Result<()> {
        let command = make_base_command();

        let expected = r#"{
  "reason": "test.run",
  "identity": "some/target",
  "reproducer": {
    "executor": "Local",
    "details": {
      "command": [
        "some",
        "command"
      ],
      "env": {
        "KEY": "val"
      }
    }
  }
}"#;
        assert_eq!(expected, serde_json::to_string_pretty(&command)?);
        Ok(())
    }

    #[test]
    fn serialize_what_ran_command_with_extra() -> anyhow::Result<()> {
        let mut command = make_base_command();
        let cases = &["case".to_owned()];
        command.extra = Some(JsonExtra::TestCases(cases));

        let expected = r#"{
  "reason": "test.run",
  "identity": "some/target",
  "reproducer": {
    "executor": "Local",
    "details": {
      "command": [
        "some",
        "command"
      ],
      "env": {
        "KEY": "val"
      }
    }
  },
  "extra": {
    "testcases": [
      "case"
    ]
  }
}"#;
        assert_eq!(expected, serde_json::to_string_pretty(&command)?);
        Ok(())
    }
}
