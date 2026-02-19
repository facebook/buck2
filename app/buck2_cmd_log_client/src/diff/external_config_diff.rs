/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeMap;

use buck2_client_ctx::client_ctx::BuckSubcommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_event_log::stream_value::StreamValue;
use derive_more::Display;
use futures::Stream;
use futures::TryStreamExt;
use serde::Serialize;

use crate::diff::diff_options::DiffEventLogOptions;

const PROJECT_ROOT: &str = "";

/// Identifies the diff between external buckconfigs between two commands.
#[derive(Debug, clap::Parser)]
pub struct ExternalConfigDiffCommand {
    #[clap(flatten)]
    diff_event_log: DiffEventLogOptions,
}

fn insert_config_value(dict: &mut BTreeMap<String, String>, config: &buck2_data::ConfigValue) {
    let config_cell = config
        .cell
        .clone()
        .map_or(PROJECT_ROOT.to_owned(), |cell| format!("({cell})"));
    dict.insert(
        format!(
            "{}{}.{}",
            config_cell,
            config.section.clone(),
            config.key.clone()
        ),
        config.value.clone(),
    );
}

fn insert_config_values(dict: &mut BTreeMap<String, String>, configs: &[buck2_data::ConfigValue]) {
    configs
        .iter()
        .for_each(|config_value| insert_config_value(dict, config_value))
}

fn process_buckconfig_data(dict: &mut BTreeMap<String, String>, event: &buck2_data::BuckEvent) {
    use buck2_data::buckconfig_component::Data::ConfigFile;
    use buck2_data::buckconfig_component::Data::ConfigValue;
    use buck2_data::buckconfig_component::Data::GlobalExternalConfigFile;
    use buck2_data::config_file::Data::GlobalExternalConfig;
    use buck2_data::config_file::Data::ProjectRelativePath;

    if let Some(buck2_data::buck_event::Data::Instant(end)) = event.data.as_ref() {
        if let Some(buck2_data::instant_event::Data::BuckconfigInputValues(input)) =
            end.data.as_ref()
        {
            input
                .components
                .iter()
                .for_each(|component| match component.data.as_ref() {
                    Some(ConfigValue(config_value)) => insert_config_value(dict, config_value),
                    Some(ConfigFile(config_file)) => config_file
                        .data
                        .as_ref()
                        .into_iter()
                        .for_each(|data| match data {
                            ProjectRelativePath(p) => {
                                dict.insert(p.clone(), "".to_owned());
                            }
                            GlobalExternalConfig(external_config_values) => {
                                insert_config_values(dict, &external_config_values.values)
                            }
                        }),
                    Some(GlobalExternalConfigFile(external_config_file)) => {
                        insert_config_values(dict, &external_config_file.values)
                    }
                    _ => {}
                });
        }
    }
}

async fn get_external_buckconfig_dict(
    mut events: impl Stream<Item = buck2_error::Result<StreamValue>> + Unpin + Send,
) -> buck2_error::Result<BTreeMap<String, String>> {
    let mut dict: BTreeMap<String, String> = BTreeMap::new();
    while let Some(event) = events.try_next().await? {
        if let StreamValue::Event(event) = event {
            process_buckconfig_data(&mut dict, &event);
        }
    }
    Ok(dict)
}

#[derive(Debug, Hash, PartialEq, Eq, Serialize)]
pub enum DiffType<'a> {
    Changed {
        key: &'a str,
        old_value: &'a str,
        new_value: &'a str,
    },
    FirstOnly {
        key: &'a str,
        value: &'a str,
    },
    SecondOnly {
        key: &'a str,
        value: &'a str,
    },
}

impl Display for DiffType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DiffType::Changed {
                key,
                old_value,
                new_value,
            } => write!(f, "{key}: {old_value} | {new_value}"),
            DiffType::FirstOnly { key, value } => write!(f, "{key}: {value} | _"),
            DiffType::SecondOnly { key, value } => write!(f, "{key}: _ | {value}"),
        }
    }
}

impl BuckSubcommand for ExternalConfigDiffCommand {
    const COMMAND_NAME: &'static str = "log-diff-buckconfig";

    async fn exec_impl(
        self,
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        _events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let (log_path1, log_path2) = self.diff_event_log.get(&ctx).await?;

        let (invocation1, events1) = log_path1.unpack_stream().await?;
        let (invocation2, events2) = log_path2.unpack_stream().await?;

        buck2_client_ctx::println!(
            "Identifying the diff of external buckconfigs between: \n{} and \n{}",
            invocation1.display_command_line(),
            invocation2.display_command_line()
        )?;

        // External buckconfigs are stored in the event log in order and can have overrides
        // We first resolve them into a single dict
        let dict1 = get_external_buckconfig_dict(events1).await?;
        let dict2 = get_external_buckconfig_dict(events2).await?;
        let mut diffs = Vec::new();
        for (key, value) in dict1.iter() {
            if let Some(new_value) = dict2.get(key) {
                if new_value != value {
                    diffs.push(DiffType::Changed {
                        key,
                        old_value: value,
                        new_value,
                    });
                }
            } else {
                diffs.push(DiffType::FirstOnly { key, value });
            }
        }

        for (key, value) in dict2.iter() {
            if !dict1.contains_key(key) {
                diffs.push(DiffType::SecondOnly { key, value });
            }
        }
        let json_diffs = serde_json::to_string_pretty(&diffs)?;
        buck2_client_ctx::println!("{}", json_diffs)?;
        ExitResult::success()
    }
}
