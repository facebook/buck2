/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_data::GlobalExternalConfig;
use buck2_error::conversion::from_any_with_tag;
use buck2_event_log::stream_value::StreamValue;
use serde::Serialize;
use tokio_stream::StreamExt;

use crate::commands::log::options::EventLogOptions;
use crate::commands::log::transform_format;
use crate::commands::log::LogCommandOutputFormat;
use crate::commands::log::LogCommandOutputFormatWithWriter;

const CLI: &str = "cli";
/// Display the values and origins of external configs for a selected command.
///
/// Buckconfigs are computed by joining together values from various inputs (repo, well-known directories, CLI flags). Each of these is
/// logged in the given order, with later components overriding earlier ones. For config files originating from the repo (i.e. project-relative paths), except .buckconfig.local,
/// we log the path, not the actual values.
#[derive(Debug, clap::Parser)]
pub struct ExternalConfigsCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
    #[clap(
        long,
        help = "Which output format to use for this command",
        default_value = "tabulated",
        ignore_case = true,
        value_enum
    )]
    format: LogCommandOutputFormat,
}

impl ExternalConfigsCommand {
    pub fn exec(self, _matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        let Self { event_log, format } = self;

        ctx.instant_command_no_log("log-external-configs", |ctx| async move {
            let log_path = event_log.get(&ctx).await?;

            let (invocation, mut events) = log_path.unpack_stream().await?;
            buck2_client_ctx::eprintln!(
                "Showing external configs from: {}",
                invocation.display_command_line()
            )?;

            while let Some(event) = events.try_next().await? {
                match event {
                    StreamValue::Event(event) => match event.data {
                        Some(buck2_data::buck_event::Data::Instant(instant)) => {
                            match instant.data {
                                Some(buck2_data::instant_event::Data::BuckconfigInputValues(
                                    configs,
                                )) => {
                                    log_external_configs(&configs.components, format.clone())?;
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }

            buck2_error::Ok(())
        })
        .into()
    }
}

#[derive(Serialize)]
struct ExternalConfigValueEntry<'a> {
    section: &'a str,
    key: &'a str,
    value: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    cell: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    origin: Option<&'a str>,
}

#[derive(Serialize)]
struct ExternalConfigFileEntry<'a> {
    path: &'a str,
    origin: &'a str,
}

fn write_config_value<'a>(
    config_value: &'a buck2_data::ConfigValue,
    origin_path: &'a str,
    mut log_writer: &mut LogCommandOutputFormatWithWriter,
) -> buck2_error::Result<()> {
    let external_config = ExternalConfigValueEntry {
        section: &config_value.section,
        key: &config_value.key,
        value: &config_value.value,
        cell: config_value.cell.as_deref(),
        origin: Some(origin_path),
    };

    match &mut log_writer {
        LogCommandOutputFormatWithWriter::Tabulated(writer) => {
            writeln!(
                writer,
                "{}.{} = {}\t{}\t{}",
                external_config.section,
                external_config.key,
                external_config.value,
                external_config
                    .cell
                    .map_or("".to_owned(), |cell| format!("({})", cell)),
                external_config.origin.unwrap_or_default(),
            )?;
        }
        LogCommandOutputFormatWithWriter::Json(writer) => {
            serde_json::to_writer(&mut **writer, &external_config)?;
            writer.write_all("\n".as_bytes())?;
        }
        LogCommandOutputFormatWithWriter::Csv(writer) => {
            writer
                .serialize(external_config)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        }
    }
    Ok(())
}

fn write_config_values(
    global_external_config: &GlobalExternalConfig,
    mut log_writer: &mut LogCommandOutputFormatWithWriter,
) -> buck2_error::Result<()> {
    global_external_config
        .values
        .iter()
        .try_for_each(|config_value| {
            write_config_value(
                config_value,
                &global_external_config.origin_path,
                &mut log_writer,
            )
        })
}

fn write_config_file(
    path: &str,
    mut log_writer: &mut LogCommandOutputFormatWithWriter,
) -> buck2_error::Result<()> {
    let origin = "config-file";
    let config_file = ExternalConfigFileEntry { path, origin };
    match &mut log_writer {
        LogCommandOutputFormatWithWriter::Tabulated(writer) => {
            writeln!(writer, "{}\t\t{}", path, origin)?;
        }
        LogCommandOutputFormatWithWriter::Json(writer) => {
            serde_json::to_writer(&mut **writer, &config_file)?;
            writer.write_all("\n".as_bytes())?;
        }
        LogCommandOutputFormatWithWriter::Csv(writer) => {
            writer
                .serialize(config_file)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        }
    }
    Ok(())
}

fn log_external_configs(
    components: &[buck2_data::BuckconfigComponent],
    format: LogCommandOutputFormat,
) -> buck2_error::Result<()> {
    buck2_client_ctx::stdio::print_with_writer::<buck2_error::Error, _>(|w| {
        let mut log_writer = transform_format(format, w);

        for component in components {
            use buck2_data::buckconfig_component::Data;
            use buck2_data::config_file::Data as CData;
            match &component.data {
                Some(Data::ConfigValue(config_value)) => {
                    assert!(
                        config_value.is_cli,
                        "Only false for configs coming from global external configs which have their origin set below"
                    );
                    write_config_value(config_value, CLI, &mut log_writer)?;
                }
                Some(Data::ConfigFile(config_file)) => config_file
                    .data
                    .as_ref()
                    .into_iter()
                    .try_for_each(|data| match data {
                        CData::ProjectRelativePath(p) => write_config_file(&p, &mut log_writer),
                        CData::GlobalExternalConfig(external_config_values) => {
                            write_config_values(external_config_values, &mut log_writer)
                        }
                    })?,

                Some(Data::GlobalExternalConfigFile(external_config_file)) => {
                    write_config_values(external_config_file, &mut log_writer)?
                }
                _ => {}
            }
        }
        Ok(())
    })
}
