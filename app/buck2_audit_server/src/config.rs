/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::io::Write;

use async_trait::async_trait;
use buck2_audit::config::AuditConfigCommand;
use buck2_audit::config::LocationStyle;
use buck2_audit::config::OutputFormat;
use buck2_audit::config::ValueStyle;
use buck2_cli_proto::ClientContext;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfigLocation;
use buck2_common::legacy_configs::LegacyBuckConfigValue;
use buck2_core::cells::name::CellName;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use gazebo::prelude::*;
use serde_json::json;

use crate::AuditSubcommand;

fn print_location_string(
    writer: &mut impl Write,
    location: &LegacyBuckConfigLocation,
    keyword: &str,
) -> anyhow::Result<()> {
    match location {
        LegacyBuckConfigLocation::File(file, line) => {
            writeln!(writer, "  ({} at {}:{})", keyword, file, line)?;
        }
        LegacyBuckConfigLocation::CommandLineArgument => {
            writeln!(writer, "  ({} on the command line)", keyword)?;
        }
    }

    Ok(())
}

fn print_location(
    writer: &mut impl Write,
    value: &LegacyBuckConfigValue,
    style: LocationStyle,
) -> anyhow::Result<()> {
    match style {
        LocationStyle::None => {}
        LocationStyle::Direct => {
            let location = value.location();
            print_location_string(writer, &location, "defined")?;
        }
        LocationStyle::Extended => {
            let stack = value.location_stack();
            let mut iter = stack.iter();
            if let Some(location) = iter.next() {
                // Extra space in the keyword as to align "defined" and "included"
                print_location_string(writer, location, "defined ")?;
            }
            for location in iter {
                print_location_string(writer, location, "included")?;
            }
        }
    }

    Ok(())
}

fn print_value(
    writer: &mut impl Write,
    key: &str,
    value: &LegacyBuckConfigValue,
    style: ValueStyle,
) -> anyhow::Result<()> {
    match style {
        ValueStyle::Resolved => {
            writeln!(writer, "    {} = {}", key, value.as_str())?;
        }
        ValueStyle::Raw => {
            writeln!(writer, "    {} = {}", key, value.raw_value())?;
        }
        ValueStyle::Both => {
            writeln!(
                writer,
                "    {} = {}\n        (raw {})",
                key,
                value.as_str(),
                value.raw_value()
            )?;
        }
    }

    Ok(())
}

#[async_trait]
impl AuditSubcommand for AuditConfigCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(async move |server_ctx, ctx| {
                let cwd = server_ctx.working_dir();
                let cell_resolver = ctx.get_cell_resolver().await?;

                let working_dir_cell = cell_resolver.find(cwd)?;
                let cell_alias_resolver = cell_resolver
                    .get(working_dir_cell)
                    .unwrap()
                    .cell_alias_resolver();

                let relevant_cell = match &self.cell {
                    Some(v) => v,
                    None => "",
                };

                let resolved_relevant_cell = cell_alias_resolver.resolve(relevant_cell)?;

                let config = ctx.get_legacy_configs().await?;

                let specs = self.specs.try_map(|v| {
                    let (cell, config) = match v.split_once("//") {
                        Some((cell, config)) => (cell_alias_resolver.resolve(cell)?, config),
                        None => (resolved_relevant_cell, v.as_str()),
                    };
                    let (section, key) = config.split1(".");
                    anyhow::Ok((cell, section, key, v))
                })?;

                let filter = move |cell: CellName, section: &str, key: &str| {
                    if specs.is_empty() {
                        if cell == resolved_relevant_cell {
                            Some(format!("{}.{}", section, key))
                        } else {
                            None
                        }
                    } else {
                        for (filter_cell, filter_section, filter_key, spec) in &specs {
                            if cell == *filter_cell
                                && &section == filter_section
                                && (filter_key == &"" || &key == filter_key)
                            {
                                return if filter_key == &"" {
                                    Some(format!("{}.{}", section, key))
                                } else {
                                    Some((*spec).to_owned())
                                };
                            }
                        }
                        None
                    }
                };

                let mut stdout = stdout.as_writer();

                match self.output_format() {
                    OutputFormat::Json => writeln!(
                        &mut stdout,
                        "{}",
                        json!(
                            config
                                .iter()
                                .flat_map(|(cell, cell_config)| cell_config
                                    .all_sections()
                                    .map(move |(section, cfg)| (cell, section, cfg)))
                                .flat_map(|(cell, section, cfg)| {
                                    cfg.iter()
                                        .filter_map(|(key, value)| {
                                            filter(cell, section, key)
                                                .map(|spec| (spec, value.as_str().to_owned()))
                                        })
                                        .collect::<HashMap<String, String>>()
                                })
                                .collect::<HashMap<String, String>>()
                        )
                    )?,
                    OutputFormat::Simple => {
                        for (cell, cell_config) in config.iter() {
                            for section in cell_config.sections() {
                                let mut printed_section = false;
                                let values = cell_config.get_section(section).unwrap();
                                for (key, value) in values.iter() {
                                    if filter(cell, section, key).is_some() {
                                        if !printed_section {
                                            writeln!(&mut stdout, "[{}]", section)?;
                                            printed_section = true;
                                        }
                                        print_value(&mut stdout, key, &value, self.value_style)?;
                                        print_location(&mut stdout, &value, self.location_style)?;
                                    }
                                }
                            }
                        }
                    }
                }

                Ok(())
            })
            .await
    }
}
