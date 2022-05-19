/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{collections::HashMap, io::Write, str::FromStr};

use async_trait::async_trait;
use buck2_common::{
    dice::cells::HasCellResolver,
    legacy_configs::{dice::HasLegacyConfigs, LegacyBuckConfigLocation, LegacyBuckConfigValue},
};
use buck2_core::cells::*;
use clap::arg_enum;
use cli_proto::ClientContext;
use gazebo::prelude::*;
use serde_json::json;
use structopt::{clap, StructOpt};

use crate::{
    commands::{
        audit::AuditSubcommand,
        common::{
            value_name_variants, CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions,
        },
    },
    daemon::server::ServerCommandContext,
};

structopt::clap::arg_enum! {
    #[derive(Debug, Dupe, Clone, Copy, serde::Serialize, serde::Deserialize)]
    enum OutputFormat {
        Simple,
        Json,
    }
}

#[derive(Debug, Clone, Copy, Dupe, serde::Serialize, serde::Deserialize)]
enum LocationStyle {
    None,
    Direct,
    Extended,
}

impl FromStr for LocationStyle {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "none" => Ok(Self::None),
            "direct" => Ok(Self::Direct),
            "extended" => Ok(Self::Extended),
            _ => Err("bad".to_owned()),
        }
    }
}

#[derive(Debug, Clone, Copy, Dupe, serde::Serialize, serde::Deserialize)]
enum ValueStyle {
    Resolved,
    Raw,
    Both,
}

impl FromStr for ValueStyle {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "resolved" => Ok(Self::Resolved),
            "raw" => Ok(Self::Raw),
            "both" => Ok(Self::Both),
            _ => Err("bad".to_owned()),
        }
    }
}

#[derive(Debug, StructOpt, serde::Serialize, serde::Deserialize)]
#[structopt(name = "audit-config", about = "buck audit config")]
pub struct AuditConfigCommand {
    #[structopt(flatten)]
    pub config_opts: CommonConfigOptions,

    #[structopt(long = "cell")]
    cell: Option<String>,

    #[structopt(
        long,
        alias = "style",
        possible_values = &OutputFormat::variants(),
        value_name = value_name_variants(&OutputFormat::variants()),
        case_insensitive = true,
    )]
    output_format: Option<OutputFormat>,

    #[structopt(long)]
    json: bool,

    #[structopt(long = "location", default_value = "none", possible_values=&["none", "direct", "extended"])]
    location_style: LocationStyle,

    #[structopt(long = "value", default_value = "resolved", possible_values=&["resolved", "raw", "both"])]
    value_style: ValueStyle,

    #[structopt(
        name = "SPECS",
        help = "config section/key specs of the form `section` or `section.key`. If any specs are provided, only values matching a spec will be printed (section headers will be printed only for sections with a key matching the spec)."
    )]
    specs: Vec<String>,
}

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

impl AuditConfigCommand {
    fn output_format(&self) -> OutputFormat {
        if let Some(format) = &self.output_format {
            *format
        } else if self.json {
            OutputFormat::Json
        } else {
            OutputFormat::Simple
        }
    }
}

#[async_trait]
impl AuditSubcommand for AuditConfigCommand {
    async fn server_execute(
        &self,
        mut server_ctx: ServerCommandContext,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        let cwd = &server_ctx.working_dir;
        let ctx = server_ctx.dice_ctx().await?;
        let cell_resolver = ctx.get_cell_resolver().await;

        let working_dir_cell = cell_resolver.find(cwd)?;
        let cell_alias_resolver = cell_resolver
            .get(working_dir_cell)
            .unwrap()
            .cell_alias_resolver();

        let cell = match &self.cell {
            Some(v) => cell_alias_resolver.resolve(&CellAlias::new(v.to_owned()))?,
            None => working_dir_cell,
        };

        let config = ctx.get_legacy_config_for_cell(cell).await.unwrap();

        let specs = self.specs.map(|v| v.split1("."));
        let filter = move |section: &str, key: &str| {
            if specs.is_empty() {
                true
            } else {
                for s in &specs {
                    if section == s.0 && (s.1 == "" || key == s.1) {
                        return true;
                    }
                }
                false
            }
        };

        let mut stdout = server_ctx.stdout()?;

        match self.output_format() {
            OutputFormat::Json => writeln!(
                &mut stdout,
                "{}",
                json!(
                    config
                        .all_sections()
                        .flat_map(|(section, cfg)| {
                            cfg.iter()
                                .filter_map(|(key, value)| match filter(section, key) {
                                    true => Some((
                                        format!("{}.{}", section, key),
                                        value.as_str().to_owned(),
                                    )),
                                    false => None,
                                })
                                .collect::<HashMap<String, String>>()
                        })
                        .collect::<HashMap<String, String>>()
                )
            )?,
            OutputFormat::Simple => {
                for section in config.sections() {
                    let mut printed_section = false;
                    let values = config.get_section(section).unwrap();
                    for (key, value) in values.iter() {
                        if filter(section, key) {
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

        Ok(())
    }

    fn config_opts(&self) -> Option<&CommonConfigOptions> {
        Some(&self.config_opts)
    }

    fn console_opts(&self) -> Option<&CommonConsoleOptions> {
        None
    }

    fn event_log_opts(&self) -> Option<&CommonEventLogOptions> {
        None
    }
}
