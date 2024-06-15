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
use buck2_common::legacy_configs::configs::LegacyBuckConfigLocation;
use buck2_common::legacy_configs::configs::LegacyBuckConfigValue;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellAliasResolver;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use gazebo::prelude::*;
use serde_json::json;

use crate::ServerAuditSubcommand;

fn print_location_string(
    writer: &mut impl Write,
    location: &LegacyBuckConfigLocation,
    keyword: &str,
) -> anyhow::Result<()> {
    writeln!(writer, "  ({} {})", keyword, location)?;
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

struct Match<'a> {
    /// The original specification - important if we want to produce JSON keys.
    /// Not present if it wasn't a key match, as then we can't express it.
    spec: Option<&'a str>,
    /// The cell, or otherwise require the cell passed in by the filterer.
    cell: Option<CellName>,
    /// The section.
    section: &'a str,
    /// The key. Might be optional if the user did `foo` as their match.
    key: Option<&'a str>,
}

impl<'a> Match<'a> {
    fn parse(resolver: &CellAliasResolver, spec: &'a str) -> anyhow::Result<Self> {
        let (cell, config) = match spec.split_once("//") {
            Some((cell, config)) => (Some(resolver.resolve(cell)?), config),
            None => (None, spec),
        };
        let (section, key) = config.split1(".");
        Ok(Self {
            spec: if key == "" { None } else { Some(spec) },
            cell,
            section,
            key: if key == "" { None } else { Some(key) },
        })
    }

    fn filter(
        &self,
        default_cell: CellName,
        cell: CellName,
        section: &str,
        key: &str,
    ) -> Option<String> {
        if cell == self.cell.unwrap_or(default_cell)
            && section == self.section
            && self.key.map_or(true, |k| k == key)
        {
            Some(
                self.spec
                    .map_or_else(|| format!("{section}.{key}"), str::to_owned),
            )
        } else {
            None
        }
    }
}

struct Matches<'a> {
    matches: Vec<Match<'a>>,
}

impl<'a> Matches<'a> {
    fn parse(resolver: &CellAliasResolver, specs: &'a [String]) -> anyhow::Result<Self> {
        Ok(Self {
            matches: specs.try_map(|x| Match::parse(resolver, x))?,
        })
    }

    fn filter(
        &self,
        default_cell: CellName,
        cell: CellName,
        section: &str,
        key: &str,
    ) -> Option<String> {
        if self.matches.is_empty() {
            if cell == default_cell {
                Some(format!("{section}.{key}"))
            } else {
                None
            }
        } else {
            self.matches
                .iter()
                .find_map(|x| x.filter(default_cell, cell, section, key))
        }
    }
}

#[async_trait]
impl ServerAuditSubcommand for AuditConfigCommand {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        mut stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        _client_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        server_ctx
            .with_dice_ctx(|server_ctx, mut ctx| async move {
                let cwd = server_ctx.working_dir();
                let cell_resolver = ctx.get_cell_resolver().await?;
                let cell_alias_resolver = cell_resolver.get_cwd_cell_alias_resolver(cwd)?;

                let relevant_cell = if self.all_cells {
                    None
                } else {
                    Some(cell_alias_resolver.resolve(self.cell.as_deref().unwrap_or_default())?)
                };

                let specs = Matches::parse(cell_alias_resolver, &self.specs)?;
                let mut stdout = stdout.as_writer();

                let output_format = self.output_format();
                let mut json_output = HashMap::new();
                for (cell, _) in cell_resolver.cells() {
                    let cell_config = ctx.get_legacy_config_for_cell(cell).await?;
                    let mut printed_cell = false;
                    for (section, values) in cell_config.all_sections() {
                        let mut printed_section = false;
                        for (key, value) in values.iter() {
                            if let Some(mut spec) =
                                specs.filter(relevant_cell.unwrap_or(cell), cell, section, key)
                            {
                                match output_format {
                                    OutputFormat::Json => {
                                        if self.all_cells && !spec.contains("//") {
                                            spec = format!("{cell}//{spec}");
                                        }
                                        json_output.insert(spec, value.as_str().to_owned());
                                    }
                                    OutputFormat::Simple => {
                                        if self.all_cells && !printed_cell {
                                            writeln!(&mut stdout, "# Cell: {cell}")?;
                                            printed_cell = true;
                                        }
                                        if !printed_section {
                                            writeln!(&mut stdout, "[{section}]")?;
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
                if output_format == OutputFormat::Json {
                    writeln!(&mut stdout, "{}", json!(json_output))?;
                }

                Ok(())
            })
            .await
    }
}
