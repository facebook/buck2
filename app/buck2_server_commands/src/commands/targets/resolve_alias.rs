/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Server-side implementation of `buck2 targets --resolve-alias` command.

#[derive(Debug, thiserror::Error)]
enum ResolveAliasError {
    #[error("`output_format` not set (internal error)")]
    OutputFormatNotSet,
    #[error("`--stat` format is not supported by `--resolve-alias`")]
    StatFormatNotSupported,
}

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Write;

use anyhow::Context;
use buck2_cli_proto::targets_request::OutputFormat;
use buck2_cli_proto::TargetsRequest;
use buck2_cli_proto::TargetsResponse;
use buck2_common::result::ToSharedResultExt;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::target::label::TargetLabel;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_node::nodes::attributes::PACKAGE;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::StreamExt;

use crate::commands::targets::fmt::JsonWriter;
use crate::json::quote_json_string;

trait ResolveAliasFormatter {
    /// Before writing anything.
    fn begin(&self, buffer: &mut String);

    /// After writing everything.
    fn end(&self, buffer: &mut String);

    /// Between items
    fn separator(&self, buffer: &mut String);

    /// Emit an alias
    fn emit(&self, alias: &str, label: &TargetLabel, buffer: &mut String);
}

impl ResolveAliasFormatter for JsonWriter {
    fn begin(&self, buffer: &mut String) {
        self.begin(buffer);
    }

    fn end(&self, buffer: &mut String) {
        self.end(buffer);
    }

    fn separator(&self, buffer: &mut String) {
        self.separator(buffer);
    }

    fn emit(&self, alias: &str, label: &TargetLabel, buffer: &mut String) {
        let mut first = true;
        self.entry_start(buffer);
        self.entry_item(buffer, &mut first, "alias", &quote_json_string(alias));
        // Using a format consistent wit hthe output of `buck2 targets`
        self.entry_item(
            buffer,
            &mut first,
            PACKAGE,
            &quote_json_string(&label.pkg().to_string()),
        );
        self.entry_item(
            buffer,
            &mut first,
            "name",
            &quote_json_string(label.name().as_str()),
        );
        self.entry_end(buffer, first);
    }
}

struct LinesWriter;

impl ResolveAliasFormatter for LinesWriter {
    fn begin(&self, _buffer: &mut String) {}

    fn end(&self, _buffer: &mut String) {}

    fn separator(&self, buffer: &mut String) {
        buffer.push('\n');
    }

    fn emit(&self, _alias: &str, label: &TargetLabel, buffer: &mut String) {
        write!(buffer, "{}", label).unwrap();
    }
}

pub(crate) async fn targets_resolve_aliases(
    dice: DiceTransaction,
    request: &TargetsRequest,
    parsed_target_patterns: Vec<ParsedPattern<TargetPatternExtra>>,
) -> anyhow::Result<TargetsResponse> {
    // If we are only asked to resolve aliases, then don't expand any of the patterns, and just
    // print them out. This expects the aliases to resolve to individual targets.
    let parsed_target_patterns = std::iter::zip(&request.target_patterns, parsed_target_patterns)
        .map(|(alias, pattern)| match pattern {
            ParsedPattern::Target(package, target_name, TargetPatternExtra) => {
                Ok((package, target_name))
            }
            _ => Err(anyhow::anyhow!(
                "Invalid alias (does not expand to a single target): `{}`",
                alias.value
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;

    let packages = parsed_target_patterns
        .iter()
        .map(|(package, _name)| package.dupe())
        .collect::<HashSet<_>>();

    let packages = packages
        .into_iter()
        .map(|package| {
            let dice = &dice;
            async move {
                (
                    package.dupe(),
                    dice.get_interpreter_results(package.dupe())
                        .await
                        .shared_error(),
                )
            }
        })
        .collect::<FuturesUnordered<_>>()
        .collect::<HashMap<_, _>>()
        .await;

    let mut buffer = String::new();

    let output_format = OutputFormat::from_i32(request.output_format)
        .context("Invalid value of `output_format` (internal error)")?;

    let json_writer;

    let formatter = match output_format {
        OutputFormat::Unknown => return Err(ResolveAliasError::OutputFormatNotSet.into()),
        OutputFormat::Text => &LinesWriter as &dyn ResolveAliasFormatter,
        OutputFormat::Json => {
            json_writer = JsonWriter { json_lines: false };
            &json_writer as &dyn ResolveAliasFormatter
        }
        OutputFormat::JsonLines => {
            json_writer = JsonWriter { json_lines: true };
            &json_writer as &dyn ResolveAliasFormatter
        }
        OutputFormat::Stats => return Err(ResolveAliasError::StatFormatNotSupported.into()),
    };

    let mut needs_separator = false;

    formatter.begin(&mut buffer);

    for (alias, (package, target_name)) in
        std::iter::zip(&request.target_patterns, &parsed_target_patterns)
    {
        // NOTE: We don't technically need the node to get the label, but we need the node to
        // validate it exists.
        let node = packages
            .get(package)
            .with_context(|| format!("Package does not exist: `{}`", package))
            .and_then(|package_data| {
                package_data
                    .as_ref()
                    .map_err(|e| e.dupe())
                    .with_context(|| format!("Package cannot be evaluated: `{}`", package))?
                    .resolve_target(target_name)
                    .with_context(|| {
                        format!(
                            "Target does not exist in package `{}`: `{}`",
                            package, target_name,
                        )
                    })
            })
            .with_context(|| format!("Invalid alias: `{}`", alias.value))?;

        if needs_separator {
            formatter.separator(&mut buffer);
        }
        needs_separator = true;
        formatter.emit(&alias.value, node.label(), &mut buffer);
    }

    formatter.end(&mut buffer);

    Ok(TargetsResponse {
        error_count: 0,
        serialized_targets_output: buffer,
    })
}
