/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Server-side implementation of `buck2 targets --resolve-alias` command.

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum ResolveAliasError {
    #[error("`--stat` format is not supported by `--resolve-alias`")]
    StatFormatNotSupported,
}

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Write;

use buck2_cli_proto::targets_request::OutputFormat;
use buck2_cli_proto::TargetsRequest;
use buck2_cli_proto::TargetsResponse;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_node::nodes::attributes::PACKAGE;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::FutureExt;

use crate::commands::targets::fmt::JsonWriter;
use crate::json::QuotedJson;

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
        self.entry_item(buffer, &mut first, "alias", QuotedJson::quote_str(alias));
        // Using a format consistent wit hthe output of `buck2 targets`
        self.entry_item(
            buffer,
            &mut first,
            PACKAGE,
            QuotedJson::quote_display(label.pkg()),
        );
        self.entry_item(
            buffer,
            &mut first,
            "name",
            QuotedJson::quote_display(label.name()),
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
    mut dice: DiceTransaction,
    request: &TargetsRequest,
    parsed_target_patterns: Vec<ParsedPattern<TargetPatternExtra>>,
) -> buck2_error::Result<TargetsResponse> {
    // If we are only asked to resolve aliases, then don't expand any of the patterns, and just
    // print them out. This expects the aliases to resolve to individual targets.
    let parsed_target_patterns = std::iter::zip(&request.target_patterns, parsed_target_patterns)
        .map(|(alias, pattern)| match pattern {
            ParsedPattern::Target(package, target_name, TargetPatternExtra) => {
                Ok((package, target_name))
            }
            _ => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Input,
                "Invalid alias (does not expand to a single target): `{}`",
                alias
            )),
        })
        .collect::<Result<Vec<_>, _>>()?;

    let packages = parsed_target_patterns
        .iter()
        .map(|(package, _name)| package.dupe())
        .collect::<HashSet<_>>();

    let packages: HashMap<_, _> = dice
        .compute_join(packages, |ctx: &mut _, package| {
            async move {
                (
                    package.dupe(),
                    ctx.get_interpreter_results(package.dupe())
                        .await
                        .map_err(buck2_error::Error::from),
                )
            }
            .boxed()
        })
        .await
        .into_iter()
        .collect();

    let mut buffer = String::new();

    let output_format = OutputFormat::try_from(request.output_format)
        .internal_error("Invalid value of `output_format`")?;

    let json_writer;

    let formatter = match output_format {
        OutputFormat::Unknown => return Err(internal_error!("`output_format` not set")),
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
            .with_buck_error_context(|| format!("Package does not exist: `{}`", package))
            .and_then(|package_data| {
                package_data
                    .as_ref()
                    .map_err(|e| e.dupe())
                    .with_buck_error_context(|| {
                        format!("Package cannot be evaluated: `{}`", package)
                    })?
                    .resolve_target(target_name)
                    .with_buck_error_context(|| {
                        format!(
                            "Target does not exist in package `{}`: `{}`",
                            package, target_name,
                        )
                    })
            })
            .with_buck_error_context(|| format!("Invalid alias: `{}`", alias))?;

        if needs_separator {
            formatter.separator(&mut buffer);
        }
        needs_separator = true;
        formatter.emit(&alias, node.label(), &mut buffer);
    }

    formatter.end(&mut buffer);

    Ok(TargetsResponse {
        error_count: 0,
        serialized_targets_output: buffer,
    })
}
