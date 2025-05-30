/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Write;

use buck2_client_ctx::exit_result::ExitResult;
use clap::Command;
use clap::builder::PossibleValue;

#[derive(Debug, clap::Parser)]
#[clap(
    name = "markdown-help-doc",
    about = "Print out the buck2 subcommand markdown docs"
)]
pub(crate) struct MarkdownHelpDocCommand {
    #[clap(
        name = "SUB_COMMAND",
        help = "The buck2 sub command we want to generate the markdown doc"
    )]
    sub_cmd: String,
}

impl MarkdownHelpDocCommand {
    pub(crate) fn exec(self, top_level_cmd: clap::Command) -> ExitResult {
        for subcommand in top_level_cmd.get_subcommands() {
            if subcommand.get_name() == self.sub_cmd {
                let markdown = cmd_markdown_help(subcommand);
                return ExitResult::success().with_stdout(markdown.into_bytes());
            }
        }
        ExitResult::bail(format!("Subcommand `{}` not found", self.sub_cmd))
    }
}

// ===================== Generate Markdown Docs =====================

fn cmd_markdown_help(cmd: &Command) -> String {
    let header = cmd_header_markdown(cmd);
    let content = cmd_content_markdown(cmd, vec!["buck2".to_owned()]).unwrap();
    format!("{header}\n{content}")
}

fn cmd_header_markdown(cmd: &clap::Command) -> String {
    let name = cmd.get_name();
    format!(
        "\
# {name}

This document provides an overview of the commands and options available under `buck2 {name}`.

"
    )
}

fn cmd_content_markdown(
    cmd: &clap::Command,
    mut path: Vec<String>,
) -> Result<String, std::fmt::Error> {
    let prefix = path.join(" ");
    let mut template = String::new();

    let about = cmd.get_long_about().or_else(|| cmd.get_about());
    let about = about
        .map(|about| escape_angle_brackets_for_mdx(&about.to_string()))
        .unwrap_or("".to_owned());

    // header
    writeln!(
        template,
        "\
## `{prefix} {{name}}`

{about}

**Usage**: `{prefix} {{usage}}`
"
    )?;

    // subcommands
    let subcommands = cmd.get_subcommands().collect::<Vec<_>>();
    if !subcommands.is_empty() {
        writeln!(template, "### Subcommands:").unwrap();
        for subcommand in subcommands {
            let name = subcommand.get_name();
            let about = subcommand.get_about();
            write!(template, "* `{name}`").unwrap();
            if let Some(about) = about {
                write!(template, ": {about}").unwrap();
            }
            writeln!(template).unwrap();
        }
        writeln!(template).unwrap();
    }

    // arguments
    let pos = cmd.get_positionals().collect::<Vec<_>>();
    if !pos.is_empty() {
        writeln!(template, "### Arguments:").unwrap();
        for arg in pos {
            write_arg_markdown(&mut template, arg).unwrap();
        }
        writeln!(template).unwrap();
    }

    // options
    let options = cmd
        .get_arguments()
        .filter(|arg| !arg.is_positional() && !arg.is_hide_set())
        .collect::<Vec<_>>();

    if !options.is_empty() {
        writeln!(template, "### Options:").unwrap();
        for arg in options {
            write_arg_markdown(&mut template, arg).unwrap();
        }
        writeln!(template).unwrap();
    }

    let mut markdown = cmd
        .clone()
        .help_template(&template)
        .render_long_help()
        .to_string();

    let name = cmd.get_name();
    path.push(name.to_owned());

    // recursively generate subcommand docs
    for subcommand in cmd.get_subcommands() {
        let sub_content = cmd_content_markdown(subcommand, path.clone())?;
        markdown.push_str(&sub_content);
    }

    markdown.push_str("\n\n");

    Ok(markdown)
}

fn write_arg_markdown(buffer: &mut String, arg: &clap::Arg) -> std::fmt::Result {
    //--------------------
    // Cmd arg or option
    // e.g. * `--opt`
    //--------------------
    write!(buffer, "* ")?;

    let value_name = arg
        .get_value_names()
        .map(|val| val.join(" "))
        .unwrap_or(arg.get_id().to_string().to_ascii_uppercase());

    let opt_prefix = match (arg.get_short(), arg.get_long()) {
        (Some(short), Some(long)) => {
            format!("-{short}, --{long}")
        }
        (Some(short), None) => {
            format!("-{short}")
        }
        (None, Some(long)) => {
            format!("--{long}")
        }
        (None, None) => "".to_owned(),
    };

    if opt_prefix.is_empty() {
        write!(buffer, "`<{value_name}>`")?;
    } else if arg.get_action().takes_values() {
        write!(buffer, "`{opt_prefix} <{value_name}>`")?;
    } else {
        write!(buffer, "`{opt_prefix}`")?;
    }

    //--------------------
    // Cmd flag aliases
    //--------------------

    if let Some(aliases) = arg.get_visible_aliases().as_deref() {
        if !aliases.is_empty() {
            let aliases_str = aliases
                .iter()
                .map(|a| format!("`--{a}`"))
                .collect::<Vec<String>>()
                .join(", ");
            write!(buffer, " (alias: {aliases_str})")?;
        }
    }

    writeln!(buffer)?;

    let indent = "    ";

    //--------------------
    // Help message
    //--------------------

    let help = arg.get_long_help().or(arg.get_help());
    if let Some(help) = help {
        // for each line of help, indent it by 4 spaces
        let content = help
            .to_string()
            .lines()
            .map(|line| format!("{indent}{line}"))
            .collect::<Vec<_>>()
            .join("\n");

        let content = escape_angle_brackets_for_mdx(&content);
        writeln!(buffer, "{content}")?;
    }

    writeln!(buffer)?;

    //--------------------
    // Arg default values
    //--------------------

    if !arg.get_default_values().is_empty() {
        let default_values: String = arg
            .get_default_values()
            .iter()
            .map(|value| format!("`{}`", value.to_string_lossy()))
            .collect::<Vec<String>>()
            .join(", ");

        if arg.get_default_values().len() > 1 {
            // Plural
            writeln!(buffer, "{indent}* Default values: {default_values}")?;
        } else {
            // Singular
            writeln!(buffer, "{indent}* Default value: {default_values}")?;
        }
    }

    //--------------------
    // Arg possible values
    //--------------------

    let possible_values: Vec<PossibleValue> = arg
        .get_possible_values()
        .into_iter()
        .filter(|pv| !pv.is_hide_set())
        .collect();

    // Print possible values for options that take a value, but not for flags
    // that can only be either present or absent and do not take a value.
    if !possible_values.is_empty() && !matches!(arg.get_action(), clap::ArgAction::SetTrue) {
        let possible_values_content = possible_values
            .iter()
            .map(|pv| {
                let name = pv.get_name();
                if let Some(help) = pv.get_help() {
                    format!("{indent}{indent}* `{name}`: {help}")
                } else {
                    format!("{indent}{indent}* `{name}`")
                }
            })
            .collect::<Vec<String>>()
            .join("\n");

        writeln!(
            buffer,
            "{indent}* Possible values:\n{possible_values_content}"
        )?;
    }

    Ok(())
}

/// Escapes angle brackets in the given string to avoid breaking the MDX parser.
/// Specifically, it replaces all `<` with `&lt;` and all `>` with `&gt;`.
fn escape_angle_brackets_for_mdx(input: &str) -> String {
    input.replace('<', "&lt;").replace('>', "&gt;")
}
