/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;

use async_trait::async_trait;
use buck2_client_ctx::common::CommonCommandOptions;
use dupe::Dupe;

use crate::AuditSubcommand;

#[derive(
    Debug,
    Dupe,
    Clone,
    Copy,
    serde::Serialize,
    serde::Deserialize,
    clap::ArgEnum
)]
#[clap(rename_all = "snake_case")]
pub enum OutputFormat {
    Simple,
    Json,
}

#[derive(Debug, Clone, Copy, Dupe, serde::Serialize, serde::Deserialize)]
pub enum LocationStyle {
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
pub enum ValueStyle {
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

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(name = "audit-config", about = "buck audit config")]
pub struct AuditConfigCommand {
    #[clap(flatten)]
    common_opts: CommonCommandOptions,

    #[clap(long = "cell")]
    pub cell: Option<String>,

    #[clap(long, alias = "style", ignore_case = true, arg_enum)]
    pub output_format: Option<OutputFormat>,

    #[clap(long)]
    pub json: bool,

    #[clap(long = "location", default_value = "none", possible_values=&["none", "direct", "extended"])]
    pub location_style: LocationStyle,

    #[clap(long = "value", default_value = "resolved", possible_values=&["resolved", "raw", "both"])]
    pub value_style: ValueStyle,

    #[clap(
        name = "SPECS",
        help = "config section/key specs of the form `section` or `section.key`. If any specs are provided, only values matching a spec will be printed (section headers will be printed only for sections with a key matching the spec)."
    )]
    pub specs: Vec<String>,
}

impl AuditConfigCommand {
    pub fn output_format(&self) -> OutputFormat {
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
    fn common_opts(&self) -> &CommonCommandOptions {
        &self.common_opts
    }
}
