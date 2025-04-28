/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;
use std::time::Duration;

use anyhow::Context;
use clap::Parser;

#[derive(Debug, Parser)]
pub struct Config {
    /// add a list of environment variables using format: --env VAR1=Value1 VAR2='Value 2'
    #[clap(long)]
    pub env: Vec<EnvValue>,

    /// Max number of seconds allowed to run a test.
    #[clap(long, default_value = "600", value_parser = try_parse_timeout_from_str)]
    pub timeout: Duration,

    /// Ignored arg included for backwards compatibility.
    #[clap(long, hide = true)]
    buck_test_info: String,

    /// Passthrough argments to test binary.
    /// Available as a workaround for when test features are available.
    #[clap(long, num_args=1.., allow_hyphen_values = true)]
    pub test_arg: Vec<String>,
}

/// Uiltity that can be used to parse Env values from CLI arguments.
#[derive(Debug, PartialEq, Clone)]
pub struct EnvValue {
    pub name: String,
    pub value: String,
}

impl EnvValue {
    pub fn new(name: &str, value: &str) -> EnvValue {
        EnvValue {
            name: name.to_owned(),
            value: value.to_owned(),
        }
    }
}

impl FromStr for EnvValue {
    type Err = anyhow::Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input.split_once('=') {
            Some((key, value)) => Ok(EnvValue::new(key, value)),
            None => Err(
                buck2_error::Error::from(EnvValueParseError::IncorrectSyntax(input.to_owned()))
                    .into(),
            ),
        }
    }
}

#[derive(Debug, buck2_error::Error, PartialEq)]
#[buck2(tag = Input)]
pub enum EnvValueParseError {
    #[error("Incorrect syntax for env value. Please use name=value. Input: `{0}`")]
    IncorrectSyntax(String),
}

fn try_parse_timeout_from_str(input: &str) -> anyhow::Result<Duration> {
    let seconds = input.parse().context("Could not parse provided timeout")?;
    Ok(Duration::from_secs(seconds))
}
