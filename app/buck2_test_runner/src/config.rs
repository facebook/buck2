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
    #[clap(long, default_value = "600", parse(try_from_str=try_parse_timeout_from_str))]
    pub timeout: Duration,

    #[clap(flatten)]
    ignored_args: IgnoredArgs,
}

/// Ignored args included for backwards compatibility.
#[derive(Debug, Parser)]
struct IgnoredArgs {
    #[clap(long, hidden = true)]
    buck_test_info: String,
}

/// Uiltity that can be used to parse Env values from CLI arguments.
#[derive(Debug, PartialEq)]
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
    type Err = EnvValueParseError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input.split_once('=') {
            Some((key, value)) => Ok(EnvValue::new(key, value)),
            None => Err(EnvValueParseError::IncorrectSyntax(input.to_owned())),
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum EnvValueParseError {
    #[error("Incorrect syntax for env value. Please use name=value. Input: `{0}`")]
    IncorrectSyntax(String),
}

fn try_parse_timeout_from_str(input: &str) -> anyhow::Result<Duration> {
    let seconds = input.parse().context("Could not parse provided timeout")?;
    Ok(Duration::from_secs(seconds))
}
