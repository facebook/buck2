/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::{self};
use std::str::FromStr;

use anyhow::Context;
use regex::Regex;

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub struct TDigest {
    pub hash: String,
    pub size_in_bytes: i64,
    pub _dot_dot: (),
}

impl Display for TDigest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.hash, self.size_in_bytes)
    }
}

impl FromStr for TDigest {
    type Err = anyhow::Error;

    fn from_str(digest: &str) -> anyhow::Result<TDigest> {
        let re = Regex::new("([0-9a-f]+):([0-9]+)").context("Failed to compile digest regex")?;
        let matches = re
            .captures(digest)
            .with_context(|| format!("Digest format not valid: {}", digest))?;
        Ok(TDigest {
            hash: matches[1].to_string(),
            size_in_bytes: matches[2].parse::<i64>().with_context(|| {
                format!("Digest size {} could not be parsed as a i64", &matches[2])
            })?,
            ..Default::default()
        })
    }
}
