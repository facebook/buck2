/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use dupe::Dupe;

#[derive(Debug, Clone, Dupe, allocative::Allocative, PartialEq, Eq)]
pub enum ExternalCellOrigin {
    Bundled,
}

impl fmt::Display for ExternalCellOrigin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bundled => write!(f, "bundled"),
        }
    }
}

impl ExternalCellOrigin {
    pub fn parse_from_config_value(value: &str) -> anyhow::Result<Self> {
        #[derive(buck2_error::Error, Debug)]
        enum ExternalCellOriginParseError {
            #[error("Unknown external cell origin `{0}`")]
            Unknown(String),
        }
        if value == "bundled" {
            Ok(ExternalCellOrigin::Bundled)
        } else {
            Err(ExternalCellOriginParseError::Unknown(value.to_owned()).into())
        }
    }
}
