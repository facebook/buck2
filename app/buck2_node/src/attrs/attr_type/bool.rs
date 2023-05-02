/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use dupe::Dupe;
use serde::Serialize;

use crate::attrs::attr_type::any_matches::AnyMatches;

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct BoolAttrType;

#[derive(Debug, Clone, Copy, Dupe, Eq, PartialEq, Hash, Allocative, Serialize)]
#[serde(transparent)]
pub struct BoolLiteral(pub bool);

impl Display for BoolLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0 {
            write!(f, "True")
        } else {
            write!(f, "False")
        }
    }
}

impl AnyMatches for BoolLiteral {
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        filter(if self.0 { "True" } else { "False" })
    }
}
