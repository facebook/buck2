/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use dupe::Dupe;
use pagable::Pagable;
use serde::Deserialize;
use serde::Serialize;
use strong_hash::StrongHash;

use crate::attrs::attr_type::any_matches::AnyMatches;

#[derive(Debug, Eq, PartialEq, Hash, Pagable, Allocative, Clone, Copy, Dupe)]
pub struct BoolAttrType;

#[derive(
    Debug,
    Clone,
    Copy,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    StrongHash,
    Allocative,
    Serialize,
    Deserialize,
    Pagable
)]
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
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        Ok(filter(if self.0 { "True" } else { "False" })?
            || filter(if self.0 { "true" } else { "false" })?)
    }
}
