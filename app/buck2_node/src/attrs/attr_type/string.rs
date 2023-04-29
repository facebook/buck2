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
use std::ops::Deref;

use allocative::Allocative;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use serde::Serialize;

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct StringAttrType;

#[derive(
    Default, Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Serialize
)]
#[serde(transparent)]
pub struct StringLiteral(pub ArcStr);

impl Deref for StringLiteral {
    type Target = ArcStr;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            f.write_str(self.0.as_str())
        } else {
            write!(f, "\"{}\"", self.0)
        }
    }
}
