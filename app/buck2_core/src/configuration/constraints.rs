/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Constraints are the building block of buck platforms.
//!
//! A constraint is identified by a "constraint key" defined by a `constraint_setting()`
//! target. There may be multiple possible values for the constraint, each defined by a
//! `constraint_value()` target.
//!
//! Currently, a platform or configuration has at most a single value for each constraint.

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;
use strong_hash::StrongHash;

use crate::target::label::label::TargetLabel;

/// A ConstraintKey is a label for a `constraint_setting()` target.
#[derive(
    Clone, Dupe, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative, StrongHash
)]
pub struct ConstraintKey(pub TargetLabel);

impl ConstraintKey {
    pub fn testing_new(label: &str) -> ConstraintKey {
        ConstraintKey(TargetLabel::testing_parse(label))
    }
}

#[derive(
    Clone, Dupe, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative, StrongHash
)]
pub struct ConstraintValue(pub TargetLabel);

impl ConstraintValue {
    pub fn testing_new(label: &str) -> ConstraintValue {
        ConstraintValue(TargetLabel::testing_parse(label))
    }
}
