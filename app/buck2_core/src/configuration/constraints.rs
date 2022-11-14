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
use gazebo::prelude::*;

use crate::target::TargetLabel;

/// A ConstraintKey is a label for a `constraint_setting()` target.
#[derive(
    Clone, Dupe, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative
)]
pub struct ConstraintKey(pub TargetLabel);

#[derive(
    Clone, Dupe, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative
)]
pub struct ConstraintValue(pub TargetLabel);
