/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use allocative::Allocative;
use dupe::Dupe;

pub trait DirectoryDigest:
    Allocative + PartialEq + Eq + Hash + Clone + Dupe + Debug + Display
{
}

/// Indicates that this type of digest is suitable for use for interning.
///
/// Specifically, this is not implemented for `NoDigest`, as that returns the same `()` digest for
/// all directories.
pub trait InternableDirectoryDigest: DirectoryDigest {}
