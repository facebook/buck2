/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Format related utilities of the core path types

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

/// formats the path as a quoted string
pub fn quoted_display<D>(d: &D, f: &mut Formatter) -> fmt::Result
where
    D: Display + ?Sized,
{
    write!(f, "\"{d:}\"")
}
