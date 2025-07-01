/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/// An ArgBuilder is almost exactly a CommandLineBuilder. The difference is that while a commandline
/// builder is building a list of strings, argbuilder is appending the values to a single string.
pub trait ArgBuilder {
    /// Add the string representation to the list of command line arguments.
    fn push_str(&mut self, s: &str);
}
