/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use fancy_regex::Regex;
use starlark::environment::GlobalsBuilder;

#[starlark_module]
pub fn register_regex(builder: &mut GlobalsBuilder) {
    /// Test if a regular expression matches a string. Fails if the regular expression
    /// is malformed.
    ///
    /// As an example:
    ///
    /// ```python
    /// regex_match("^[a-z]*$", "hello") == True
    /// regex_match("^[a-z]*$", "1234") == False
    /// ```
    fn regex_match(regex: &str, str: &str) -> anyhow::Result<bool> {
        let re = Regex::new(regex)?;
        Ok(re.is_match(str)?)
    }
}
