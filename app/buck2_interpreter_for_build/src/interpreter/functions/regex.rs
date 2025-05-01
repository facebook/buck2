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
use starlark::starlark_module;

// TODO(nga): drop it, and only use `regex` function.
#[starlark_module]
pub(crate) fn register_regex(builder: &mut GlobalsBuilder) {
    /// Test if a regular expression matches a string. Fails if the regular expression
    /// is malformed.
    ///
    /// As an example:
    ///
    /// ```python
    /// regex_match("^[a-z]*$", "hello") == True
    /// regex_match("^[a-z]*$", "1234") == False
    /// ```
    fn regex_match(
        #[starlark(require = pos)] regex: &str,
        #[starlark(require = pos)] str: &str,
    ) -> starlark::Result<bool> {
        let re = Regex::new(regex).map_err(buck2_error::Error::from)?;
        Ok(re.is_match(str).map_err(buck2_error::Error::from)?)
    }
}

#[cfg(test)]
mod tests {
    use starlark::assert::Assert;

    use crate::interpreter::functions::regex::register_regex;

    #[test]
    fn test_regex() {
        let mut a = Assert::new();
        a.globals_add(register_regex);
        a.eq("regex_match('abc|def|ghi', 'abc')", "True");
        a.eq("regex_match('abc|def|ghi', 'xyz')", "False");
        a.eq("regex_match('^((?!abc).)*$', 'abc')", "False");
        a.eq("regex_match('^((?!abc).)*$', 'xyz')", "True");
    }
}
