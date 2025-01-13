/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::values::FreezeError;

// Adding conversion under 'buck2_interpreter' instead of 'buck2_error' to avoid
// starlark dependency in 'buck2_error' crate. Any crate that requires a freeze error
// conversion currently depends on 'buck2_interpreter' (And will likely be the case in the future too)
#[cold]
pub fn from_freeze_error(e: FreezeError) -> buck2_error::Error {
    let mut base = buck2_error::buck2_error!(buck2_error::ErrorTag::StarlarkError, "{}", e.err_msg);
    for err in e.contexts {
        base = base.context(err);
    }

    base
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_freeze_error() {
        let freeze_error = FreezeError::new("base error".to_owned());
        let freeze_error = freeze_error.context("context 1");
        let freeze_error = freeze_error.context("context 2");

        let buck2_error = from_freeze_error(freeze_error);
        let actual = buck2_error.get_stack_for_debug();

        let expected = "CONTEXT: context 2\nCONTEXT: context 1\nCONTEXT: [StarlarkError]\nROOT:\n\"base error\"\n";
        assert_eq!(expected, actual);
    }
}
