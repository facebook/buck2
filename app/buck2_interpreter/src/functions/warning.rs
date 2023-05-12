/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;
use starlark::values::none::NoneType;

#[starlark_module]
pub fn register_warning(builder: &mut GlobalsBuilder) {
    /// Print a warning. The line will be decorated with the timestamp and other details,
    /// including the word `WARN` (colored, if the console supports it).
    ///
    /// If you are not writing a warning, use `print` instead. Be aware that printing
    /// lots of output (warnings or not) can be cause all information to be ignored by the user.
    fn warning(#[starlark(require = pos)] x: &str) -> anyhow::Result<NoneType> {
        tracing::warn!("{}", x);
        Ok(NoneType)
    }
}
