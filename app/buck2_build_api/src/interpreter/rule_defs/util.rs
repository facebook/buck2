/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

// Returns a function that produces commas every time apart from the first
pub fn commas() -> impl FnMut(&mut fmt::Formatter<'_>) -> fmt::Result {
    let mut with_comma = false;
    move |f: &mut fmt::Formatter<'_>| -> fmt::Result {
        if with_comma {
            write!(f, ", ")?;
        }
        with_comma = true;
        Ok(())
    }
}
