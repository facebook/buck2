/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Arguments;

#[doc(hidden)]
#[cold]
#[track_caller]
pub fn internal_error_impl(args: Arguments) -> anyhow::Error {
    let anyhow_error = anyhow::anyhow!("{} (internal error)", args);
    let error = crate::Error::from(anyhow_error).tag([crate::ErrorTag::InternalError]);
    anyhow::Error::from(error)
}

/// Indicates a bug in buck2.
#[macro_export]
macro_rules! internal_error {
    ($format:expr) => {
        internal_error!($format,)
    };
    ($format:expr , $($arg:tt)*) => {
        $crate::macros::internal_error_impl(format_args!($format, $($arg)*))
    };
}
