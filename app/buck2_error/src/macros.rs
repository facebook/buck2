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
pub fn buck2_error_impl(tags: &[crate::ErrorTag], args: Arguments) -> anyhow::Error {
    let anyhow_error = anyhow::anyhow!("{args}");
    let error = crate::Error::from(anyhow_error).tag(tags.iter().copied());
    anyhow::Error::from(error)
}

#[doc(hidden)]
#[cold]
#[track_caller]
pub fn internal_error_impl(args: Arguments) -> anyhow::Error {
    buck2_error_impl(
        &[crate::ErrorTag::InternalError],
        format_args!("{args} (internal error)"),
    )
}

#[macro_export]
macro_rules! buck2_error {
    ($tags:expr, $format:expr) => {
        buck2_error!($tags, $format,)
    };
    ($tags:expr, $format:expr, $($arg:tt)*) => {
        $crate::macros::buck2_error_impl(&$tags, format_args!($format, $($arg)*))
    };
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
