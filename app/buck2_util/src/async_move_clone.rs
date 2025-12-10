/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/// Convenience macro to clone captures for an async move block.
///
/// Example:
///
/// ```rust
/// let s = String::new();
/// let f = buck2_util::async_move_clone!(s, {
///     drop(s);
/// });
/// drop(s);
/// ```
pub macro async_move_clone {
    ($($f:ident,)* { $($tt:tt)* }) => {{
        $(
        let $f = std::clone::Clone::clone(&$f);
        )*
        async move { $($tt)* }
    }}
}
