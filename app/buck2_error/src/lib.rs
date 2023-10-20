/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(trait_alias)]
#![feature(trait_upcasting)]

mod any;
mod context;
mod error;
mod format;
mod root;
pub mod shared_result;

pub use any::AnyError;
pub use context::Context;
pub use error::Error;
pub use root::UniqueRootId;

pub type Result<T> = std::result::Result<T, crate::Error>;

/// Generates an error impl for the type.
///
/// This macro is a drop-in replacement for [`thiserror::Error`]. In the near future, all uses of
/// `thiserror` in `buck2/app` will be replaced with this macro.
///
/// Currently, the only distinction from `thiserror::Error` is that an additional impl of
/// [`AnyError`] is generated for the type, which makes some of the interactions with `buck2_error` more
/// ergonomic. In the future, this macro will also be used to be able to annotate errors with
/// additional structured context information.
///
/// ## Example
///
/// ```rust
/// #[derive(Debug, buck2_error::Error)]
/// #[error("My error type")]
/// struct MyError;
///
/// let e = buck2_error::Error::from(MyError);
/// assert_eq!(&format!("{}", e), "My error type");
/// ```
#[doc(inline)]
pub use buck2_error_derive::ErrorForReexport as Error;

#[doc(hidden)]
pub mod __for_macro {
    pub use buck2_error_derive::exterminate;
    pub use thiserror;
}
