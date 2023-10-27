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
mod context_value;
mod error;
mod format;
mod root;
pub mod shared_result;

pub use any::AnyError;
pub use context::Context;
/// A piece of metadata to indicate whether this error is an infra or user error.
///
/// You can attach this to an error by passing it to the [`Error::context`] method. Alternatively,
/// you can call `.user()` or `.infra()` on a [`buck2_error::Result`][`Result`].
///
/// The category is fundamentally closed - the expectation is that it will not grow new variants in
/// the future.
#[doc(inline)]
pub use context_value::Category;
pub use error::Error;
pub use root::UniqueRootId;

pub type Result<T> = std::result::Result<T, crate::Error>;

/// The type of the error that is being produced.
///
/// The type of the error approximately indicates where the error came from. It is useful when you
/// want to track a particular error scenario in more detail.
///
/// The error type is not a piece of context - it can only be set when creating the error, not at
/// some later point.
///
/// Unlike the [`category`](crate::Category) metadata, this type is "open" in the sense that it is
/// expected to grow in the future. You should not match on it exhaustively.
pub use buck2_data::ErrorType;
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
