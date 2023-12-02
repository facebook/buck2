/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(let_chains)]
#![feature(trait_alias)]
#![feature(trait_upcasting)]

mod any;
mod context;
mod context_value;
mod derive_tests;
mod error;
mod format;
mod root;
mod source_location;

use std::error::request_value;
use std::error::Error as StdError;
use std::error::Request;

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
pub use error::DynLateFormat;
pub use error::Error;
pub use root::UniqueRootId;

pub type Result<T> = std::result::Result<T, crate::Error>;

/// See the documentation in the `error.proto` file for details.
pub use buck2_data::error::ErrorTag;
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
pub use buck2_data::error::ErrorType;
/// Generates an error impl for the type.
///
/// This macro is a drop-in replacement for [`thiserror::Error`]. In the near future, all uses of
/// `thiserror` in `buck2/app` will be replaced with this macro.
///
/// Currently, the only distinction from `thiserror::Error` is that an additional impl of
/// `AnyError` is generated for the type, which makes some of the interactions with `buck2_error` more
/// ergonomic. In the future, this macro will also be used to be able to annotate errors with
/// additional structured context information.
///
/// ## Example
///
/// ```rust
/// # #![feature(error_generic_member_access)]
/// #[derive(Debug, buck2_error::Error)]
/// #[error("My error type")]
/// struct MyError;
///
/// let e = buck2_error::Error::from(MyError);
/// assert_eq!(&format!("{}", e), "My error type");
/// ```
#[doc(inline)]
pub use buck2_error_derive::Error;

use crate::any::ProvidableContextMetadata;
use crate::any::ProvidableRootMetadata;

/// Provide metadata about an error.
///
/// This is a manual alternative to deriving `buck2_error::Error`, which should be preferred if at
/// all possible. This function has a pretty strict contract: You must call it within the `provide`
/// implementation for an error type `E`, and must pass `E` as the type parameter.
///
/// If the `typ` argument is provided, then this metadata is treated as "root-like." That means that
/// this error will be treated as the error root and errors furthere down in the `.source()` chain
/// will not be checked for context. However they will still be printed as a part of the `Display`
/// and `Debug` impls on `buck2_error::Error`.
///
/// The `source_file` should just be `std::file!()`; the `source_location_extra` should be the type
/// - and possibly variant - name, formatted as either `Type` or `Type::Variant`.
pub fn provide_metadata<'a, 'b, E: StdError + Send + Sync + 'static>(
    request: &'b mut Request<'a>,
    category: Option<crate::Category>,
    typ: Option<crate::ErrorType>,
    tags: &[Option<crate::ErrorTag>],
    source_file: &'static str,
    source_location_extra: Option<&'static str>,
    action_error: Option<buck2_data::ActionError>,
) {
    let check_error_type = ProvidableRootMetadata::gen_check_error_type::<E>();
    if typ.is_some() {
        let metadata = ProvidableRootMetadata {
            typ,
            check_error_type,
            action_error,
        };
        Request::provide_value(request, metadata);
    }
    let metadata = ProvidableContextMetadata {
        category,
        tags: tags.iter().copied().flatten().collect(),
        source_file,
        source_location_extra,
        check_error_type,
    };
    Request::provide_value(request, metadata);
}

/// Forwards the metadata provided by another error.
///
/// This method is intended for cases where your error type contains another error and that error is
/// not reported as a `source`. There's no need to call this with sub-errors that are reported as
/// sources.
pub fn forward_provide<'a, 'b, E: StdError + Send + Sync + 'static>(
    request: &'b mut Request<'a>,
    other: &dyn StdError,
) {
    // Unfortunately, this implementation can't be trivial. The problem is that the conversion logic
    // expects the `CheckErrorType` value to be generated based on a type which actually matches the
    // type of the error reported in the source chain. So we have to replace the `CheckErrorType`
    // value with the proper one.
    let check_error_type = ProvidableRootMetadata::gen_check_error_type::<E>();
    if let Some(mut m) = request_value::<ProvidableRootMetadata>(other) {
        m.check_error_type = check_error_type;
        Request::provide_value(request, m);
    }
    if let Some(mut m) = request_value::<ProvidableContextMetadata>(other) {
        m.check_error_type = check_error_type;
        Request::provide_value(request, m);
    }
}

#[doc(hidden)]
pub mod __for_macro {
    use std::error::Error as StdError;

    pub use anyhow;
    pub use thiserror;

    pub trait AsDynError {
        fn as_dyn_error<'a>(&'a self) -> &'a (dyn StdError + 'static);
    }

    impl AsDynError for dyn StdError + Sync + Send + 'static {
        fn as_dyn_error<'a>(&'a self) -> &'a (dyn StdError + 'static) {
            self
        }
    }

    impl<T: StdError + 'static> AsDynError for T {
        fn as_dyn_error<'a>(&'a self) -> &'a (dyn StdError + 'static) {
            self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate as buck2_error;

    #[test]
    fn test_forward_provide() {
        #[derive(buck2_error_derive::Error, Debug)]
        #[error("unused")]
        struct Inner();

        #[derive(Debug, derive_more::Display)]
        struct Outer(Inner);

        impl StdError for Outer {
            fn provide<'a, 'b>(&self, request: &'b mut Request<'a>) {
                buck2_error::forward_provide::<Self>(request, &self.0);
            }
        }

        let err = Outer(Inner());
        let err = buck2_error::Error::from(err);
        // This works because we used `forward_provide`, it would not work if we had just called
        // `.provide()`
        assert_eq!(err.source_location(), Some("buck2_error/src/lib.rs::Inner"));
    }
}
