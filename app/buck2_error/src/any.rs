/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Integrations of `buck2_error::Error` with `anyhow::Error` and `StdError`.

use std::any::request_value;
use std::error::Error as StdError;
use std::fmt;
use std::sync::Arc;

use mappable_rc::Marc;
use ref_cast::RefCast;

use crate::error::ErrorKind;
use crate::root::ErrorRoot;

// This implementation is fairly magic and is what allows us to bypass the issue with conflicting
// implementations between `anyhow::Error` and `T: StdError`. The `T: Into<anyhow::Error>` bound is
// what we actually make use of in the implementation, while the other bound is needed to make sure
// this impl does not accidentally cover too many types. Importantly, this impl does not conflict
// with `T: From<T>`
impl<T: fmt::Debug + fmt::Display + Sync + Send + 'static> From<T> for crate::Error
where
    T: Into<anyhow::Error>,
    Result<(), T>: anyhow::Context<(), T>,
{
    #[track_caller]
    fn from(value: T) -> crate::Error {
        let source_location =
            crate::source_location::from_file(std::panic::Location::caller().file(), None);
        // `Self` may be an `anyhow::Error` or any `StdError`. We'll check by downcasting
        let mut e = Some(value);
        let r: &mut dyn std::any::Any = &mut e;
        if let Some(e) = r.downcast_mut::<Option<anyhow::Error>>() {
            return recover_crate_error(Marc::new(e.take().unwrap()), source_location);
        }

        // Otherwise, we'll use the strategy for `StdError`
        let anyhow = e.unwrap().into();
        recover_crate_error(Marc::new(anyhow), source_location)
    }
}

fn construct_root_from_recovered_error(
    e: Marc<dyn StdError + Send + Sync + 'static>,
    typ: Option<crate::ErrorType>,
    context: Option<ProvidableContextMetadata>,
) -> crate::Error {
    let source_location = if let Some(context) = context {
        crate::source_location::from_file(context.source_file, context.source_location_extra)
    } else {
        None
    };
    let e = crate::Error(Arc::new(ErrorKind::Root(ErrorRoot::new(
        e,
        None,
        typ,
        source_location,
    ))));
    if let Some(context) = context && let Some(category) = context.category {
        e.context(category)
    } else {
        e
    }
}

pub(crate) fn recover_crate_error(
    value: Marc<anyhow::Error>,
    source_location: Option<String>,
) -> crate::Error {
    // Instead of just turning this into an error root, we will first check if this error has any
    // information associated with it that would allow us to recover more structure.
    let mut context_stack = Vec::new();
    let mut cur: Marc<dyn StdError + 'static> = Marc::map(value.clone(), AsRef::as_ref);
    let base = 'base: loop {
        // Handle the `cur` error
        if let Some(base) = cur.downcast_ref::<CrateAsStdError>() {
            break base.0.clone();
        }

        let context_metadata = if let Some(metadata) = request_value::<ProvidableContextMetadata>(&*cur) && (metadata.check_error_type)(&*cur).is_some() {
            Some(metadata)
        } else {
            None
        };

        if let Some(metadata) = request_value::<ProvidableRootMetadata>(&*cur) && (metadata.check_error_type)(&*cur).is_some() {
            // FIXME(JakobDegen): `Marc` needs `try_map` here too
            let cur = Marc::map(cur, |e| (metadata.check_error_type)(e).unwrap());
            break construct_root_from_recovered_error(cur, metadata.typ, context_metadata);
        }

        context_stack.push((cur.clone(), context_metadata));

        // Compute the next element in the source chain
        if cur.source().is_some() {
            // FIXME(JakobDegen): `Marc` should have `try_map` or some such so that we don't need to
            // unwrap
            cur = Marc::map(cur, |e| e.source().unwrap());
            continue;
        }

        // The error was not created directly from a `buck2_error::Error` or with a
        // `ProvidableRootMetadata`. However, if may have only `ProvidableContextMetadata`, so
        // check for that possibility
        while let Some((e, context_metadata)) = context_stack.pop() {
            let Some(context_metadata) = context_metadata else {
                continue;
            };
            // The `unwrap` is ok because we checked this condition when initially constructing the `context_metadata`
            let e = Marc::map(e, |e| (context_metadata.check_error_type)(e).unwrap());
            break 'base construct_root_from_recovered_error(e, None, Some(context_metadata));
        }
        // This error was not created with any useful metadata on it, so there's nothing smart we can do
        return crate::Error(Arc::new(ErrorKind::Root(ErrorRoot::new_anyhow(
            value,
            source_location,
        ))));
    };
    // We were able to convert the error into a `buck2_error::Error` in some non-trivial way. We'll
    // now need to add back any context that is not included in the `base` buck2_error yet.
    // Unfortunately, we cannot detect whether the remainder of the error chain is actually
    // associated with `anyhow::Context::context` calls or not. If it is, this will all work
    // correctly. If not, we might get some whacky formatting. However, in order for this to go
    // wrong, someone else has to have put an `anyhow::Error` into their custom error type, which
    // they really shouldn't be doing anyway.
    let mut e = base;
    for (context_value, context_metadata) in context_stack.into_iter().rev() {
        if let Some(context_metadata) = context_metadata {
            // The `unwrap` is ok because we checked this condition when initially constructing the `context_metadata`
            let context_value = Marc::map(context_value, |e| {
                (context_metadata.check_error_type)(e).unwrap()
            });
            e = e.context(context_value);
            if let Some(category) = context_metadata.category {
                e = e.context(category);
            }
        } else {
            // The context that shows up here is restricted to that which was added by
            // `anyhow::Context` or non-`buck2_error::Error` impls of `StdError`, including from
            // `thiserror`. The only way we can get access to this context in any way is via the
            // source chain - however the source chain only gives us a `dyn StdError`, without `Send
            // + Sync`. As a result, we cannot store the context in a "typed" way, and must resort
            //   to doing this.
            let context = format!("{}", context_value);
            e = e.context(context);
        }
    }
    e
}

impl From<crate::Error> for anyhow::Error {
    fn from(value: crate::Error) -> Self {
        Into::into(CrateAsStdError(value))
    }
}

#[derive(derive_more::Display, RefCast)]
#[repr(transparent)]
pub(crate) struct CrateAsStdError(pub(crate) crate::Error);

impl fmt::Debug for CrateAsStdError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl StdError for CrateAsStdError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match &*self.0.0 {
            ErrorKind::Root(r) => r.source(),
            ErrorKind::WithContext(_, r) | ErrorKind::Emitted(r) => {
                Some(CrateAsStdError::ref_cast(r))
            }
        }
    }
}

pub type CheckErrorType =
    for<'a> fn(&'a (dyn StdError + 'static)) -> Option<&'a (dyn StdError + Send + Sync + 'static)>;

/// This can be `provide`d by an error to inject buck2-specific information about it.
///
/// Currently intended for macro use only, might make sense to allow it more generally in the
/// future.
#[derive(Copy, Clone)]
pub struct ProvidableRootMetadata {
    pub typ: Option<crate::ErrorType>,
    /// Some errors will transitively call `Provide` for their sources. That means that even when a
    /// `request_value` call returns `Some`, the value might actually be provided by something
    /// further down the source chain. We work around this issue by calling this function to confirm
    /// that the value was indeed provided by the element of the source chain we're currently
    /// inspecting.
    ///
    /// We also reuse this to get a `Send + Sync` reference to our error, since `source()` does not
    /// give us that.
    pub check_error_type: CheckErrorType,
}

/// Like `ProvidableRootMetadata`, but for "context-like" metadata that can appear on the error more
/// than once.
#[derive(Copy, Clone)]
pub struct ProvidableContextMetadata {
    /// Technically this should be in the `ProvidableRootMetadata`. However, we allow it to appear
    /// multiple times in the context and just pick the last one. There's no benefit to being picky.
    pub source_file: &'static str,
    /// Extra information to add to the end of the source location - typically a type/variant name,
    /// and the same thing as gets passed to `buck2_error::source_location::from_file`.
    pub source_location_extra: Option<&'static str>,
    pub category: Option<crate::Category>,
    /// See `ProvidableRootMetadata`
    pub check_error_type: CheckErrorType,
}

impl ProvidableRootMetadata {
    pub const fn gen_check_error_type<E: StdError + Send + Sync + 'static>() -> CheckErrorType {
        |e| e.downcast_ref::<E>().map(|e| e as _)
    }
}

#[cfg(test)]
mod tests {
    use std::any::Demand;

    use super::*;
    use crate::error::ErrorKind;

    #[derive(Debug, derive_more::Display)]
    struct TestError;

    impl StdError for TestError {}

    fn check_equal(mut a: &crate::Error, mut b: &crate::Error) {
        loop {
            match (&*a.0, &*b.0) {
                (ErrorKind::Root(a), ErrorKind::Root(b)) => {
                    // Avoid comparing vtable pointers
                    assert!(a.test_equal(b));
                    return;
                }
                (
                    ErrorKind::WithContext(a_context, a_inner),
                    ErrorKind::WithContext(b_context, b_inner),
                ) => {
                    a_context.assert_eq(b_context);
                    a = a_inner;
                    b = b_inner;
                }
                (ErrorKind::Emitted(a_inner), ErrorKind::Emitted(b_inner)) => {
                    a = a_inner;
                    b = b_inner;
                }
                (_, _) => {
                    panic!("Left side did not match right: {:?} {:?}", a, b)
                }
            }
        }
    }

    #[test]
    fn test_rountrip_no_context() {
        let e = crate::Error::new(TestError).context("context 1");
        let e2 = crate::Error::from(anyhow::Error::from(e.clone()));
        check_equal(&e, &e2);
    }

    #[test]
    fn test_rountrip_with_context() {
        let e = crate::Error::new(TestError).context("context 1");
        let e2 = crate::Error::from(anyhow::Error::from(e.clone()).context("context 2"));
        let e3 = e.context("context 2");
        check_equal(&e2, &e3);
    }

    #[derive(Debug, derive_more::Display)]
    struct FullMetadataError;

    impl StdError for FullMetadataError {
        fn provide<'a>(&'a self, demand: &mut Demand<'a>) {
            demand
                .provide_value(ProvidableRootMetadata {
                    typ: Some(crate::ErrorType::Watchman),
                    check_error_type: ProvidableRootMetadata::gen_check_error_type::<Self>(),
                })
                .provide_value(ProvidableContextMetadata {
                    source_file: file!(),
                    source_location_extra: Some("FullMetadataError"),
                    category: Some(crate::Category::User),
                    check_error_type: ProvidableRootMetadata::gen_check_error_type::<Self>(),
                });
        }
    }

    #[test]
    fn test_metadata() {
        for e in [
            FullMetadataError.into(),
            crate::Error::new(FullMetadataError),
        ] {
            assert_eq!(e.get_category(), Some(crate::Category::User));
            assert_eq!(e.get_error_type(), Some(crate::ErrorType::Watchman));
            assert_eq!(
                e.source_location(),
                Some("buck2_error/src/any.rs::FullMetadataError")
            );
        }
    }

    #[test]
    fn test_metadata_through_anyhow() {
        let e: anyhow::Error = FullMetadataError.into();
        let e = e.context("anyhow");
        let e: crate::Error = e.into();
        assert_eq!(e.get_category(), Some(crate::Category::User));
        assert!(format!("{:?}", e).contains("anyhow"));
    }

    #[derive(Debug, thiserror::Error)]
    #[error("wrapper")]
    struct WrapperError(#[source] FullMetadataError);

    #[test]
    fn test_metadata_through_wrapper() {
        let e: crate::Error = WrapperError(FullMetadataError).into();
        assert_eq!(e.get_category(), Some(crate::Category::User));
        assert!(format!("{:?}", e).contains("wrapper"));
    }

    #[derive(Debug, buck2_error_derive::Error)]
    #[buck2(infra)]
    #[error("wrapper2")]
    struct FullMetadataContextWrapperError(#[source] FullMetadataError);

    #[test]
    fn test_context_in_wrapper() {
        let e: crate::Error = FullMetadataContextWrapperError(FullMetadataError).into();
        assert_eq!(e.get_category(), Some(crate::Category::Infra));
        assert_eq!(e.get_error_type(), Some(crate::ErrorType::Watchman));
        assert_eq!(
            e.source_location(),
            Some("buck2_error/src/any.rs::FullMetadataError")
        );
        assert!(format!("{:?}", e).contains("wrapper2"));
    }

    #[derive(Debug, buck2_error_derive::Error)]
    #[buck2(user)]
    #[error("unused")]
    struct UserMetadataError;

    #[derive(Debug, buck2_error_derive::Error)]
    #[buck2(infra)]
    #[error("unused")]
    struct InfraMetadataWrapperError(#[from] UserMetadataError);

    #[test]
    fn test_no_root_metadata_context() {
        let e: InfraMetadataWrapperError = UserMetadataError.into();
        let e: crate::Error = e.into();
        assert_eq!(e.get_category(), Some(crate::Category::Infra));
    }
}
