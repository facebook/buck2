/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Integrations of `buck2_error::Error` with `anyhow::Error` and `std::error::Error`.

use std::any::request_value;
use std::fmt;
use std::sync::Arc;

use mappable_rc::Marc;
use ref_cast::RefCast;

use crate::error::ErrorKind;
use crate::root::ErrorRoot;

// This implementation is fairly magic and is what allows us to bypass the issue with conflicting
// implementations between `anyhow::Error` and `T: std::error::Error`. The `T: Into<anyhow::Error>`
// bound is what we actually make use of in the implementation, while the other bound is needed to
// make sure this impl does not accidentally cover too many types. Importantly, this impl does not
// conflict with `T: From<T>`
impl<T: fmt::Debug + fmt::Display + Sync + Send + 'static> From<T> for crate::Error
where
    T: Into<anyhow::Error>,
    Result<(), T>: anyhow::Context<(), T>,
{
    #[track_caller]
    fn from(value: T) -> crate::Error {
        let source_location =
            crate::source_location::from_file(std::panic::Location::caller().file(), None);
        // `Self` may be an `anyhow::Error` or any `std::error::Error`. We'll check by downcasting
        let mut e = Some(value);
        let r: &mut dyn std::any::Any = &mut e;
        if let Some(e) = r.downcast_mut::<Option<anyhow::Error>>() {
            return recover_crate_error(Marc::new(e.take().unwrap()), source_location);
        }

        // Otherwise, we'll use the strategy for `std::error::Error`
        let anyhow = e.unwrap().into();
        recover_crate_error(Marc::new(anyhow), source_location)
    }
}

pub(crate) fn recover_crate_error(
    value: Marc<anyhow::Error>,
    source_location: Option<String>,
) -> crate::Error {
    // Instead of just turning this into an error root, we will first check if this error has any
    // information associated with it that would allow us to recover more structure.
    let mut context_stack = Vec::new();
    let mut cur: Marc<dyn std::error::Error + 'static> = Marc::map(value.clone(), AsRef::as_ref);
    let base = loop {
        // Handle the `cur` error
        if let Some(base) = cur.downcast_ref::<CrateAsStdError>() {
            break base.0.clone();
        } else if let Some(metadata) = request_value::<ProvidableMetadata>(&*cur) && (metadata.check_error_type)(&*cur).is_some() {
            // FIXME(JakobDegen): `Marc` needs `try_map` here too
            let cur = Marc::map(cur, |e| (metadata.check_error_type)(e).unwrap());
            let source_location = crate::source_location::from_file(metadata.source_file, metadata.source_location_extra);
            let mut e = crate::Error(Arc::new(ErrorKind::Root(ErrorRoot::new(
                cur,
                None,
                metadata.typ,
                source_location,
            ))));
            if let Some(category) = metadata.category {
                e = e.context(category);
            }
            break e;
        } else {
            context_stack.push(cur.clone());
        }

        // Compute the next element in the source chain
        if cur.source().is_none() {
            // This error was not created from a `buck2_error::Error`, so we can't do anything
            // smart
            return crate::Error(Arc::new(ErrorKind::Root(ErrorRoot::new_anyhow(
                value,
                source_location,
            ))));
        } else {
            // FIXME(JakobDegen): `Marc` should have `try_map` or some such so that we don't need to
            // unwrap
            cur = Marc::map(cur, |e| e.source().unwrap());
        }
    };
    // We were able to convert the error into a `buck2_error::Error` in some non-trivial way. We'll
    // now need to add back any context that is not included in the `base` buck2_error yet.
    // Unfortunately, we cannot detect whether the remainder of the error chain is actually
    // associated with `anyhow::Context::context` calls or not. If it is, this will all work
    // correctly. If not, we might get some whacky formatting. However, in order for this to go
    // wrong, someone else has to have put an `anyhow::Error` into their custom error type, which
    // they really shouldn't be doing anyway.
    let mut e = base;
    for context in context_stack.into_iter().rev() {
        // Even for proper context objects, anyhow does not give us access to them directly. The
        // best we can do is turn them into strings.
        let context = format!("{}", context);
        e = e.context(context);
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

impl std::error::Error for CrateAsStdError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &*self.0.0 {
            ErrorKind::Root(r) => r.source(),
            ErrorKind::WithContext(_, r) | ErrorKind::Emitted(r) => {
                Some(CrateAsStdError::ref_cast(r))
            }
        }
    }
}

pub type CheckErrorType = for<'a> fn(
    &'a (dyn std::error::Error + 'static),
)
    -> Option<&'a (dyn std::error::Error + Send + Sync + 'static)>;

/// This can be `provide`d by an error to inject buck2-specific information about it.
///
/// Currently intended for macro use only, might make sense to allow it more generally in the
/// future.
#[derive(Copy, Clone)]
pub struct ProvidableMetadata {
    pub source_file: &'static str,
    /// Extra information to add to the end of the source location - typically a type/variant name,
    /// and the same thing as gets passed to `buck2_error::source_location::from_file`.
    pub source_location_extra: Option<&'static str>,
    pub category: Option<crate::Category>,
    pub typ: Option<crate::ErrorType>,
    /// Some errors will transitively call `Provide` for their sources, others won't. In order to
    /// make sure that we get consistent behavior, we need to be able to check that the error we're
    /// currently inspecting is actually the one doing the providing. This is how we do it.
    ///
    /// We also reuse this to get a `Send + Sync` reference to our error, since `source()` does not
    /// give us that.
    pub check_error_type: CheckErrorType,
}

impl ProvidableMetadata {
    pub const fn gen_check_error_type<E: std::error::Error + Send + Sync + 'static>()
    -> CheckErrorType {
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

    impl std::error::Error for TestError {}

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
    struct MetadataError;

    impl std::error::Error for MetadataError {
        fn provide<'a>(&'a self, demand: &mut Demand<'a>) {
            demand.provide_value(ProvidableMetadata {
                source_file: file!(),
                source_location_extra: Some("MetadataError"),
                category: Some(crate::Category::User),
                typ: Some(crate::ErrorType::Watchman),
                check_error_type: ProvidableMetadata::gen_check_error_type::<Self>(),
            });
        }
    }

    #[test]
    fn test_metadata() {
        for e in [MetadataError.into(), crate::Error::new(MetadataError)] {
            assert_eq!(e.get_category(), Some(crate::Category::User));
            assert_eq!(e.get_error_type(), Some(crate::ErrorType::Watchman));
            assert_eq!(
                e.source_location(),
                Some("buck2_error/src/any.rs::MetadataError")
            );
        }
    }

    #[test]
    fn test_metadata_through_anyhow() {
        let e: anyhow::Error = MetadataError.into();
        let e = e.context("anyhow");
        let e: crate::Error = e.into();
        assert_eq!(e.get_category(), Some(crate::Category::User));
        assert!(format!("{:?}", e).contains("anyhow"));
    }

    #[derive(Debug, thiserror::Error)]
    #[error("wrapper")]
    struct WrapperError(#[source] MetadataError);

    #[test]
    fn test_metadata_through_wrapper() {
        let e: crate::Error = WrapperError(MetadataError).into();
        assert_eq!(e.get_category(), Some(crate::Category::User));
        assert!(format!("{:?}", e).contains("wrapper"));
    }
}
