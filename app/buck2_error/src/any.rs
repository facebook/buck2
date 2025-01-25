/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Integrations of `buck2_error::Error` with `anyhow::Error` and `StdError`.

use std::error::request_value;
use std::error::Error as StdError;
use std::fmt;

use buck2_data::error::ErrorTag;
use ref_cast::RefCast;

use crate::error::ErrorKind;
use crate::source_location::SourceLocation;

fn maybe_add_context_from_metadata(mut e: crate::Error, context: &dyn StdError) -> crate::Error {
    if let Some(metadata) = request_value::<ProvidableMetadata>(context) {
        if !metadata.tags.is_empty() {
            e = e.tag(metadata.tags.iter().copied());
        }
        e
    } else {
        e
    }
}

pub fn recover_crate_error(
    value: &'_ (dyn StdError + 'static),
    source_location: SourceLocation,
    error_tag: ErrorTag,
) -> crate::Error {
    // Instead of just turning this into an error root, we'll extract the whole context stack and
    // convert it manually.
    let mut context_stack = Vec::new();
    let mut cur = value;
    // We allow all of these to appear more than once in the context chain, however we always use
    // the bottom-most value when actually generating the root
    let mut source_location = source_location;
    let mut action_error = None;
    let base = 'base: loop {
        // Handle the `cur` error
        if let Some(base) = cur.downcast_ref::<CrateAsStdError>() {
            break base.0.clone();
        }

        if let Some(metadata) = request_value::<ProvidableMetadata>(cur) {
            source_location = metadata.source_location;
            if metadata.action_error.is_some() {
                action_error = metadata.action_error;
            }
        }

        // Compute the next element in the source chain
        if let Some(new_cur) = cur.source() {
            context_stack.push(cur);
            cur = new_cur;
            continue;
        }

        // `anyhow` only ever uses the standard `Display` formatting of error types, never the
        // alternate or debug formatting. We can match that behavior by just converting the error to
        // a string. That prevents us from having to deal with the type returned by `source` being
        // potentially non-`Send` or non-`Sync`.
        let description = format!("{}", cur);
        let e = crate::Error::new(description, error_tag, source_location, action_error);
        break 'base maybe_add_context_from_metadata(e, cur);
    };
    // We've converted the base error to a `buck2_error::Error`. Next, we need to add back any
    // context that is not included in the `base` error yet.
    let mut e = base;
    for context_value in context_stack.into_iter().rev() {
        if let Some(starlark_err) = cur.downcast_ref::<crate::starlark_error::BuckStarlarkError>() {
            e = e.context(format!("{}", starlark_err));
        } else {
            // First, just add the value directly. This value is only used for formatting
            e = e.context(format!("{}", context_value));
            // Now add any additional information from the metadata, if it's available
            e = maybe_add_context_from_metadata(e, context_value);
        }
    }
    e
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
            ErrorKind::Root(_) => None,
            ErrorKind::WithContext(_, r) | ErrorKind::Emitted(_, r) => {
                Some(CrateAsStdError::ref_cast(r))
            }
        }
    }
}

/// This can be `provide`d by an error to inject buck2-specific information about it.
///
/// For `action_error`, and the source information, only the value that appears last in the
/// source chain will be used. The derive macro typically handles this to prevent any surprises,
/// however if this value is being provided manually then care may need to be taken.
#[derive(Clone)]
pub struct ProvidableMetadata {
    pub tags: Vec<crate::ErrorTag>,
    pub source_location: SourceLocation,
    /// The protobuf ActionError, if the root was an action error
    pub action_error: Option<buck2_data::ActionError>,
}

#[cfg(test)]
mod tests {
    use std::error::Request;

    use allocative::Allocative;

    use super::*;
    use crate as buck2_error;
    use crate::conversion::from_any_with_tag;
    use crate::TypedContext;

    #[derive(Debug, derive_more::Display)]
    struct TestError;

    impl From<TestError> for crate::Error {
        #[cold]
        fn from(value: TestError) -> Self {
            let error = anyhow::Error::from(value);
            let source_location = SourceLocation::new(file!());
            crate::any::recover_crate_error(error.as_ref(), source_location, ErrorTag::Input)
        }
    }

    impl StdError for TestError {}

    #[test]
    fn test_roundtrip_no_context() {
        let e = crate::Error::from(TestError).context("context 1");
        let e2 = from_any_with_tag(anyhow::Error::from(e.clone()), ErrorTag::Input);
        crate::Error::check_equal(&e, &e2);
    }

    #[test]
    fn test_roundtrip_with_context() {
        let e = crate::Error::from(TestError).context("context 1");
        let e2 = from_any_with_tag(
            anyhow::Error::from(e.clone()).context("context 2"),
            ErrorTag::Input,
        );
        let e3 = e.context("context 2");
        crate::Error::check_equal(&e2, &e3);
    }

    #[test]
    fn test_roundtrip_with_typed_context() {
        #[derive(Debug, Allocative, Eq, PartialEq)]
        struct T(usize);
        impl std::fmt::Display for T {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:?}", self)
            }
        }

        impl TypedContext for T {
            fn eq(&self, other: &dyn TypedContext) -> bool {
                match (other as &dyn std::any::Any).downcast_ref::<Self>() {
                    Some(right) => self == right,
                    None => false,
                }
            }
        }

        let e = crate::Error::from(TestError).context(T(1));
        let e2 = from_any_with_tag(
            anyhow::Error::from(e.clone()).context("context 2"),
            ErrorTag::Input,
        );
        let e3 = e.context("context 2");
        crate::Error::check_equal(&e2, &e3);
    }

    #[derive(Debug, derive_more::Display)]
    struct FullMetadataError;

    impl From<FullMetadataError> for crate::Error {
        #[cold]
        fn from(value: FullMetadataError) -> Self {
            let error = anyhow::Error::from(value);
            let source_location = SourceLocation::new(file!());
            crate::any::recover_crate_error(error.as_ref(), source_location, ErrorTag::Input)
        }
    }

    impl StdError for FullMetadataError {
        fn provide<'a>(&'a self, request: &mut Request<'a>) {
            request.provide_value(ProvidableMetadata {
                action_error: None,
                source_location: SourceLocation::new(file!()).with_type_name("FullMetadataError"),
                tags: vec![
                    crate::ErrorTag::WatchmanTimeout,
                    crate::ErrorTag::StarlarkFail,
                    crate::ErrorTag::WatchmanTimeout,
                ],
            });
        }
    }

    #[test]
    fn test_metadata() {
        for e in [
            FullMetadataError.into(),
            crate::Error::from(FullMetadataError),
        ] {
            assert_eq!(e.get_tier(), Some(crate::Tier::Tier0));
            assert_eq!(
                e.source_location().to_string(),
                "buck2_error/src/any.rs::FullMetadataError"
            );
            assert_eq!(
                &e.tags(),
                &[
                    crate::ErrorTag::Input,
                    crate::ErrorTag::StarlarkFail,
                    crate::ErrorTag::WatchmanTimeout
                ]
            );
        }
    }

    #[test]
    fn test_metadata_through_anyhow() {
        let e: anyhow::Error = FullMetadataError.into();
        let e = e.context("anyhow");
        let e: crate::Error = from_any_with_tag(e, ErrorTag::Input);
        assert_eq!(e.get_tier(), Some(crate::Tier::Tier0));
        assert!(format!("{:?}", e).contains("anyhow"));
    }

    #[derive(Debug, buck2_error_derive::Error)]
    #[error("wrapper")]
    #[buck2(tag = Environment)]
    struct WrapperError(#[source] FullMetadataError);

    #[test]
    fn test_metadata_through_wrapper() {
        let e: crate::Error = WrapperError(FullMetadataError).into();
        assert_eq!(e.get_tier(), Some(crate::Tier::Tier0));
        assert!(format!("{:?}", e).contains("wrapper"));
    }

    #[derive(Debug, buck2_error_derive::Error)]
    #[buck2(tier0)]
    #[error("wrapper2")]
    struct FullMetadataContextWrapperError(#[source] FullMetadataError);

    #[test]
    fn test_context_in_wrapper() {
        let e: crate::Error = FullMetadataContextWrapperError(FullMetadataError).into();
        assert_eq!(e.get_tier(), Some(crate::Tier::Tier0));
        assert_eq!(
            e.source_location().to_string(),
            "buck2_error/src/any.rs::FullMetadataError"
        );
        assert!(format!("{:?}", e).contains("wrapper2"));
    }

    #[derive(Debug, buck2_error_derive::Error)]
    #[buck2(input)]
    #[error("unused")]
    struct UserMetadataError;

    #[derive(Debug, buck2_error_derive::Error)]
    #[buck2(tier0)]
    #[error("unused")]
    struct InfraMetadataWrapperError(#[source] UserMetadataError);

    #[test]
    fn test_no_root_metadata_context() {
        let e = InfraMetadataWrapperError(UserMetadataError);
        let e: crate::Error = e.into();
        assert_eq!(e.get_tier(), Some(crate::Tier::Tier0));
    }
}
