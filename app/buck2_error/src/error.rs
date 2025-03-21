/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::sync::Arc;

use buck2_data::ActionError;
use smallvec::SmallVec;

use crate::classify::best_tag;
use crate::classify::error_tag_category;
use crate::classify::tag_is_generic;
use crate::classify::tag_is_hidden;
use crate::context_value::ContextValue;
use crate::context_value::StarlarkContext;
use crate::context_value::TypedContext;
use crate::format::into_anyhow_for_format;
use crate::root::ErrorRoot;
use crate::source_location::SourceLocation;
use crate::ErrorTag;
use crate::Tier;
use crate::UniqueRootId;
pub type DynLateFormat = dyn Fn(&mut fmt::Formatter<'_>) -> fmt::Result + Send + Sync + 'static;

/// The core error type provided by this crate.
///
/// While this type has many of the features of `anyhow::Error`, in most places you should continue
/// to use `anyhow`. This type is only expected to appear on a small number of APIs which require a
/// clonable error.
///
/// Unlike `anyhow::Error`, this type supports no downcasting. That is an intentional choice -
/// downcasting errors is fragile and becomes difficult to support in conjunction with anyhow
/// compatibility.
#[derive(allocative::Allocative, Clone, dupe::Dupe)]
pub struct Error(pub(crate) Arc<ErrorKind>);

/// The actual error representation.
///
/// The representation is expected to take on a significant bit of additional complexity in the
/// future - the current version is an initial MVP.
///
/// Right now, this type can represent an error root, together with a stack of context information.
#[derive(allocative::Allocative)]
pub(crate) enum ErrorKind {
    Root(Box<ErrorRoot>),
    /// For now we use untyped context to maximize compatibility with anyhow.
    WithContext(ContextValue, Error),
    /// Indicates that the error has been emitted, ie shown to the user.
    // This `Arc` should ideally be a `Box`. However, that doesn't work right now because of the
    // implementation of `into_anyhow_for_format`.
    #[allocative(skip)] // FIXME(JakobDegen): "Implementation is not general enough"
    Emitted(Arc<DynLateFormat>, Error),
}

impl Error {
    #[track_caller]
    #[cold]
    pub fn new(
        error_msg: String,
        error_tag: ErrorTag,
        source_location: SourceLocation,
        action_error: Option<ActionError>,
    ) -> Self {
        let error_root = ErrorRoot::new(error_msg, error_tag, source_location, action_error);

        let buck2_error = crate::Error(Arc::new(ErrorKind::Root(Box::new(error_root))));
        buck2_error.tag([error_tag])
    }

    fn iter_kinds<'a>(&'a self) -> impl Iterator<Item = &'a ErrorKind> {
        let mut cur = Some(self);
        std::iter::from_fn(move || {
            let out = cur?;
            match &*out.0 {
                ErrorKind::WithContext(_, next) | ErrorKind::Emitted(_, next) => cur = Some(next),
                ErrorKind::Root(_) => cur = None,
            };
            Some(out.0.as_ref())
        })
    }

    fn root(&self) -> &ErrorRoot {
        let Some(ErrorKind::Root(r)) = self.iter_kinds().last() else {
            unreachable!()
        };
        r
    }

    pub fn action_error(&self) -> Option<&buck2_data::ActionError> {
        self.root().action_error()
    }

    pub(crate) fn iter_context<'a>(&'a self) -> impl Iterator<Item = &'a ContextValue> {
        self.iter_kinds().filter_map(|kind| match kind {
            ErrorKind::WithContext(ctx, _) => Some(ctx),
            _ => None,
        })
    }

    pub fn mark_emitted(self, late_format: Arc<DynLateFormat>) -> Self {
        // Have to write this kind of weird to get the compiler to infer a higher ranked closure
        Self(Arc::new(ErrorKind::Emitted(late_format, self)))
    }

    /// If the error has not been emitted yet, returns `None`, otherwise `Some`.
    ///
    /// Most errors are only shown to the user once. However, some errors, specifically action
    /// errors, are shown to the user twice: Once when the error occurs, and again at the end of the
    /// build in the form of a short "Failed to build target" summary.
    ///
    /// After the error has been shown to the user for the first time, it is marked as emitted. The
    /// late formatter that is returned here is what should be printed at the end of the build
    pub fn is_emitted<'a>(&'a self) -> Option<impl fmt::Debug + fmt::Display + 'a> {
        let (val, was_late_formatted) = into_anyhow_for_format(self, true);
        if was_late_formatted { Some(val) } else { None }
    }

    /// Only intended to be used for debugging, helps to understand the structure of the error
    pub fn get_stack_for_debug(&self) -> String {
        use fmt::Write;
        let mut s = String::new();
        for kind in self.iter_kinds() {
            match kind {
                ErrorKind::Root(r) => {
                    writeln!(s, "ROOT:\n{:#?}", r).unwrap();
                }
                ErrorKind::Emitted(_, _) => {
                    writeln!(s, "EMITTED").unwrap();
                }
                ErrorKind::WithContext(ctx, _) => {
                    writeln!(s, "CONTEXT: {:#}", ctx).unwrap();
                }
            }
        }
        s
    }

    /// Identifier for deduplication during a build.
    pub fn root_id(&self) -> UniqueRootId {
        self.root().id()
    }

    /// Stable identifier for grouping errors.
    ///
    /// This tries to include the least information possible that can be used to uniquely identify an error type.
    pub fn category_key(&self) -> String {
        let tags = self.tags();

        let non_generic_tags: Vec<ErrorTag> = tags
            .clone()
            .into_iter()
            .filter(|tag| !tag_is_generic(tag))
            .collect();

        let (source_location, key_tags) = if !non_generic_tags.is_empty() {
            (None, non_generic_tags)
        } else {
            // Only include source location if there are no non-generic tags.
            let source_location = if let Some(type_name) = self.source_location().type_name() {
                // If type name available, include it and exclude source location.
                Some(type_name.to_owned())
            } else {
                Some(self.source_location().to_string())
            };

            (
                source_location,
                // Only include generic tags if there are no non-generic tags. Always exclude hidden tags.
                tags.into_iter().filter(|tag| !tag_is_hidden(tag)).collect(),
            )
        };

        let key_tags = key_tags.into_iter().map(|tag| tag.as_str_name().to_owned());

        let context_key_values = self.iter_context().filter_map(|kind| match kind {
            ContextValue::Key(val) => Some(val.to_string()),
            _ => None,
        });

        let values: Vec<String> = source_location
            .into_iter()
            .chain(key_tags)
            .chain(context_key_values)
            .collect();

        values.join(":").to_owned()
    }

    pub fn source_location(&self) -> &SourceLocation {
        self.root().source_location()
    }

    pub fn context<C: Into<ContextValue>>(self, context: C) -> Self {
        Self(Arc::new(ErrorKind::WithContext(context.into(), self)))
    }

    pub fn context_for_key(self, context: &str) -> Self {
        Self(Arc::new(ErrorKind::WithContext(
            ContextValue::Key(context.into()),
            self,
        )))
    }

    pub fn context_for_starlark_backtrace(self, context: StarlarkContext) -> Self {
        Self(Arc::new(ErrorKind::WithContext(
            ContextValue::StarlarkError(context),
            self,
        )))
    }

    #[cold]
    #[track_caller]
    pub(crate) fn new_anyhow_with_context<E, C: Into<ContextValue>>(e: E, c: C) -> anyhow::Error
    where
        Error: From<E>,
    {
        crate::Error::from(e).context(c).into()
    }

    pub fn tag(self, tags: impl IntoIterator<Item = crate::ErrorTag>) -> Self {
        let tags = SmallVec::from_iter(tags);
        if tags.is_empty() {
            self
        } else {
            self.context(ContextValue::Tags(tags))
        }
    }

    pub fn get_tier(&self) -> Option<Tier> {
        best_tag(self.tags()).map(error_tag_category).flatten()
    }

    /// All tags unsorted and with duplicates.
    fn tags_unsorted(&self) -> impl Iterator<Item = crate::ErrorTag> + '_ {
        self.iter_context()
            .filter_map(|kind| match kind {
                ContextValue::Tags(tags) => Some(tags.iter().copied()),
                _ => None,
            })
            .flatten()
    }

    pub fn find_typed_context<T: TypedContext>(&self) -> Option<Arc<T>> {
        self.iter_context().find_map(|kind| match kind {
            ContextValue::Typed(v) => {
                if let Ok(typed) = Arc::downcast(v.clone()) {
                    Some(typed)
                } else {
                    None
                }
            }
            _ => None,
        })
    }

    /// Get all the tags that have been added to this error
    pub fn tags(&self) -> Vec<crate::ErrorTag> {
        let mut tags: Vec<_> = self.tags_unsorted().collect();
        tags.sort_unstable_by_key(|tag| tag.as_str_name());
        tags.dedup();
        tags
    }

    /// The most interesting tag among this error tags.
    pub fn best_tag(&self) -> Option<crate::ErrorTag> {
        best_tag(self.tags_unsorted())
    }

    pub fn has_tag(&self, tag: crate::ErrorTag) -> bool {
        self.tags_unsorted().any(|t| t == tag)
    }

    pub(crate) fn compute_context<
        TC: TypedContext,
        C1: Into<ContextValue>,
        C2: Into<ContextValue>,
        F: FnOnce(Arc<TC>) -> C1,
        F2: FnOnce() -> C2,
    >(
        self,
        map_context: F,
        new_context: F2,
    ) -> crate::Error {
        if let ErrorKind::WithContext(crate::context_value::ContextValue::Typed(v), err) = &*self.0
        {
            if let Ok(typed) = Arc::downcast(v.clone()) {
                return Self(Arc::new(ErrorKind::WithContext(
                    map_context(typed).into(),
                    err.clone(),
                )));
            }
        }
        self.context(new_context())
    }

    #[cfg(test)]
    pub(crate) fn check_equal(mut a: &Self, mut b: &Self) {
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
                (ErrorKind::Emitted(_, a_inner), ErrorKind::Emitted(_, b_inner)) => {
                    a = a_inner;
                    b = b_inner;
                }
                (_, _) => {
                    panic!("Left side did not match right: {:?} {:?}", a, b)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate as buck2_error;
    use crate::conversion::from_any_with_tag;
    use crate::Tier;

    #[derive(Debug, buck2_error_derive::Error)]
    #[error("Test")]
    #[buck2(tag = Environment)]
    struct TestError;

    #[test]
    fn test_emitted_works() {
        let e: crate::Error = TestError.into();
        assert!(e.is_emitted().is_none());
        let e = e.mark_emitted(Arc::new(|_| Ok(())));
        assert!(e.is_emitted().is_some());
        let e: anyhow::Error = e.into();
        let e: crate::Error = from_any_with_tag(e.context("context"), crate::ErrorTag::Input);
        assert!(e.is_emitted().is_some());
    }

    #[test]
    fn test_root_id() {
        let e1: crate::Error = TestError.into();
        let e1x = e1.clone().context("context");
        let e1y = e1.clone().context("context2");

        let e2: crate::Error = TestError.into();

        assert_eq!(e1.root_id(), e1x.root_id());
        assert_eq!(e1.root_id(), e1y.root_id());
        assert_eq!(e1x.root_id(), e1y.root_id());

        assert_ne!(e1.root_id(), e2.root_id());
    }

    #[test]
    fn test_get_tier() {
        let e: crate::Error = crate::Error::from(TestError)
            .tag([crate::ErrorTag::Tier0, crate::ErrorTag::Environment]);
        assert_eq!(e.get_tier(), Some(Tier::Environment));
        let e: crate::Error = crate::Error::from(TestError)
            .tag([crate::ErrorTag::Environment, crate::ErrorTag::Input]);
        assert_eq!(e.get_tier(), Some(Tier::Environment));
    }

    #[test]
    fn test_category_key() {
        let err: crate::Error = TestError.into();
        assert_eq!(err.category_key(), "TestError");

        let err = err.clone().tag([crate::ErrorTag::Analysis]);
        assert_eq!(
            err.category_key(),
            format!(
                "{}:{}",
                err.source_location().type_name().unwrap(),
                "ANALYSIS"
            )
        );

        let err = err.clone().tag([
            crate::ErrorTag::AnyActionExecution,
            crate::ErrorTag::ReInternal,
        ]);
        assert_eq!(err.category_key(), format!("RE_INTERNAL"));
    }
}
