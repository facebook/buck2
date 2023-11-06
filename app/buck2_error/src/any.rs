/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Integrations of `buck2_error::Error` with `anyhow::Error` and `std::error::Error`.

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
            return from_anyhow_for_crate(Marc::new(e.take().unwrap()), source_location);
        }

        // Otherwise, we'll use the strategy for `std::error::Error`
        let anyhow = e.unwrap().into();
        crate::Error(Arc::new(ErrorKind::Root(ErrorRoot::new_anyhow(
            Marc::new(anyhow),
            source_location,
        ))))
    }
}

fn from_anyhow_for_crate(
    value: Marc<anyhow::Error>,
    source_location: Option<String>,
) -> crate::Error {
    // Instead of just turning this into an error root, we will first check if this
    // `anyhow::Error` was created from a `buck2_error::Error`. If so, we can recover the context in
    // a structured way.
    let mut context_stack = Vec::new();
    let mut chain = value.chain();
    let base = loop {
        match chain.next() {
            None => {
                // This error was not created from a `buck2_error::Error`, so we can't do anything
                // smart
                return crate::Error(Arc::new(ErrorKind::Root(ErrorRoot::new_anyhow(
                    value,
                    source_location,
                ))));
            }
            Some(e) => {
                if let Some(base) = e.downcast_ref::<CrateAsStdError>() {
                    break base;
                } else {
                    context_stack.push(e);
                }
            }
        }
    };
    // We've discovered that this `anyhow::Error` has a cause chain that includes a
    // `buck2_error::Error`. We'll try and recover a properly structured error by converting the
    // part of the cause chain that's not in the base to context on the buck2_error error.
    // Unfortunately, we cannot detect whether the remainder of the error chain is actually
    // associated with `.context` calls on the anyhow error or not. If it is, this will all work
    // correctly. If not, we might get some whacky formatting. However, in order for this to go
    // wrong, someone else has to have put an `anyhow::Error` into their custom error type,
    // which they really shouldn't be doing anyway.
    let mut e = base.0.clone();
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

#[cfg(test)]
mod tests {
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
}
