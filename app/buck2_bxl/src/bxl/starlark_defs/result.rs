/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Display;

use allocative::Allocative;
use derivative::Derivative;
use derive_more::Display;
use display_container::fmt_container;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use serde::ser::SerializeMap;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::starlark_complex_value;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::Freeze;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::starlark_value;
use starlark::values::string::StarlarkStr;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum BxlResultError {
    #[error("called `bxl.Result.unwrap()` on an `Err` value: {0}")]
    UnwrapOnError(buck2_error::Error),
    #[error("called `bxl.Result.unwrap_err()` on an `Ok` value: {0}")]
    UnwrapErrOnOk(String),
}

/// Error value object returned by fallible BXL operation.
#[derive(
    Debug,
    ProvidesStaticType,
    Derivative,
    Display,
    Allocative,
    Trace,
    starlark::StarlarkPagable
)]
#[display("bxl.Error({})", StarlarkStr::repr(&format!("{err:?}")))]
pub(crate) struct StarlarkError {
    #[starlark_pagable(pagable)]
    err: buck2_error::Error,
}

impl Serialize for StarlarkError {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(2))?;
        map.serialize_entry("result", "error")?;
        map.serialize_entry("value", &format!("{:?}", self.err))?;
        map.end()
    }
}

impl StarlarkError {
    pub(crate) fn new(err: buck2_error::Error) -> Self {
        Self { err }
    }
}

starlark_simple_value!(StarlarkError);

starlark::methods_static!(BXL_ERROR_METHODS = error_methods);

#[starlark_value(type = "bxl.Error")]
impl<'v> StarlarkValue<'v> for StarlarkError {
    fn get_methods() -> Option<&'static Methods> {
        Some(BXL_ERROR_METHODS.methods())
    }
}

/// The error type for bxl
#[starlark_module]
fn error_methods(builder: &mut MethodsBuilder) {
    /// The error message
    #[starlark(attribute)]
    fn message<'v>(this: &'v StarlarkError) -> starlark::Result<String> {
        Ok(format!("{:?}", this.err))
    }
}

#[derive(
    Debug,
    Trace,
    Freeze,
    ProvidesStaticType,
    Allocative,
    starlark::StarlarkPagable
)]
pub(crate) enum StarlarkResult<'v> {
    Ok(Value<'v>),
    Err(
        #[freeze(identity)]
        #[starlark_pagable(pagable)]
        buck2_error::Error,
    ),
}

impl<'v> Serialize for StarlarkResult<'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(2))?;
        match self {
            StarlarkResult::Ok(val) => {
                map.serialize_entry("result", "ok")?;
                map.serialize_entry("value", val)?;
            }
            StarlarkResult::Err(err) => {
                map.serialize_entry("result", "error")?;
                map.serialize_entry("value", &format!("{:?}", err))?;
            }
        }
        map.end()
    }
}

starlark_complex_value!(pub(crate) StarlarkResult);

impl<'v> Display for StarlarkResult<'v> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StarlarkResult::Ok(val) => fmt_container(f, "Result(Ok = ", ")", [val]),
            StarlarkResult::Err(err) => fmt_container(
                f,
                "Result(Err = ",
                ")",
                // TODO(nero): implement multiline when multiline is requested
                [StarlarkStr::repr(&format!("{err:?}"))],
            ),
        }
    }
}

#[starlark_value(type = "bxl.Result")]
impl<'v> StarlarkValue<'v> for StarlarkResult<'v> {
    fn get_methods() -> Option<&'static Methods>
    where
        Self: Sized,
    {
        Some(BXL_RESULT_METHODS.methods())
    }
}

starlark::methods_static!(BXL_RESULT_METHODS = result_methods);

#[starlark_module]
fn result_methods(builder: &mut MethodsBuilder) {
    /// Returns true if the result is an `Ok` value, false if it is an Error
    fn is_ok<'v>(this: &'v StarlarkResult<'v>) -> starlark::Result<bool> {
        Ok(this.is_ok())
    }

    /// Unwrap the result, returning the inner value if the result is `Ok`.
    /// If the result is an `Error`, it will fail
    fn unwrap<'v>(this: &'v StarlarkResult<'v>) -> starlark::Result<Value<'v>> {
        Ok(this.unwrap()?)
    }

    /// If the result is an `Ok`, return the inner value, otherwise return the default
    fn unwrap_or<'v>(
        this: &'v StarlarkResult<'v>,
        #[starlark(require = pos)] default: Value<'v>,
    ) -> starlark::Result<Value<'v>> {
        Ok(this.unwrap_or(default))
    }

    /// Unwrap the error, returning the inner error if the result is `Err`.
    /// If the result is an `Ok`, it will fail
    fn unwrap_err<'v>(this: &'v StarlarkResult<'v>) -> starlark::Result<StarlarkError> {
        Ok(this.unwrap_err()?)
    }
}

impl<'v> StarlarkResult<'v> {
    pub(crate) fn from_result(res: buck2_error::Result<Value<'v>>) -> Self {
        match res {
            Ok(val) => Self::Ok(val),
            Err(err) => Self::Err(err),
        }
    }

    fn is_ok(&self) -> bool {
        match self {
            StarlarkResult::Ok(_) => true,
            StarlarkResult::Err(_) => false,
        }
    }

    fn unwrap(&self) -> buck2_error::Result<Value<'v>> {
        match self {
            StarlarkResult::Ok(val) => Ok(*val),
            StarlarkResult::Err(err) => Err(BxlResultError::UnwrapOnError(err.dupe()).into()),
        }
    }

    fn unwrap_or(&self, default: Value<'v>) -> Value<'v> {
        match self {
            StarlarkResult::Ok(val) => *val,
            StarlarkResult::Err(_) => default,
        }
    }

    fn unwrap_err(&self) -> buck2_error::Result<StarlarkError> {
        match self {
            StarlarkResult::Ok(val) => {
                let display_str = format!("{val}");
                Err(BxlResultError::UnwrapErrOnOk(display_str).into())
            }
            StarlarkResult::Err(err) => Ok(StarlarkError { err: err.dupe() }),
        }
    }
}

starlark::__starlark_pagable_only! {
    #[cfg(test)]
    mod tests {
        use buck2_error::ErrorTag;
        use pagable::PagableDeserialize;
        use pagable::PagableSerialize;
        use starlark::values::FrozenHeapName;
        use starlark::values::OwnedFrozen;

        use super::*;

        fn round_trip(
            owned: OwnedFrozen<Value<'static>>,
        ) -> pagable::Result<OwnedFrozen<Value<'static>>> {
            let mut serializer = pagable::testing::TestingSerializer::new();
            owned.pagable_serialize(&mut serializer)?;
            let bytes = serializer.finish();
            let mut deserializer = pagable::testing::TestingDeserializer::new(&bytes);
            OwnedFrozen::<Value<'static>>::pagable_deserialize(&mut deserializer)
        }

        fn test_error() -> buck2_error::Error {
            buck2_error::buck2_error!(ErrorTag::Analysis, "analysis of `cell//pkg:target` failed")
                .context("while resolving a lazy operation")
                .string_tag("bxl_result_test")
                .tag([ErrorTag::Bxl])
        }

        /// A rendering cut before the backtrace `anyhow` appends under
        /// `RUST_BACKTRACE`, which is captured where the error is formatted and
        /// so never compares equal between two renderings. The cut is the same
        /// on both sides of a comparison, so what follows it does not matter,
        /// including inside a `repr` that escapes the newlines.
        fn without_backtrace(rendered: &str) -> &str {
            rendered
                .split_once("Stack backtrace:")
                .map_or(rendered, |(message, _backtrace)| message)
        }

        /// What a paged-in error keeps; see `buck2_error::paging`.
        fn assert_same_error(restored: &buck2_error::Error, original: &buck2_error::Error) {
            assert_eq!(
                without_backtrace(&format!("{restored:?}")),
                without_backtrace(&format!("{original:?}"))
            );
            assert_eq!(restored.tags(), original.tags());
            assert_eq!(restored.string_tags(), original.string_tags());
            assert_eq!(restored.source_location(), original.source_location());
            assert_eq!(restored.category_key(), original.category_key());
        }

        #[test]
        fn result_err_round_trips() -> pagable::Result<()> {
            let err = test_error();
            let owned: OwnedFrozen<Value<'static>> =
                OwnedFrozen::build(FrozenHeapName::user("result_err_round_trips"), |heap| {
                    heap.alloc(StarlarkResult::Err(err.dupe()))
                });
            let expected_display = owned.by_ref(|v| v.to_string());

            let restored = round_trip(owned)?;

            restored.by_ref(|v| {
                let result = StarlarkResult::from_value(*v).expect("a bxl.Result");
                assert!(!result.is_ok());
                assert_eq!(
                    without_backtrace(&v.to_string()),
                    without_backtrace(&expected_display)
                );
                let restored_err = result.unwrap_err().expect("an Err result");
                assert_same_error(&restored_err.err, &err);
            });
            Ok(())
        }

        #[test]
        fn result_ok_round_trips() -> pagable::Result<()> {
            let owned: OwnedFrozen<Value<'static>> =
                OwnedFrozen::build(FrozenHeapName::user("result_ok_round_trips"), |heap| {
                    heap.alloc(StarlarkResult::Ok(heap.alloc("payload")))
                });

            let restored = round_trip(owned)?;

            restored.by_ref(|v| {
                let result = StarlarkResult::from_value(*v).expect("a bxl.Result");
                assert!(result.is_ok());
                let payload = result.unwrap().expect("an Ok result");
                assert_eq!(payload.unpack_str(), Some("payload"));
            });
            Ok(())
        }

        #[test]
        fn error_round_trips() -> pagable::Result<()> {
            let err = test_error();
            let owned: OwnedFrozen<Value<'static>> =
                OwnedFrozen::build(FrozenHeapName::user("error_round_trips"), |heap| {
                    heap.alloc(StarlarkError::new(err.dupe()))
                });
            let expected_display = owned.by_ref(|v| v.to_string());

            let restored = round_trip(owned)?;

            restored.by_ref(|v| {
                let restored_err = v.downcast_ref::<StarlarkError>().expect("a bxl.Error");
                assert_eq!(
                    without_backtrace(&v.to_string()),
                    without_backtrace(&expected_display)
                );
                assert_same_error(&restored_err.err, &err);
            });
            Ok(())
        }
    }
}
