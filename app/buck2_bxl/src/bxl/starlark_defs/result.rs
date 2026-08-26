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
use starlark::starlark_complex_value_branded;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::FreezeBranded;
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
    starlark::StarlarkPagablePanic // badbadbad!!! todo!("bxl")
)]
#[display("bxl.Error({})", StarlarkStr::repr(&format!("{err:?}")))]
pub(crate) struct StarlarkError {
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
    FreezeBranded,
    ProvidesStaticType,
    Allocative,
    starlark::StarlarkPagablePanic // badbadbad!!! todo!("bxl")
)]
pub(crate) enum StarlarkResult<'v> {
    Ok(Value<'v>),
    Err(#[freeze_branded(identity)] buck2_error::Error),
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

starlark_complex_value_branded!(pub(crate) StarlarkResult);

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
