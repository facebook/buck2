/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::empty_enum)]

use allocative::Allocative;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsStatic;
use starlark::values::starlark_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

use crate::interpreter::rule_defs::provider::provider_methods;

/// Base type for all providers, both built-in and user defined.
///
/// This is used for starlark typechecking.
#[derive(
    Debug,
    ProvidesStaticType,
    Allocative,
    derive_more::Display,
    NoSerialize
)]
#[display(fmt = "unreachable")]
#[allocative(skip)] // TODO(nga): fix bug in allocative: this should be derived easily.
pub(crate) enum AbstractProvider {}

#[starlark_value(type = "provider")]
impl<'v> StarlarkValue<'v> for AbstractProvider {
    // We don't instantiate this type, but expose methods for the typechecker.
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(provider_methods)
    }
}
