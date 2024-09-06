/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::values::starlark_value;
use starlark::values::AllocStaticSimple;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;

/// We do not make metadata available to rules, so instead we expose this opaque value when trying
/// to resolve it to a Starlark object.
#[derive(
    Debug,
    Dupe,
    Clone,
    PartialEq,
    ProvidesStaticType,
    Allocative,
    NoSerialize,
    Display
)]
#[display("{:?}", self)]
pub struct OpaqueMetadata;

#[starlark_value(type = "OpaqueMetadata")]
impl<'v> StarlarkValue<'v> for OpaqueMetadata {}

impl<'v> AllocValue<'v> for OpaqueMetadata {
    fn alloc_value(self, _heap: &'v Heap) -> Value<'v> {
        static INSTANCE: AllocStaticSimple<OpaqueMetadata> =
            AllocStaticSimple::alloc(OpaqueMetadata);
        INSTANCE.to_frozen_value().to_value()
    }
}
