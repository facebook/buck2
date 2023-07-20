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
use starlark::docs::StarlarkDocs;
use starlark::starlark_simple_value;
use starlark::values::starlark_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

/// We do not make metadata available to rules, so instead we expose this opaque value when trying
/// to resolve it to a Starlark object.
#[derive(
    Debug,
    Dupe,
    Clone,
    PartialEq,
    ProvidesStaticType,
    Allocative,
    StarlarkDocs,
    NoSerialize,
    Display
)]
#[display(fmt = "{:?}", self)]
pub struct OpaqueMetadata;

starlark_simple_value!(OpaqueMetadata);

#[starlark_value(type = "opaque_metadata")]
impl<'v> StarlarkValue<'v> for OpaqueMetadata {}
