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
use starlark::environment::GlobalsBuilder;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

/// Symbol for artifact type. Matches `StarlarkArtifact` and `StarlarkDeclaredArtifact`
/// which share the same type name `artifact`.
#[derive(
    Debug,
    derive_more::Display,
    NoSerialize,
    ProvidesStaticType,
    Allocative
)]
#[allocative(skip)] // TODO(nga): support empty enums.
enum AbstractArtifact {}

#[starlark_value(type = "artifact")]
impl<'v> StarlarkValue<'v> for AbstractArtifact {}

#[starlark_module]
pub(crate) fn register_artifact(globals: &mut GlobalsBuilder) {
    const Artifact: StarlarkValueAsType<AbstractArtifact> = StarlarkValueAsType::new();
}
