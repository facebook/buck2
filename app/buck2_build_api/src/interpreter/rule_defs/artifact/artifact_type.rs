/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(clippy::empty_enums)]

use starlark::environment::GlobalsBuilder;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;

#[starlark_module]
pub(crate) fn register_artifact(globals: &mut GlobalsBuilder) {
    const Artifact: StarlarkValueAsType<StarlarkArtifact> = StarlarkValueAsType::new();
}
