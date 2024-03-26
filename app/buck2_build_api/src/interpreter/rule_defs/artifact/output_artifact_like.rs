/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::UnpackValue;
use starlark::values::ValueTyped;

use crate::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;

#[derive(StarlarkTypeRepr, UnpackValue)]
pub enum OutputArtifactArg<'v> {
    Str(&'v str),
    OutputArtifact(ValueTyped<'v, StarlarkOutputArtifact<'v>>),
    DeclaredArtifact(ValueTyped<'v, StarlarkDeclaredArtifact>),
    /// This for error reporting.
    WrongArtifact(ValueAsArtifactLike<'v>),
}
