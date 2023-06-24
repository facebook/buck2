/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::typing::Ty;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueTyped;

use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;

pub enum OutputArtifactArg<'v> {
    Str(&'v str),
    OutputArtifact(ValueTyped<'v, StarlarkOutputArtifact<'v>>),
    DeclaredArtifact(ValueTyped<'v, StarlarkDeclaredArtifact>),
    /// This for error reporting.
    WrongArtifact(ValueAsArtifactLike<'v>),
}

impl<'v> StarlarkTypeRepr for OutputArtifactArg<'v> {
    fn starlark_type_repr() -> Ty {
        Ty::unions(vec![
            <&str>::starlark_type_repr(),
            StarlarkOutputArtifact::starlark_type_repr(),
            ValueAsArtifactLike::starlark_type_repr(),
        ])
    }
}

impl<'v> UnpackValue<'v> for OutputArtifactArg<'v> {
    #[allow(clippy::manual_map, clippy::same_functions_in_if_condition)]
    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.unpack_str() {
            Some(OutputArtifactArg::Str(s))
        } else if let Some(artifact) = ValueTyped::new(value) {
            Some(OutputArtifactArg::OutputArtifact(artifact))
        } else if let Some(artifact) = ValueTyped::new(value) {
            Some(OutputArtifactArg::DeclaredArtifact(artifact))
        } else if let Some(artifact_like) = ValueAsArtifactLike::unpack_value(value) {
            Some(OutputArtifactArg::WrongArtifact(artifact_like))
        } else {
            None
        }
    }
}
