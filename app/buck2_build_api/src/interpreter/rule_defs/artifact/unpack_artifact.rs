/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_artifact::artifact::artifact_type::Artifact;
use dupe::Dupe;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::UnpackValue;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;

#[derive(StarlarkTypeRepr, UnpackValue)]
pub enum UnpackArtifactOrDeclaredArtifact<'v> {
    Artifact(&'v StarlarkArtifact),
    DeclaredArtifact(&'v StarlarkDeclaredArtifact),
}

impl<'v> UnpackArtifactOrDeclaredArtifact<'v> {
    pub fn artifact(&self) -> buck2_error::Result<Artifact> {
        match self {
            UnpackArtifactOrDeclaredArtifact::Artifact(x) => Ok(x.artifact.dupe()),
            UnpackArtifactOrDeclaredArtifact::DeclaredArtifact(x) => x.get_bound_artifact(),
        }
    }
}
