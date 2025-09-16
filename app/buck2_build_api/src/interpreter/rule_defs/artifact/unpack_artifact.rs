/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_artifact::artifact::artifact_type::Artifact;
use dupe::Dupe;
use starlark::values::UnpackValue;
use starlark::values::type_repr::StarlarkTypeRepr;

use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use crate::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkInputArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;

#[derive(StarlarkTypeRepr, UnpackValue)]
pub enum UnpackNonPromiseInputArtifact<'v> {
    Artifact(&'v StarlarkArtifact),
    DeclaredArtifact(&'v StarlarkDeclaredArtifact<'v>),
}

impl<'v> UnpackNonPromiseInputArtifact<'v> {
    pub fn artifact(&self) -> buck2_error::Result<Artifact> {
        match self {
            UnpackNonPromiseInputArtifact::Artifact(x) => Ok(x.artifact.dupe()),
            UnpackNonPromiseInputArtifact::DeclaredArtifact(x) => x.get_bound_artifact(),
        }
    }
}
