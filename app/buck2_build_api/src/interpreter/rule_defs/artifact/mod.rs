/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod associated;
mod starlark_artifact;
pub mod starlark_artifact_like;
mod starlark_artifact_value;
mod starlark_declared_artifact;
mod starlark_output_artifact;

use std::fmt::Debug;

use buck2_core::base_deferred_key_dyn::BaseDeferredKeyDyn;

pub use self::starlark_artifact::StarlarkArtifact;
pub(crate) use self::starlark_artifact_like::StarlarkArtifactLike;
pub(crate) use self::starlark_artifact_like::ValueAsArtifactLike;
pub use self::starlark_artifact_value::StarlarkArtifactValue;
pub use self::starlark_declared_artifact::StarlarkDeclaredArtifact;
pub use self::starlark_output_artifact::FrozenStarlarkOutputArtifact;
pub use self::starlark_output_artifact::StarlarkOutputArtifact;

#[derive(Debug, thiserror::Error)]
pub(crate) enum ArtifactError {
    #[error("expected artifact {repr} to be used as the output of an action, but it was not")]
    DeclaredArtifactWasNotBound { repr: String },
    #[error(
        "attempted to use source artifact {repr} as the output of an action. Source \
        artifacts may not be outputs."
    )]
    SourceArtifactAsOutput { repr: String },
    #[error(
        "attempted to use artifact {artifact_repr} as the output of an action, but \
        it was already used by another action in {existing_owner}"
    )]
    BoundArtifactAsOutput {
        artifact_repr: String,
        existing_owner: BaseDeferredKeyDyn,
    },
}
