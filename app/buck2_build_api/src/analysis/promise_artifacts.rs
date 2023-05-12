/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::Arc;

use allocative::Allocative;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use dupe::Dupe;
use once_cell::sync::OnceCell;
use starlark::codemap::FileSpan;
use starlark::values::Trace;
use starlark::values::ValueTyped;

use crate::artifact_groups::promise::PromiseArtifact;
use crate::artifact_groups::promise::PromiseArtifactId;
use crate::artifact_groups::promise::PromiseArtifactResolveError;
use crate::deferred::base_deferred_key::BaseDeferredKey;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;

/// The PromiseArtifactRegistry stores promises registered with ctx.actions.promise_artifact() and their
/// corresponding internal PromiseArtifact. At the end of analysis (after promises have been resolved),
/// all PromiseArtifact will be updated to have the resolved artifact from the corresponding starlark promise.
#[derive(Debug, Trace, Allocative)]
pub struct PromiseArtifactRegistry<'v> {
    promises: Vec<ValueTyped<'v, StarlarkPromise<'v>>>,
    artifacts: Vec<(Option<FileSpan>, PromiseArtifact)>,
    owner: BaseDeferredKey,
}
impl<'v> PromiseArtifactRegistry<'v> {
    pub(crate) fn new(owner: BaseDeferredKey) -> Self {
        Self {
            owner,
            promises: Vec::new(),
            artifacts: Vec::new(),
        }
    }

    pub(crate) fn resolve_all(&self) -> anyhow::Result<()> {
        for (promise, (location, artifact)) in std::iter::zip(&self.promises, &self.artifacts) {
            match promise.get() {
                Some(v) => match v.as_artifact() {
                    Some(v) => {
                        artifact.resolve(v)?;
                    }
                    None => {
                        return Err(PromiseArtifactResolveError::NotAnArtifact(
                            location.clone(),
                            v.to_repr(),
                        )
                        .into());
                    }
                },
                None => {
                    return Err(PromiseArtifactResolveError::PromiseNotResolved(
                        location.clone(),
                        promise.to_string(),
                    )
                    .into());
                }
            }
        }
        Ok(())
    }

    pub(crate) fn register(
        &mut self,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        location: Option<FileSpan>,
    ) -> anyhow::Result<PromiseArtifact> {
        let id = PromiseArtifactId::new(self.owner.dupe(), self.promises.len());
        let artifact = PromiseArtifact::new(Arc::new(OnceCell::new()), Arc::new(id));

        self.promises.push(promise);
        self.artifacts.push((location, artifact.dupe()));
        Ok(artifact)
    }
}
