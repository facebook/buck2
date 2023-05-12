/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use dupe::Dupe;
use once_cell::sync::OnceCell;
use starlark::codemap::FileSpan;
use thiserror::Error;

use crate::actions::artifact::artifact_type::Artifact;
use crate::deferred::base_deferred_key::BaseDeferredKey;
use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;

#[derive(Debug, Error)]
pub(crate) enum PromiseArtifactResolveError {
    #[error("artifact_promise(){} resolved promise was not an artifact (was `{1}`)", maybe_declared_at(&_0))]
    NotAnArtifact(Option<FileSpan>, String),
    #[error("artifact_promise(){1}{} promise wasn't resolved", maybe_declared_at(&_0))]
    PromiseNotResolved(Option<FileSpan>, String),
    #[error("artifact_promise() resolved multiple times")]
    AlreadyResolved,
    #[error(
        "artifact_promise(){0} is not yet resolved, this indicates it was used as an artifact during the analysis where it was declared"
    )]
    PromiseNotYetResolved(PromiseArtifact),
    #[error(
        "artifact_promise() resolved to artifact with associated artifacts, this isn't allowed"
    )]
    HasAssociatedArtifacts,
    #[error(
        "artifact_promise() resolved to a source artifact, this isn't allowed: an artifact_promise() must resolve to a build artifact"
    )]
    SourceArtifact,
}

fn maybe_declared_at(location: &Option<FileSpan>) -> String {
    match location {
        Some(v) => format!(" (declared at {})", v),
        None => String::new(),
    }
}

/// A PromiseArtifact is used to assert that a starlark promise will resolve to an artifact. While the analysis
/// that declares the promise is running, the promise may not yet be resolved. In that case, operations that require
/// the underlying artifact will fail. Once that analysis is complete, all promises will be resolved and this will
/// have a reference to the fully resolved artifact.
#[derive(Clone, Debug, Dupe, Allocative)]
pub struct PromiseArtifact {
    artifact: Arc<OnceCell<Artifact>>,
    id: Arc<PromiseArtifactId>,
}

#[derive(Clone, Debug, Dupe, Allocative, Hash, Eq, PartialEq)]
pub struct PromiseArtifactId {
    owner: BaseDeferredKey,
    id: usize,
}
impl PromiseArtifactId {
    pub(crate) fn new(owner: BaseDeferredKey, id: usize) -> PromiseArtifactId {
        Self { owner, id }
    }
}

impl PromiseArtifact {
    pub fn new(artifact: Arc<OnceCell<Artifact>>, id: Arc<PromiseArtifactId>) -> Self {
        Self { artifact, id }
    }

    pub fn get_err(&self) -> anyhow::Result<&Artifact> {
        match self.artifact.get() {
            Some(v) => Ok(v),
            None => Err(PromiseArtifactResolveError::PromiseNotYetResolved(self.clone()).into()),
        }
    }

    pub fn unwrap(&self) -> &Artifact {
        self.get_err().unwrap()
    }

    pub fn get(&self) -> Option<&Artifact> {
        self.artifact.get()
    }

    pub(crate) fn resolve(&self, artifact: &dyn StarlarkArtifactLike) -> anyhow::Result<()> {
        if let Some(v) = artifact.get_associated_artifacts() {
            if !v.is_empty() {
                return Err(PromiseArtifactResolveError::HasAssociatedArtifacts.into());
            }
        }
        let bound = artifact
            .get_bound_artifact()
            .context("expected bound artifact for promise_artifact resolve")?;
        if bound.is_source() {
            return Err(PromiseArtifactResolveError::SourceArtifact.into());
        }
        match self.artifact.set(bound) {
            Ok(_) => Ok(()),
            Err(_) => Err(PromiseArtifactResolveError::AlreadyResolved.into()),
        }
    }

    pub(crate) fn id(&self) -> &PromiseArtifactId {
        self.id.as_ref()
    }
}

impl Display for PromiseArtifact {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(artifact) = self.artifact.get() {
            write!(f, "PromiseArtifact(Resolved({}))", artifact)
        } else {
            write!(
                f,
                "PromiseArtifact(Unresolved({}, {}))",
                &self.id.owner, self.id.id
            )
        }
    }
}

impl Hash for PromiseArtifact {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for PromiseArtifact {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for PromiseArtifact {}
