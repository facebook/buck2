/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::collections::HashMap;
use std::fmt::Debug;
use std::slice;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::deferred::key::DeferredHolderKey;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::internal_error;
use buck2_execute::digest_config::DigestConfig;
use buck2_futures::cancellable_future::CancellationObserver;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use dice::DiceComputations;
use dupe::Dupe;
use either::Either;
use indexmap::IndexSet;

use crate::analysis::registry::RecordedAnalysisValues;

/// An asynchronous chunk of work that will be executed when requested.
/// The 'Deferred' can have "inputs" which are values that will be guaranteed to be ready to use
/// before the 'Deferred' is actually executed. These can be 'Artifact's, which means that those
/// 'Artifact's will be materialized and its corresponding 'Action's executed, or other
/// 'DeferredData', which means those 'Deferred' will be computed first.
///
/// `any::Provider` can be used to obtain data for introspection.
#[async_trait]
pub trait Deferred: Debug + Allocative + provider::Provider + Send + Sync + Any + 'static {
    /// the set of 'Deferred's that should be computed first before executing this 'Deferred'
    fn inputs(&self) -> DeferredInputsRef<'_>;

    /// executes this 'Deferred', assuming all inputs and input artifacts are already computed
    async fn execute(
        &self,
        dice: &mut DiceComputations,
        action_key: String,
        configured_targets: HashMap<ConfiguredTargetLabel, ConfiguredTargetNode>,
        materialized_artifacts: HashMap<Artifact, ProjectRelativePathBuf>,
        registry: &mut DeferredRegistry,
        project_filesystem: ProjectRoot,
        digest_config: DigestConfig,
        liveness: CancellationObserver,
    ) -> anyhow::Result<()>;

    /// An optional stage to wrap execution in.
    fn span(&self) -> Option<buck2_data::span_start_event::Data> {
        None
    }
}

/// input to a deferred that needs to be computed first before executing
#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative)]
pub enum DeferredInput {
    ConfiguredTarget(ConfiguredTargetLabel),
    /// Materialized artifact is an input of `dynamic_output`.
    /// Regular actions like `run` do not have `MaterializedArtifact` inputs.
    MaterializedArtifact(Artifact),
}

#[derive(Copy, Clone, Dupe)]
pub enum DeferredInputsRef<'a> {
    IndexSet(&'a IndexSet<DeferredInput>),
    Slice(&'a [DeferredInput]),
}

impl<'a> IntoIterator for DeferredInputsRef<'a> {
    type Item = &'a DeferredInput;
    type IntoIter = Either<indexmap::set::Iter<'a, DeferredInput>, slice::Iter<'a, DeferredInput>>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            DeferredInputsRef::IndexSet(set) => Either::Left(set.iter()),
            DeferredInputsRef::Slice(slice) => Either::Right(slice.iter()),
        }
    }
}

/// The registry for creating 'DeferredData's and registering 'Deferred's
#[derive(Allocative)]
pub struct DeferredRegistry {
    base_key: DeferredHolderKey,
    recorded_values: Option<RecordedAnalysisValues>,
}

impl DeferredRegistry {
    pub fn new(base_key: DeferredHolderKey) -> Self {
        Self {
            base_key,
            recorded_values: None,
        }
    }

    pub fn key(&self) -> &DeferredHolderKey {
        &self.base_key
    }

    pub fn take_result(self) -> anyhow::Result<RecordedAnalysisValues> {
        let values = self
            .recorded_values
            // TODO(cjhopman): We should require this to be set, but a bunch of non-analysis things still use deferreds.
            .unwrap_or_else(RecordedAnalysisValues::new_empty);
        Ok(values)
    }

    pub(crate) fn register_values(&mut self, values: RecordedAnalysisValues) -> anyhow::Result<()> {
        match self.recorded_values.replace(values) {
            // TODO(cjhopman): delete this error in this stack
            Some(_) => Err(internal_error!("recorded analysis values already set")),
            None => Ok(()),
        }
    }
}

#[derive(Debug, buck2_error::Error)]
pub enum DeferredErrors {
    #[error("no deferred found for deferred id `{0}`")]
    DeferredNotFound(u32),
    #[error("reserved deferred id of `{0:?}` was never bound")]
    UnboundReservedDeferred(usize),
}
