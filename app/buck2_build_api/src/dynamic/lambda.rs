/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_error::internal_error;
use indexmap::IndexSet;
use starlark::values::any_complex::StarlarkAnyComplex;
use starlark::values::OwnedFrozenValueTyped;

use crate::dynamic::params::FrozenDynamicLambdaParams;

/// The lambda captured by `dynamic_output`, alongside the other required data.
#[derive(Debug, Allocative)]
pub struct DynamicLambda {
    /// the owner that defined this lambda
    pub owner: BaseDeferredKey,
    /// Input artifacts required to be materialized by the lambda.
    pub dynamic: IndexSet<Artifact>,
    /// Things I produce
    pub outputs: Box<[BuildArtifact]>,
    /// Execution platform inherited from the owner to use for actionsfbcode/buck2/app/buck2_action_impl/src/dynamic/deferred.rs
    pub execution_platform: ExecutionPlatformResolution,
    /// A Starlark pair of the attributes and a lambda function that binds the outputs given a context
    pub attributes_lambda:
        Option<OwnedFrozenValueTyped<StarlarkAnyComplex<FrozenDynamicLambdaParams>>>,
}

impl DynamicLambda {
    pub fn new(
        owner: BaseDeferredKey,
        dynamic: IndexSet<Artifact>,
        outputs: Box<[BuildArtifact]>,
        execution_platform: ExecutionPlatformResolution,
    ) -> Self {
        Self {
            owner,
            dynamic,
            outputs,
            execution_platform,
            attributes_lambda: None,
        }
    }

    pub fn outputs(&self) -> &[BuildArtifact] {
        &self.outputs
    }

    pub fn bind(
        &mut self,
        attributes_lambda: OwnedFrozenValueTyped<StarlarkAnyComplex<FrozenDynamicLambdaParams>>,
    ) -> anyhow::Result<()> {
        if self.attributes_lambda.is_some() {
            return Err(internal_error!("`attributes_lambda` field already set"));
        }
        self.attributes_lambda = Some(attributes_lambda);
        Ok(())
    }
}

#[derive(Debug, buck2_error::Error)]
pub enum DynamicLambdaError {
    #[error("dynamic_output and anon_target cannot be used together (yet)")]
    AnonTargetIncompatible,
    #[error("dynamic_output lambda must return `None`, got: `{0}`")]
    LambdaMustReturnNone(String),
}
