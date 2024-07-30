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
use buck2_error::internal_error;
use dupe::Dupe;
use indexmap::IndexSet;
use starlark::values::any_complex::StarlarkAnyComplex;
use starlark::values::OwnedFrozenValueTyped;

use crate::deferred::types::DeferredInput;
use crate::dynamic::params::FrozenDynamicLambdaParams;

/// The lambda captured by `dynamic_output`, alongside the other required data.
#[derive(Debug, Allocative)]
pub struct DynamicLambda {
    /// the owner that defined this lambda
    pub owner: BaseDeferredKey,
    /// Things required by the lambda (wrapped in DeferredInput)
    pub dynamic: IndexSet<DeferredInput>,
    /// Things I produce
    pub outputs: Box<[BuildArtifact]>,
    /// A Starlark pair of the attributes and a lambda function that binds the outputs given a context
    pub attributes_lambda:
        Option<OwnedFrozenValueTyped<StarlarkAnyComplex<FrozenDynamicLambdaParams>>>,
}

impl DynamicLambda {
    pub fn new(
        owner: BaseDeferredKey,
        dynamic: IndexSet<Artifact>,
        outputs: Box<[BuildArtifact]>,
    ) -> Self {
        let mut depends = IndexSet::with_capacity(dynamic.len() + 1);
        match &owner {
            BaseDeferredKey::TargetLabel(target) => {
                depends.insert(DeferredInput::ConfiguredTarget(target.dupe()));
            }
            BaseDeferredKey::BxlLabel(_) => {
                // Execution platform resolution is handled when we execute the DynamicLambda
            }
            BaseDeferredKey::AnonTarget(_) => {
                // This will return an error later, so doesn't need to have the dependency
            }
        }
        depends.extend(dynamic.into_iter().map(DeferredInput::MaterializedArtifact));
        Self {
            owner,
            dynamic: depends,
            outputs,
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
