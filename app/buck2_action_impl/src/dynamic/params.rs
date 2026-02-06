/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api::dynamic_value::DynamicValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::FrozenStarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use buck2_build_api::interpreter::rule_defs::plugins::AnalysisPlugins;
use buck2_build_api::interpreter::rule_defs::plugins::FrozenAnalysisPlugins;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_error::internal_error;
use gazebo::prelude::OptionExt;
use starlark::any::ProvidesStaticType;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueOfUnchecked;
use starlark::values::FrozenValueTyped;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::ValueTypedComplex;
use starlark::values::structs::StructRef;
use starlark::values::typing::FrozenStarlarkCallable;
use starlark::values::typing::StarlarkCallable;

use crate::dynamic::attrs::DynamicAttrValues;
use crate::dynamic::dynamic_actions_callable::FrozenStarlarkDynamicActionsCallable;

#[derive(Allocative, Debug)]
pub(crate) struct DynamicLambdaStaticFields {
    /// Input artifacts required to be materialized by the lambda.
    pub(crate) artifact_values: Box<[Artifact]>,
    /// Dynamic values I depend on.
    pub(crate) dynamic_values: Box<[DynamicValue]>,
    /// Execution platform inherited from the owner to use for actionsfbcode/buck2/app/buck2_action_impl/src/dynamic/deferred.rs
    pub(crate) execution_platform: ExecutionPlatformResolution,
}

#[derive(Allocative, Trace, Debug, ProvidesStaticType)]
pub(crate) struct DynamicLambdaParams<'v> {
    pub(crate) attributes: Option<ValueOfUnchecked<'v, StructRef<'static>>>,
    pub(crate) plugins: Option<ValueTypedComplex<'v, AnalysisPlugins<'v>>>,
    pub(crate) lambda: StarlarkCallable<'v>,
    pub(crate) attr_values: Option<(
        DynamicAttrValues<Value<'v>>,
        FrozenValueTyped<'v, FrozenStarlarkDynamicActionsCallable>,
    )>,
    pub(crate) outputs: Box<[ValueTyped<'v, StarlarkOutputArtifact<'v>>]>,
    pub(crate) static_fields: DynamicLambdaStaticFields,
}

#[derive(Allocative, Debug, ProvidesStaticType)]
pub struct FrozenDynamicLambdaParams {
    pub(crate) attributes: Option<FrozenValueOfUnchecked<'static, StructRef<'static>>>,
    pub(crate) plugins: Option<FrozenValueTyped<'static, FrozenAnalysisPlugins>>,
    pub(crate) lambda: FrozenStarlarkCallable,
    pub attr_values: Option<(
        DynamicAttrValues<FrozenValue>,
        FrozenValueTyped<'static, FrozenStarlarkDynamicActionsCallable>,
    )>,
    pub(crate) outputs: Box<[FrozenValueTyped<'static, FrozenStarlarkOutputArtifact>]>,
    pub(crate) static_fields: DynamicLambdaStaticFields,
}

impl FrozenDynamicLambdaParams {
    pub(crate) fn attributes<'v>(
        &'v self,
    ) -> buck2_error::Result<Option<ValueOfUnchecked<'v, StructRef<'static>>>> {
        let Some(attributes) = self.attributes else {
            return Ok(None);
        };
        Ok(Some(attributes.to_value().cast()))
    }

    pub(crate) fn plugins<'v>(
        &'v self,
    ) -> buck2_error::Result<Option<ValueTypedComplex<'v, AnalysisPlugins<'v>>>> {
        let Some(plugins) = self.plugins else {
            return Ok(None);
        };
        Ok(Some(
            ValueTypedComplex::new(plugins.to_value())
                .ok_or_else(|| internal_error!("plugins must be AnalysisPlugins"))?,
        ))
    }

    pub fn lambda<'v>(&'v self) -> Value<'v> {
        self.lambda.0.to_value()
    }
}

impl<'v> Freeze for DynamicLambdaParams<'v> {
    type Frozen = FrozenDynamicLambdaParams;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let attr_values = match self.attr_values {
            None => None,
            Some((attr_values, callable)) => Some((
                attr_values.freeze(freezer)?,
                // Change lifetime.
                FrozenValueTyped::new_err(callable.to_frozen_value())
                    .map_err(|e| FreezeError::new(e.to_string()))?,
            )),
        };
        Ok(FrozenDynamicLambdaParams {
            attributes: self.attributes.try_map(|a| Ok(a.freeze(freezer)?.cast()))?,
            plugins: self.plugins.freeze(freezer)?,
            lambda: self.lambda.freeze(freezer)?,
            attr_values,
            outputs: self
                .outputs
                .into_iter()
                .map(|o| o.freeze(freezer))
                .collect::<FreezeResult<_>>()?,
            static_fields: self.static_fields,
        })
    }
}
