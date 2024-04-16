/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_value::StarlarkArtifactValue;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use buck2_build_api::interpreter::rule_defs::dynamic_value::DynamicValue;
use buck2_build_api::interpreter::rule_defs::plugins::AnalysisPlugins;
use buck2_build_api::interpreter::rule_defs::plugins::FrozenAnalysisPlugins;
use starlark::any::ProvidesStaticType;
use starlark::values::none::NoneType;
use starlark::values::starlark_value;
use starlark::values::structs::StructRef;
use starlark::values::typing::FrozenStarlarkCallable;
use starlark::values::typing::StarlarkCallable;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTypedComplex;
use starlark_map::small_map::SmallMap;

#[derive(
    Allocative,
    Trace,
    derive_more::Display,
    Debug,
    ProvidesStaticType,
    NoSerialize
)]
#[display(fmt = "{:?}", "self")]
pub(crate) struct DynamicLambdaParams<'v> {
    pub(crate) attributes: ValueOfUnchecked<'v, StructRef<'v>>,
    pub(crate) plugins: ValueTypedComplex<'v, AnalysisPlugins<'v>>,
    pub(crate) lambda: StarlarkCallable<
        'v,
        (
            FrozenValue,
            SmallMap<StarlarkArtifact, StarlarkArtifactValue>,
            SmallMap<DynamicValue, FrozenValue>,
            SmallMap<StarlarkArtifact, StarlarkDeclaredArtifact>,
        ),
        NoneType,
    >,
}

#[derive(
    Allocative,
    derive_more::Display,
    Debug,
    ProvidesStaticType,
    NoSerialize
)]
#[display(fmt = "{:?}", "self")]
pub(crate) struct FrozenDynamicLambdaParams {
    pub(crate) attributes: FrozenValue,
    pub(crate) plugins: FrozenValueTyped<'static, FrozenAnalysisPlugins>,
    pub(crate) lambda: FrozenStarlarkCallable<
        (
            FrozenValue,
            SmallMap<StarlarkArtifact, StarlarkArtifactValue>,
            SmallMap<DynamicValue, FrozenValue>,
            SmallMap<StarlarkArtifact, StarlarkDeclaredArtifact>,
        ),
        NoneType,
    >,
}

#[starlark_value(type = "AttributesPluginLambda")]
impl<'v> StarlarkValue<'v> for DynamicLambdaParams<'v> {}

#[starlark_value(type = "AttributesPluginLambda")]
impl<'v> StarlarkValue<'v> for FrozenDynamicLambdaParams {
    type Canonical = DynamicLambdaParams<'v>;
}

impl<'v> Freeze for DynamicLambdaParams<'v> {
    type Frozen = FrozenDynamicLambdaParams;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Ok(FrozenDynamicLambdaParams {
            attributes: self.attributes.get().freeze(freezer)?,
            plugins: self.plugins.freeze(freezer)?,
            lambda: self.lambda.freeze(freezer)?,
        })
    }
}
