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
use buck2_build_api::interpreter::rule_defs::plugins::AnalysisPlugins;
use buck2_build_api::interpreter::rule_defs::plugins::FrozenAnalysisPlugins;
use starlark::any::ProvidesStaticType;
use starlark::values::none::NoneType;
use starlark::values::structs::StructRef;
use starlark::values::typing::FrozenStarlarkCallable;
use starlark::values::typing::StarlarkCallable;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Trace;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTypedComplex;
use starlark_map::small_map::SmallMap;

#[derive(Allocative, Trace, Debug, ProvidesStaticType)]
pub(crate) struct DynamicLambdaParams<'v> {
    pub(crate) attributes: ValueOfUnchecked<'v, StructRef<'v>>,
    pub(crate) plugins: ValueTypedComplex<'v, AnalysisPlugins<'v>>,
    pub(crate) lambda: StarlarkCallable<
        'v,
        (
            FrozenValue,
            SmallMap<StarlarkArtifact, StarlarkArtifactValue>,
            SmallMap<StarlarkArtifact, StarlarkDeclaredArtifact>,
        ),
        NoneType,
    >,
}

#[derive(Allocative, Debug, ProvidesStaticType)]
pub struct FrozenDynamicLambdaParams {
    pub(crate) attributes: FrozenValue,
    pub(crate) plugins: FrozenValueTyped<'static, FrozenAnalysisPlugins>,
    pub lambda: FrozenStarlarkCallable<
        (
            FrozenValue,
            SmallMap<StarlarkArtifact, StarlarkArtifactValue>,
            SmallMap<StarlarkArtifact, StarlarkDeclaredArtifact>,
        ),
        NoneType,
    >,
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
