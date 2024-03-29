/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
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

use crate::interpreter::rule_defs::plugins::AnalysisPlugins;
use crate::interpreter::rule_defs::plugins::FrozenAnalysisPlugins;

#[derive(
    Allocative,
    Trace,
    derive_more::Display,
    Debug,
    ProvidesStaticType,
    NoSerialize
)]
#[display(fmt = "{:?}", "self")]
pub struct DynamicLambdaParams<'v> {
    pub attributes: ValueOfUnchecked<'v, StructRef<'v>>,
    pub plugins: ValueTypedComplex<'v, AnalysisPlugins<'v>>,
    pub lambda: StarlarkCallable<'v, (FrozenValue, FrozenValue, FrozenValue), NoneType>,
}

#[derive(
    Allocative,
    derive_more::Display,
    Debug,
    ProvidesStaticType,
    NoSerialize
)]
#[display(fmt = "{:?}", "self")]
pub struct FrozenDynamicLambdaParams {
    pub attributes: FrozenValue,
    pub plugins: FrozenValueTyped<'static, FrozenAnalysisPlugins>,
    pub lambda: FrozenStarlarkCallable<(FrozenValue, FrozenValue, FrozenValue), NoneType>,
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
