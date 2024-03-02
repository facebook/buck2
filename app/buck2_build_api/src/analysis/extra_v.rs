/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::OnceCell;

use allocative::Allocative;
use buck2_error::BuckErrorContext;
use gazebo::prelude::OptionExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::values::starlark_value;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenValueTyped;
use starlark::values::NoSerialize;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;

use crate::analysis::registry::AnalysisValueStorage;
use crate::analysis::registry::FrozenAnalysisValueStorage;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use crate::interpreter::rule_defs::provider::collection::ProviderCollection;

#[derive(
    Default,
    Debug,
    derive_more::Display,
    ProvidesStaticType,
    Allocative,
    Trace,
    NoSerialize
)]
#[display(fmt = "{:?}", "self")]
pub struct AnalysisExtraValue<'v> {
    /// Populated after running rule function to get the providers frozen.
    pub provider_collection: OnceCell<ValueTyped<'v, ProviderCollection<'v>>>,
    pub(crate) analysis_value_storage: OnceCell<ValueTyped<'v, AnalysisValueStorage<'v>>>,
}

#[derive(
    Debug,
    derive_more::Display,
    ProvidesStaticType,
    Allocative,
    NoSerialize
)]
#[display(fmt = "{:?}", "self")]
pub struct FrozenAnalysisExtraValue {
    pub provider_collection: Option<FrozenValueTyped<'static, FrozenProviderCollection>>,
    pub(crate) analysis_value_storage:
        Option<FrozenValueTyped<'static, FrozenAnalysisValueStorage>>,
}

#[starlark_value(type = "AnalysisExtraValue")]
impl<'v> StarlarkValue<'v> for AnalysisExtraValue<'v> {}

#[starlark_value(type = "AnalysisExtraValue")]
impl<'v> StarlarkValue<'v> for FrozenAnalysisExtraValue {
    type Canonical = AnalysisExtraValue<'v>;
}

impl<'v> Freeze for AnalysisExtraValue<'v> {
    type Frozen = FrozenAnalysisExtraValue;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let AnalysisExtraValue {
            provider_collection,
            analysis_value_storage,
        } = self;
        let provider_collection =
            provider_collection
                .into_inner()
                .try_map(|provider_collection| {
                    FrozenValueTyped::new_err(provider_collection.to_value().freeze(freezer)?)
                })?;
        let analysis_value_storage =
            analysis_value_storage
                .into_inner()
                .try_map(|analysis_value_storage| {
                    FrozenValueTyped::new_err(analysis_value_storage.to_value().freeze(freezer)?)
                })?;
        Ok(FrozenAnalysisExtraValue {
            provider_collection,
            analysis_value_storage,
        })
    }
}

impl<'v> AnalysisExtraValue<'v> {
    pub fn get(module: &'v Module) -> anyhow::Result<Option<&'v AnalysisExtraValue<'v>>> {
        let Some(extra) = module.extra_value() else {
            return Ok(None);
        };
        Ok(Some(extra.downcast_ref_err()?))
    }

    pub fn get_or_init(module: &'v Module) -> anyhow::Result<&'v AnalysisExtraValue<'v>> {
        if let Some(extra) = Self::get(module)? {
            return Ok(extra);
        }
        module.set_extra_value_no_overwrite(
            module.heap().alloc_complex(AnalysisExtraValue::default()),
        )?;
        Self::get(module)?.internal_error("extra_value must be set")
    }
}

impl FrozenAnalysisExtraValue {
    pub fn get(
        module: &FrozenModule,
    ) -> anyhow::Result<OwnedFrozenValueTyped<FrozenAnalysisExtraValue>> {
        module
            .owned_extra_value()
            .internal_error("extra_value not set")?
            .downcast_anyhow()
    }
}
