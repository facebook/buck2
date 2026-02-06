/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::OnceCell;

use allocative::Allocative;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::internal_error;
use gazebo::prelude::OptionExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenValueTyped;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Trace;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::values::any_complex::StarlarkAnyComplex;

use crate::analysis::registry::AnalysisValueStorage;
use crate::analysis::registry::FrozenAnalysisValueStorage;

#[derive(Default, Debug, ProvidesStaticType, Allocative, Trace)]
pub struct AnalysisExtraValue<'v> {
    pub analysis_value_storage:
        OnceCell<ValueTyped<'v, StarlarkAnyComplex<AnalysisValueStorage<'v>>>>,
}

#[derive(Debug, ProvidesStaticType, Allocative)]
pub struct FrozenAnalysisExtraValue {
    pub(crate) analysis_value_storage:
        Option<FrozenValueTyped<'static, StarlarkAnyComplex<FrozenAnalysisValueStorage>>>,
}

impl<'v> Freeze for AnalysisExtraValue<'v> {
    type Frozen = FrozenAnalysisExtraValue;
    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let AnalysisExtraValue {
            analysis_value_storage,
        } = self;
        let analysis_value_storage =
            analysis_value_storage
                .into_inner()
                .try_map(|analysis_value_storage| {
                    Ok(FrozenValueTyped::new_err(
                        analysis_value_storage.to_value().freeze(freezer)?,
                    )
                    .map_err(|e| FreezeError::new(format!("{e}"))))?
                })?;
        Ok(FrozenAnalysisExtraValue {
            analysis_value_storage,
        })
    }
}

impl<'v> AnalysisExtraValue<'v> {
    pub fn get(module: &Module<'v>) -> buck2_error::Result<Option<&'v AnalysisExtraValue<'v>>> {
        let Some(extra) = module.extra_value() else {
            return Ok(None);
        };
        Ok(Some(
            &extra
                .downcast_ref_err::<StarlarkAnyComplex<AnalysisExtraValue>>()?
                .value,
        ))
    }

    pub fn get_or_init(module: &Module<'v>) -> buck2_error::Result<&'v AnalysisExtraValue<'v>> {
        if let Some(extra) = Self::get(module)? {
            return Ok(extra);
        }
        module
            .set_extra_value_no_overwrite(
                module
                    .heap()
                    .alloc(StarlarkAnyComplex::new(AnalysisExtraValue::default())),
            )
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        Self::get(module)?.ok_or_else(|| internal_error!("extra_value must be set"))
    }
}

impl FrozenAnalysisExtraValue {
    pub fn get(
        module: &FrozenModule,
    ) -> buck2_error::Result<OwnedFrozenValueTyped<StarlarkAnyComplex<FrozenAnalysisExtraValue>>>
    {
        Ok(module
            .owned_extra_value()
            .ok_or_else(|| internal_error!("extra_value not set"))?
            .downcast_starlark()?)
    }
}
