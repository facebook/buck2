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
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::analysis::registry::AnalysisValueStorage;
use buck2_build_api::analysis::registry::FrozenAnalysisValueStorage;
use buck2_build_api::dynamic::storage::DYNAMIC_LAMBDA_PARAMS_STORAGES;
use buck2_build_api::dynamic::storage::DynamicLambdaParamStorages;
use buck2_build_api::dynamic::storage::DynamicLambdaParamsStorage;
use buck2_build_api::dynamic::storage::FrozenDynamicLambdaParamsStorage;
use buck2_core::deferred::dynamic::DynamicLambdaIndex;
use buck2_core::deferred::dynamic::DynamicLambdaResultsKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_error::internal_error;
use dupe::Dupe;
use starlark::any::AnyLifetime;
use starlark::any::ProvidesStaticType;
use starlark::values::DynStarlark;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::OwnedRefFrozenRef;
use starlark::values::Trace;
use starlark::values::Tracer;
use starlark_map::small_map::SmallMap;

use crate::dynamic::params::DynamicLambdaParams;
use crate::dynamic::params::FrozenDynamicLambdaParams;

#[derive(Debug, Allocative, ProvidesStaticType)]
pub(crate) struct DynamicLambdaParamsStorageImpl<'v> {
    self_key: DeferredHolderKey,
    lambda_params: SmallMap<DynamicLambdaResultsKey, DynamicLambdaParams<'v>>,
}

#[derive(Debug, Allocative, ProvidesStaticType)]
pub(crate) struct FrozenDynamicLambdaParamsStorageImpl {
    lambda_params: SmallMap<DynamicLambdaResultsKey, FrozenDynamicLambdaParams>,
}

impl<'v> DynamicLambdaParamsStorageImpl<'v> {
    pub(crate) fn get<'a>(
        storage: &'a mut AnalysisValueStorage<'v>,
    ) -> buck2_error::Result<&'a mut DynamicLambdaParamsStorageImpl<'v>> {
        storage
            .lambda_params
            .as_any_mut()
            .downcast_mut()
            .ok_or_else(|| internal_error!("Wrong type for lambda params storage"))
    }

    pub fn next_dynamic_actions_key(&self) -> buck2_error::Result<DynamicLambdaResultsKey> {
        let index = DynamicLambdaIndex::new(self.lambda_params.len().try_into()?);
        Ok(DynamicLambdaResultsKey::new(self.self_key.dupe(), index))
    }

    pub fn set_dynamic_actions(
        &mut self,
        key: DynamicLambdaResultsKey,
        lambda_params: DynamicLambdaParams<'v>,
    ) -> buck2_error::Result<()> {
        if &self.self_key != key.holder_key() {
            return Err(internal_error!(
                "Wrong lambda owner: expecting `{}`, got `{}`",
                self.self_key,
                key
            ));
        }
        self.lambda_params.insert(key, lambda_params);
        Ok(())
    }
}

impl FrozenDynamicLambdaParamsStorageImpl {
    pub(crate) fn lookup_lambda<'f>(
        storage: OwnedRefFrozenRef<'f, FrozenAnalysisValueStorage>,
        key: &DynamicLambdaResultsKey,
    ) -> buck2_error::Result<OwnedRefFrozenRef<'f, FrozenDynamicLambdaParams>> {
        if key.holder_key() != &storage.as_ref().self_key {
            return Err(internal_error!(
                "Wrong owner for lambda: expecting `{}`, got `{}`",
                storage.as_ref().self_key,
                key
            ));
        }
        storage.try_map_result(|s| {
            s.lambda_params
                .as_any()
                .downcast_ref::<FrozenDynamicLambdaParamsStorageImpl>()
                .ok_or_else(|| internal_error!("Wrong type for lambda params storage"))?
                .lambda_params
                .get(key)
                .ok_or_else(|| internal_error!("missing lambda `{key}`"))
        })
    }
}

unsafe impl<'v> Trace<'v> for DynamicLambdaParamsStorageImpl<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        let DynamicLambdaParamsStorageImpl {
            self_key,
            lambda_params,
        } = self;
        tracer.trace_static(self_key);
        for (k, v) in lambda_params.iter_mut() {
            tracer.trace_static(k);
            v.trace(tracer);
        }
    }
}

impl<'v> DynamicLambdaParamsStorage<'v> for DynamicLambdaParamsStorageImpl<'v> {
    fn as_any_mut(&mut self) -> &mut dyn AnyLifetime<'v> {
        self
    }

    fn freeze(
        self: Box<DynStarlark<'v, Self>>,
        freezer: &Freezer,
    ) -> FreezeResult<Box<dyn FrozenDynamicLambdaParamsStorage>> {
        let DynamicLambdaParamsStorageImpl {
            lambda_params,
            self_key: _,
        } = self.into_inner();
        let lambda_params = lambda_params
            .into_iter_hashed()
            .map(|(k, v)| Ok((k, v.freeze(freezer)?)))
            .collect::<FreezeResult<_>>()?;
        Ok(Box::new(FrozenDynamicLambdaParamsStorageImpl {
            lambda_params,
        }))
    }
}

impl FrozenDynamicLambdaParamsStorage for FrozenDynamicLambdaParamsStorageImpl {
    fn as_any(&self) -> &dyn AnyLifetime<'static> {
        self
    }

    fn iter_dynamic_lambda_outputs(&self) -> Box<dyn Iterator<Item = BuildArtifact> + Send + '_> {
        Box::new(
            self.lambda_params
                .values()
                .flat_map(|v| v.outputs.iter().map(|a| a.as_build_artifact().dupe())),
        )
    }
}

pub(crate) fn init_dynamic_lambda_params_storages() {
    struct Impl;

    impl DynamicLambdaParamStorages for Impl {
        fn new_dynamic_lambda_params_storage<'v>(
            &self,
            self_key: DeferredHolderKey,
        ) -> Box<DynStarlark<'v, dyn DynamicLambdaParamsStorage<'v>>> {
            Box::new(DynStarlark::new(DynamicLambdaParamsStorageImpl {
                self_key,
                lambda_params: SmallMap::new(),
            }))
        }

        fn new_frozen_dynamic_lambda_params_storage(
            &self,
        ) -> Box<dyn FrozenDynamicLambdaParamsStorage> {
            Box::new(FrozenDynamicLambdaParamsStorageImpl {
                lambda_params: SmallMap::new(),
            })
        }
    }

    DYNAMIC_LAMBDA_PARAMS_STORAGES.init(&Impl);
}
