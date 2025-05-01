/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use allocative::Allocative;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_util::late_binding::LateBinding;
use starlark::any::AnyLifetime;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::Trace;

pub trait DynamicLambdaParamsStorage<'v>: Trace<'v> + Debug + Allocative + Send + 'v {
    fn as_any_mut(&mut self) -> &mut dyn AnyLifetime<'v>;

    fn freeze(
        self: Box<Self>,
        freezer: &Freezer,
    ) -> FreezeResult<Box<dyn FrozenDynamicLambdaParamsStorage>>;
}

pub trait FrozenDynamicLambdaParamsStorage: Debug + Allocative + Send + Sync + 'static {
    fn as_any(&self) -> &dyn AnyLifetime<'static>;

    fn iter_dynamic_lambda_outputs(&self) -> Box<dyn Iterator<Item = BuildArtifact> + Send + '_>;
}

pub trait DynamicLambdaParamStorages: Send + Sync + 'static {
    fn new_dynamic_lambda_params_storage<'v>(
        &self,
        self_key: DeferredHolderKey,
    ) -> Box<dyn DynamicLambdaParamsStorage<'v>>;
    fn new_frozen_dynamic_lambda_params_storage(
        &self,
        self_key: DeferredHolderKey,
    ) -> Box<dyn FrozenDynamicLambdaParamsStorage>;
}

pub static DYNAMIC_LAMBDA_PARAMS_STORAGES: LateBinding<&'static dyn DynamicLambdaParamStorages> =
    LateBinding::new("DYNAMIC_LAMBDA_PARAMS_STORAGES");
