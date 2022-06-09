/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use buck2_build_api_derive::internal_provider;
use gazebo::{any::ProvidesStaticType, coerce::Coerce};
use starlark::{
    environment::GlobalsBuilder,
    values::{list::FrozenList, Freeze, FrozenRef, Trace, Value},
};
use thiserror::Error;

use crate::interpreter::rule_defs::provider::execution_platform_info::FrozenExecutionPlatformInfo;

#[derive(Debug, Error)]
enum ExecutionPlatformRegistrationTypeError {
    #[error("expected a list of ExecutionPlatformInfo, got `{0}` (type `{1}`)")]
    ExpectedListOfPlatforms(String, String),
    #[error("expected a ExecutionPlatformInfo, got `{0}` (type `{1}`)")]
    NotAPlatform(String, String),
}

/// Provider that gives the list of all execution platforms available for this build.
#[internal_provider(info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType)]
#[repr(C)]
pub(crate) struct ExecutionPlatformRegistrationInfoGen<V> {
    platforms: V, // List<ExecutionPlatformInfo>
}

impl FrozenExecutionPlatformRegistrationInfo {
    // TODO(cjhopman): If we impl this on the non-frozen one, we can check validity when constructed rather than only when used.
    pub fn platforms(
        &self,
    ) -> anyhow::Result<Vec<FrozenRef<'static, FrozenExecutionPlatformInfo>>> {
        FrozenList::from_frozen_value(&self.platforms)
            .ok_or_else(|| {
                ExecutionPlatformRegistrationTypeError::ExpectedListOfPlatforms(
                    self.platforms.to_value().to_repr(),
                    self.platforms.to_value().get_type().to_owned(),
                )
            })?
            .iter()
            .map(|v| {
                v.unpack_frozen()
                    .expect("should be frozen")
                    .downcast_frozen_ref::<FrozenExecutionPlatformInfo>()
                    .ok_or_else(|| {
                        anyhow::anyhow!(ExecutionPlatformRegistrationTypeError::NotAPlatform(
                            v.to_repr(),
                            v.get_type().to_owned(),
                        ))
                    })
            })
            .collect::<anyhow::Result<_>>()
    }
}

#[starlark_module]
fn info_creator(globals: &mut GlobalsBuilder) {
    fn ExecutionPlatformRegistrationInfo<'v>(
        #[starlark(require = named)] platforms: Value<'v>,
    ) -> anyhow::Result<ExecutionPlatformRegistrationInfo<'v>> {
        Ok(ExecutionPlatformRegistrationInfo { platforms })
    }
}
