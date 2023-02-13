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
use buck2_build_api_derive::internal_provider;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::values::list::ListRef;
use starlark::values::Freeze;
use starlark::values::FrozenRef;
use starlark::values::Trace;
use starlark::values::Value;
use thiserror::Error;

use crate::configuration::ExecutionPlatformFallback;
use crate::interpreter::rule_defs::provider::builtin::execution_platform_info::ExecutionPlatformInfo;
use crate::interpreter::rule_defs::provider::builtin::execution_platform_info::FrozenExecutionPlatformInfo;

#[derive(Debug, Error)]
enum ExecutionPlatformRegistrationTypeError {
    #[error("expected a list of ExecutionPlatformInfo, got `{0}` (type `{1}`)")]
    ExpectedListOfPlatforms(String, String),
    #[error("expected a ExecutionPlatformInfo, got `{0}` (type `{1}`)")]
    NotAPlatform(String, String),
    #[error(
        "expected an ExecutionPlatformInfo or one of the strings \"error\" or \"use_unspecified\", got `{0}` (type `{1}`)"
    )]
    InvalidFallback(String, String),
}

/// Provider that gives the list of all execution platforms available for this build.
#[internal_provider(info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[repr(C)]
pub(crate) struct ExecutionPlatformRegistrationInfoGen<V> {
    #[provider(field_type = "Vec<FrozenExecutionPlatformInfo>")]
    platforms: V,
    // OneOf<ExecutionPlatformInfo, \"error\", \"unspecified\", None>
    fallback: V,
}

impl FrozenExecutionPlatformRegistrationInfo {
    // TODO(cjhopman): If we impl this on the non-frozen one, we can check validity when constructed rather than only when used.
    pub fn platforms(
        &self,
    ) -> anyhow::Result<Vec<FrozenRef<'static, FrozenExecutionPlatformInfo>>> {
        ListRef::from_frozen_value(self.platforms)
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

    pub fn fallback(&self) -> anyhow::Result<ExecutionPlatformFallback> {
        if self.fallback.is_none() {
            return Ok(ExecutionPlatformFallback::UseUnspecifiedExec);
        }
        let fallback = self.fallback.to_value();
        if let Some(v) = ExecutionPlatformInfo::from_value(fallback) {
            return Ok(ExecutionPlatformFallback::Platform(
                v.to_execution_platform()?,
            ));
        }

        match fallback.unpack_str() {
            Some("error") => Ok(ExecutionPlatformFallback::Error),
            Some("use_unspecified") => Ok(ExecutionPlatformFallback::UseUnspecifiedExec),
            _ => Err(anyhow::anyhow!(
                ExecutionPlatformRegistrationTypeError::InvalidFallback(
                    fallback.to_repr(),
                    fallback.get_type().to_owned(),
                )
            )),
        }
    }
}

#[starlark_module]
fn info_creator(globals: &mut GlobalsBuilder) {
    fn ExecutionPlatformRegistrationInfo<'v>(
        #[starlark(require = named)] platforms: Value<'v>,
        #[starlark(require = named)] fallback: Option<Value<'v>>,
    ) -> anyhow::Result<ExecutionPlatformRegistrationInfo<'v>> {
        Ok(ExecutionPlatformRegistrationInfo {
            platforms,
            fallback: fallback.unwrap_or_else(Value::new_none),
        })
    }
}
