/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use buck2_core::configuration::data::ConfigurationData;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::values::FreezeBranded;
use starlark::values::Heap;
use starlark::values::StarlarkPagable;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::provider::builtin::configuration_info::ConfigurationInfo;

#[internal_provider(platform_info_creator)]
#[derive(
    Clone,
    Debug,
    Trace,
    FreezeBranded,
    ProvidesStaticType,
    Allocative,
    StarlarkPagable
)]
#[repr(C)]
pub struct PlatformInfo<'v> {
    label: ValueOfUnchecked<'v, String>,
    configuration: ValueTyped<'v, ConfigurationInfo<'v>>,
}

impl<'v> PlatformInfo<'v> {
    pub fn to_configuration(
        &self,
        is_marked_as_exec_platform: bool,
    ) -> buck2_error::Result<ConfigurationData> {
        let label = self
            .label
            .get()
            .unpack_str()
            .expect("type checked during construction")
            .to_owned();
        let data = self.configuration.to_configuration_data()?;
        ConfigurationData::from_platform(label, data, is_marked_as_exec_platform)
    }

    pub fn from_configuration(
        cfg: &ConfigurationData,
        heap: Heap<'v>,
    ) -> buck2_error::Result<PlatformInfo<'v>> {
        let label = heap.alloc_str(cfg.label()?);
        Ok(PlatformInfo {
            label: label.to_value_of_unchecked().cast(),
            configuration: heap.alloc_typed(ConfigurationInfo::from_configuration_data(
                cfg.data()?,
                heap,
            )),
        })
    }
}

#[starlark_module]
fn platform_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenPlatformInfo)]
    fn PlatformInfo<'v>(
        #[starlark(require = named)] label: StringValue<'v>,
        #[starlark(require = named)] configuration: ValueTyped<'v, ConfigurationInfo<'v>>,
    ) -> starlark::Result<PlatformInfo<'v>> {
        Ok(PlatformInfo {
            label: label.to_value_of_unchecked().cast(),
            configuration,
        })
    }
}
