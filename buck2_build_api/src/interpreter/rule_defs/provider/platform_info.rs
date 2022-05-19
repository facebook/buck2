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
use buck2_core::configuration::Configuration;
use gazebo::{any::AnyLifetime, coerce::Coerce};
use starlark::{
    environment::GlobalsBuilder,
    values::{Freeze, Heap, StringValue, Trace, ValueLike, ValueOf},
};

use crate::interpreter::rule_defs::provider::configuration_info::ConfigurationInfo;

#[internal_provider(platform_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, AnyLifetime)]
#[repr(C)]
pub(crate) struct PlatformInfoGen<V> {
    /// str
    label: V,
    /// ConfigurationInfo
    configuration: V,
}

impl<'v, V: ValueLike<'v>> PlatformInfoGen<V> {
    pub fn to_configuration(&self) -> anyhow::Result<Configuration> {
        Configuration::from_platform(
            self.label
                .to_value()
                .unpack_str()
                .expect("type checked during construction")
                .to_owned(),
            ConfigurationInfo::from_value(self.configuration.to_value())
                .expect("type checked during construction")
                .to_configuration_data(),
        )
    }
}

impl<'v> PlatformInfo<'v> {
    pub(crate) fn from_configuration(
        cfg: &Configuration,
        heap: &'v Heap,
    ) -> anyhow::Result<PlatformInfo<'v>> {
        let label = heap.alloc_str(cfg.label()?).to_value();
        let configuration = heap.alloc(ConfigurationInfo::from_configuration_data(
            cfg.data()?,
            heap,
        ));
        Ok(PlatformInfoGen {
            label,
            configuration,
        })
    }
}

#[starlark_module]
fn platform_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(type = "PlatformInfo")]
    fn PlatformInfo<'v>(
        label: StringValue,
        configuration: ValueOf<&ConfigurationInfo>,
    ) -> anyhow::Result<PlatformInfo<'v>> {
        Ok(PlatformInfo {
            label: label.to_value(),
            configuration: *configuration,
        })
    }
}
