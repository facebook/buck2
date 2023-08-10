/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::NonDefaultProvidersName;
use buck2_core::provider::label::ProviderName;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use derive_more::Display;
use derive_more::From;
use dupe::Dupe;
use serde::Serialize;
use starlark::any::ProvidesStaticType;
use starlark::collections::StarlarkHasher;
use starlark::docs::StarlarkDocs;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::list::AllocList;
use starlark::values::list::ListRef;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;

use crate::starlark::values::AllocValue;
use crate::types::configuration::StarlarkConfiguration;
use crate::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use crate::types::configured_providers_label::StarlarkProvidersLabel;
use crate::types::label_relative_path::LabelRelativePath;

#[derive(
    Clone,
    Dupe,
    Debug,
    Hash,
    Display,
    PartialEq,
    Eq,
    From,
    ProvidesStaticType,
    Serialize,
    StarlarkDocs,
    Allocative
)]
#[serde(transparent)]
pub struct StarlarkTargetLabel {
    label: TargetLabel,
}

starlark_simple_value!(StarlarkTargetLabel);

impl StarlarkTargetLabel {
    pub fn label(&self) -> &TargetLabel {
        &self.label
    }

    pub fn new(label: TargetLabel) -> Self {
        StarlarkTargetLabel { label }
    }
}

#[starlark_value(type = "target_label")]
impl<'v> StarlarkValue<'v> for StarlarkTargetLabel {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(label_methods)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.hash(hasher);
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.label == other.label)
        } else {
            Ok(false)
        }
    }

    fn compare(&self, other: Value<'v>) -> anyhow::Result<std::cmp::Ordering> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.label.cmp(&other.label))
        } else {
            ValueError::unsupported_with(self, "compare", other)
        }
    }
}

#[starlark_module]
fn label_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn package<'v>(this: &StarlarkTargetLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.pkg().cell_relative_path().as_str())
    }

    #[starlark(attribute)]
    fn name<'v>(this: &StarlarkTargetLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.name().as_str())
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &'v StarlarkTargetLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.pkg().cell_name().as_str())
    }

    /// Converts a `TargetLabel` into its corresponding `ProvidersLabel` given the subtarget names,
    /// which is a list for each layer of subtarget
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_sub_target(ctx):
    ///     owners = ctx.uquery().owner("bin/TARGETS.fixture")
    ///     for owner in owners:
    ///         unconfigured_label = owner.label
    ///         ctx.output.print(unconfigured_label.with_sub_target())
    ///         ctx.output.print(unconfigured_label.with_sub_target("subtarget1"))
    ///         ctx.output.print(unconfigured_label.with_sub_target(["subtarget1", "subtarget2"]))
    /// ```
    fn with_sub_target<'v>(
        this: &StarlarkTargetLabel,
        #[starlark(default = AllocList::EMPTY)] subtarget_name: Value<'v>,
    ) -> anyhow::Result<StarlarkProvidersLabel> {
        let providers_name = value_to_providers_name(subtarget_name)?;

        Ok(StarlarkProvidersLabel::new(ProvidersLabel::new(
            this.label().dupe(),
            providers_name,
        )))
    }
}

#[derive(
    Clone,
    Dupe,
    Debug,
    Hash,
    Display,
    PartialEq,
    Eq,
    From,
    ProvidesStaticType,
    Serialize,
    StarlarkDocs,
    Allocative
)]
#[serde(transparent)]
pub struct StarlarkConfiguredTargetLabel {
    label: ConfiguredTargetLabel,
}

starlark_simple_value!(StarlarkConfiguredTargetLabel);

impl StarlarkConfiguredTargetLabel {
    pub fn label(&self) -> &ConfiguredTargetLabel {
        &self.label
    }

    pub fn new(label: ConfiguredTargetLabel) -> Self {
        StarlarkConfiguredTargetLabel { label }
    }
}

#[starlark_value(type = "configured_target_label")]
impl<'v> StarlarkValue<'v> for StarlarkConfiguredTargetLabel {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_label_methods)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.hash(hasher);
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.label == other.label)
        } else {
            Ok(false)
        }
    }

    fn compare(&self, other: Value<'v>) -> anyhow::Result<std::cmp::Ordering> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.label.cmp(&other.label))
        } else {
            ValueError::unsupported_with(self, "compare", other)
        }
    }
}

#[starlark_module]
fn configured_label_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn package<'v>(this: &StarlarkConfiguredTargetLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.pkg().cell_relative_path().as_str())
    }

    #[starlark(attribute)]
    fn name<'v>(this: &StarlarkConfiguredTargetLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.name().as_str())
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &'v StarlarkConfiguredTargetLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.pkg().cell_name().as_str())
    }

    #[starlark(attribute)]
    fn path<'v>(this: &StarlarkConfiguredTargetLabel, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let path = LabelRelativePath(this.label.pkg().to_cell_path());
        Ok(path.alloc_value(heap))
    }

    fn config<'v>(this: &StarlarkConfiguredTargetLabel) -> anyhow::Result<StarlarkConfiguration> {
        Ok(StarlarkConfiguration((this.label.cfg()).dupe()))
    }

    /// Returns the unconfigured underlying target label.
    fn raw_target(this: &StarlarkConfiguredTargetLabel) -> anyhow::Result<StarlarkTargetLabel> {
        Ok(StarlarkTargetLabel::new(
            (*this.label.unconfigured()).dupe(),
        ))
    }

    /// Converts a `ConfiguredTargetLabel` into its corresponding `Label` given the subtarget name
    /// which is a list for each layer of subtarget
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_sub_target(ctx):
    ///     owners = ctx.cquery().owner("bin/TARGETS.fixture")
    ///     for owner in owners:
    ///         configured_label = owner.label
    ///         ctx.output.print(configured_label.with_sub_target())
    ///         ctx.output.print(configured_label.with_sub_target("subtarget1"))
    ///         ctx.output.print(configured_label.with_sub_target(["subtarget1", "subtarget2"]))
    /// ```
    fn with_sub_target<'v>(
        this: &'v StarlarkConfiguredTargetLabel,
        #[starlark(default = AllocList::EMPTY)] subtarget_name: Value<'v>,
    ) -> anyhow::Result<StarlarkConfiguredProvidersLabel> {
        let providers_name = value_to_providers_name(subtarget_name)?;

        Ok(StarlarkConfiguredProvidersLabel::new(
            ConfiguredProvidersLabel::new(this.label().dupe(), providers_name),
        ))
    }
}

pub fn value_to_providers_name<'v>(subtarget_name: Value<'v>) -> anyhow::Result<ProvidersName> {
    let subtarget = if let Some(list) = ListRef::from_value(subtarget_name) {
        list.iter()
            .map(|name| {
                name.unpack_str()
                    .ok_or_else(|| {
                        anyhow::anyhow!(ValueError::IncorrectParameterTypeNamedWithExpected(
                            "subtarget_name".to_owned(),
                            "list of str or str".to_owned(),
                            name.get_type().to_owned(),
                        ))
                    })
                    .and_then(|name| {
                        ProviderName::new(name.to_owned())
                            .context("for parameter `subtarget_name`")
                            .map_err(|e| anyhow::anyhow!(e))
                    })
            })
            .collect::<anyhow::Result<Vec<_>>>()?
    } else if let Some(str) = subtarget_name.unpack_str() {
        vec![ProviderName::new(str.to_owned()).context("for parameter `subtarget_name`")?]
    } else {
        return Err(anyhow::anyhow!(
            ValueError::IncorrectParameterTypeNamedWithExpected(
                "subtarget_name".to_owned(),
                "list of str or str".to_owned(),
                subtarget_name.get_type().to_owned()
            )
        ));
    };

    Ok(if subtarget.is_empty() {
        ProvidersName::Default
    } else {
        ProvidersName::NonDefault(Box::new(NonDefaultProvidersName::Named(
            subtarget.into_boxed_slice(),
        )))
    })
}

#[starlark_module]
pub fn register_target_label(globals: &mut GlobalsBuilder) {
    const TargetLabel: StarlarkValueAsType<StarlarkTargetLabel> = StarlarkValueAsType::new();
}
