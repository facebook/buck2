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
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::NonDefaultProvidersName;
use buck2_core::provider::label::ProviderName;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::BuckErrorContext;
use derive_more::Display;
use derive_more::From;
use dupe::Dupe;
use serde::Serialize;
use starlark::any::ProvidesStaticType;
use starlark::collections::StarlarkHasher;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::list::UnpackList;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;

use crate::types::cell_path::StarlarkCellPath;
use crate::types::configuration::StarlarkConfiguration;
use crate::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use crate::types::configured_providers_label::StarlarkProvidersLabel;

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

#[starlark_value(type = "TargetLabel")]
impl<'v> StarlarkValue<'v> for StarlarkTargetLabel {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(label_methods)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        self.hash(hasher);
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.label == other.label)
        } else {
            Ok(false)
        }
    }

    fn compare(&self, other: Value<'v>) -> starlark::Result<std::cmp::Ordering> {
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
    fn package<'v>(
        this: &StarlarkTargetLabel,
        heap: &'v Heap,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(heap.alloc_str_intern(this.label.pkg().cell_relative_path().as_str()))
    }

    #[starlark(attribute)]
    fn name<'v>(this: &'v StarlarkTargetLabel) -> starlark::Result<&'v str> {
        Ok(this.label.name().as_str())
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &'v StarlarkTargetLabel) -> starlark::Result<&'v str> {
        Ok(this.label.pkg().cell_name().as_str())
    }

    #[starlark(attribute)]
    fn path<'v>(this: &StarlarkTargetLabel) -> starlark::Result<StarlarkCellPath> {
        Ok(StarlarkCellPath(this.label.pkg().to_cell_path()))
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
        // TODO(nga): must be either positional or named.
        #[starlark(default = SubtargetNameArg::List(UnpackList { items: Vec::new() }))]
        subtarget_name: SubtargetNameArg<'v>,
    ) -> starlark::Result<StarlarkProvidersLabel> {
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

#[starlark_value(type = "ConfiguredTargetLabel")]
impl<'v> StarlarkValue<'v> for StarlarkConfiguredTargetLabel {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_label_methods)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        self.hash(hasher);
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.label == other.label)
        } else {
            Ok(false)
        }
    }

    fn compare(&self, other: Value<'v>) -> starlark::Result<std::cmp::Ordering> {
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
    fn package<'v>(
        this: &StarlarkConfiguredTargetLabel,
        heap: &'v Heap,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(heap.alloc_str_intern(this.label.pkg().cell_relative_path().as_str()))
    }

    #[starlark(attribute)]
    fn name<'v>(this: &'v StarlarkConfiguredTargetLabel) -> starlark::Result<&'v str> {
        Ok(this.label.name().as_str())
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &'v StarlarkConfiguredTargetLabel) -> starlark::Result<&'v str> {
        Ok(this.label.pkg().cell_name().as_str())
    }

    #[starlark(attribute)]
    fn path<'v>(this: &StarlarkConfiguredTargetLabel) -> starlark::Result<StarlarkCellPath> {
        Ok(StarlarkCellPath(this.label.pkg().to_cell_path()))
    }

    fn config<'v>(this: &StarlarkConfiguredTargetLabel) -> starlark::Result<StarlarkConfiguration> {
        Ok(StarlarkConfiguration((this.label.cfg()).dupe()))
    }

    /// Returns the unconfigured underlying target label.
    fn raw_target(this: &StarlarkConfiguredTargetLabel) -> starlark::Result<StarlarkTargetLabel> {
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
        // TODO(nga): must be either positional or named.
        #[starlark(default = SubtargetNameArg::List(UnpackList { items: Vec::new() }))]
        subtarget_name: SubtargetNameArg<'v>,
    ) -> starlark::Result<StarlarkConfiguredProvidersLabel> {
        let providers_name = value_to_providers_name(subtarget_name)?;

        Ok(StarlarkConfiguredProvidersLabel::new(
            ConfiguredProvidersLabel::new(this.label().dupe(), providers_name),
        ))
    }
}

#[derive(StarlarkTypeRepr, UnpackValue)]
enum SubtargetNameArg<'v> {
    List(UnpackList<String>),
    Str(&'v str),
}

fn value_to_providers_name(subtarget_name: SubtargetNameArg) -> buck2_error::Result<ProvidersName> {
    let subtarget = match subtarget_name {
        SubtargetNameArg::List(list) => list
            .items
            .into_iter()
            .map(|name| {
                ProviderName::new(name).buck_error_context("for parameter `subtarget_name`")
            })
            .collect::<buck2_error::Result<Vec<_>>>()?,
        SubtargetNameArg::Str(str) => {
            vec![
                ProviderName::new(str.to_owned())
                    .buck_error_context("for parameter `subtarget_name`")?,
            ]
        }
    };

    Ok(if subtarget.is_empty() {
        ProvidersName::Default
    } else {
        ProvidersName::NonDefault(triomphe::Arc::new(NonDefaultProvidersName::Named(
            buck2_util::arc_str::ArcSlice::from_iter(subtarget),
        )))
    })
}

#[starlark_module]
pub fn register_target_label(globals: &mut GlobalsBuilder) {
    const TargetLabel: StarlarkValueAsType<StarlarkTargetLabel> = StarlarkValueAsType::new();
    const ConfiguredTargetLabel: StarlarkValueAsType<StarlarkConfiguredTargetLabel> =
        StarlarkValueAsType::new();
}
