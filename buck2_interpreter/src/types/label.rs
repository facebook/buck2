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
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use gazebo::prelude::*;
use serde::Serialize;
use serde::Serializer;
use starlark::collections::StarlarkHasher;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_type;
use starlark::values::docs::StarlarkDocs;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;

use crate::types::cell_root::CellRoot;
use crate::types::label_relative_path::LabelRelativePath;
use crate::types::target_label::StarlarkConfiguredTargetLabel;
use crate::types::target_label::StarlarkTargetLabel;

impl Label {
    pub fn label(&self) -> &ConfiguredProvidersLabel {
        &self.label
    }
}

/// Container for `ConfiguredProvidersLabel` that gives users access to things like package, cell, etc. This can also be properly stringified by our forthcoming `CommandLine` object
#[derive(
    Clone,
    Debug,
    Coerce,
    Display,
    Trace,
    Freeze,
    ProvidesStaticType,
    StarlarkDocs,
    Allocative
)]
#[display(fmt = "{}", label)]
#[repr(C)]
pub struct Label {
    #[freeze(identity)]
    label: ConfiguredProvidersLabel,
}

starlark_simple_value!(Label);

impl Serialize for Label {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.label.serialize(serializer)
    }
}

impl Label {
    pub fn new(label: ConfiguredProvidersLabel) -> Self {
        Label { label }
    }

    pub fn inner(&self) -> &ConfiguredProvidersLabel {
        &self.label
    }
}

impl<'v> StarlarkValue<'v> for Label
where
    Self: ProvidesStaticType,
{
    starlark_type!("label");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_label_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        Ok(match Label::from_value(other) {
            Some(other) => self.label == other.label,
            None => false,
        })
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.label.hash(hasher);
        Ok(())
    }
}

#[starlark_module]
fn configured_label_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn package<'v>(this: &'v Label) -> anyhow::Result<&'v str> {
        Ok(this.label.target().pkg().cell_relative_path().as_str())
    }

    #[starlark(attribute)]
    fn name<'v>(this: &'v Label) -> anyhow::Result<&'v str> {
        Ok(this.label.target().name().as_ref())
    }

    #[starlark(attribute)]
    fn sub_target<'v>(this: &'v Label) -> anyhow::Result<Option<Vec<&'v str>>> {
        Ok(match this.label.name() {
            ProvidersName::Default => None,
            ProvidersName::Named(s) => Some(s.iter().map(|p| p.as_str()).collect()),
            ProvidersName::UnrecognizedFlavor(_) => unreachable!(
                "This should have been an error when looking up the corresponding analysis (`{}`)",
                this.label
            ),
        })
    }

    #[starlark(attribute)]
    fn path<'v>(this: &Label, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let path = LabelRelativePath(this.label.target().pkg().to_cell_path());
        Ok(path.alloc_value(heap))
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &'v Label) -> anyhow::Result<&'v str> {
        Ok(this.label.target().pkg().cell_name().as_str())
    }

    #[starlark(attribute)]
    fn cell_root<'v>(this: &Label, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let cell_root = CellRoot::new(this.label.target().pkg().cell_name().clone());
        Ok(heap.alloc(cell_root))
    }

    /// Returns the unconfigured underlying target label.
    fn raw_target(this: &Label) -> anyhow::Result<StarlarkTargetLabel> {
        Ok(StarlarkTargetLabel::new(
            (*this.label.target().unconfigured()).dupe(),
        ))
    }

    /// Returns the underlying configured target label, dropping the sub target
    fn configured_target(this: &Label) -> anyhow::Result<StarlarkConfiguredTargetLabel> {
        Ok(StarlarkConfiguredTargetLabel::new(
            (*this.label.target()).dupe(),
        ))
    }
}

impl StarlarkProvidersLabel {
    pub fn label(&self) -> &ProvidersLabel {
        &self.label
    }
}

/// Container for `ProvidersLabel` that gives users access to things like package, cell, etc.
#[derive(
    Clone,
    Debug,
    Coerce,
    Display,
    Trace,
    Freeze,
    ProvidesStaticType,
    Serialize,
    Allocative
)]
#[display(fmt = "{}", label)]
#[repr(C)]
pub struct StarlarkProvidersLabel {
    #[freeze(identity)]
    #[serde(flatten)]
    label: ProvidersLabel,
}

starlark_simple_value!(StarlarkProvidersLabel);

impl StarlarkProvidersLabel {
    pub fn new(label: ProvidersLabel) -> Self {
        StarlarkProvidersLabel { label }
    }
}

impl<'v> StarlarkValue<'v> for StarlarkProvidersLabel
where
    Self: ProvidesStaticType,
{
    starlark_type!("providers_label");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(label_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        if let Some(other) = StarlarkProvidersLabel::from_value(other) {
            Ok(self.label == other.label)
        } else {
            Ok(false)
        }
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.label.hash(hasher);
        Ok(())
    }
}

#[starlark_module]
fn label_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn name<'v>(this: &'v StarlarkProvidersLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.target().name().as_ref())
    }

    #[starlark(attribute)]
    fn sub_target<'v>(this: &'v StarlarkProvidersLabel) -> anyhow::Result<Option<Vec<&'v str>>> {
        Ok(match this.label.name() {
            ProvidersName::Default => None,
            ProvidersName::Named(s) => Some(s.iter().map(|p| p.as_str()).collect()),
            ProvidersName::UnrecognizedFlavor(_) => unreachable!(
                "This should have been an error when looking up the corresponding analysis (`{}`)",
                this.label
            ),
        })
    }

    #[starlark(attribute)]
    fn path<'v>(this: &StarlarkProvidersLabel, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let path = LabelRelativePath(this.label.target().pkg().to_cell_path());
        Ok(path.alloc_value(heap))
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &'v StarlarkProvidersLabel) -> anyhow::Result<&'v str> {
        let cell = this.label.target().pkg().cell_name().as_str();
        Ok(cell)
    }

    /// Returns the unconfigured underlying target label.
    fn raw_target(this: &StarlarkProvidersLabel) -> anyhow::Result<StarlarkTargetLabel> {
        Ok(StarlarkTargetLabel::new((*this.label.target()).dupe()))
    }
}

#[cfg(test)]
mod tests {
    // Tests live in `buck2_build_api` crate because tests depend on `Tester` type
    // which depends on `buck2_build_api` heavily.
}
