/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use buck2_core::provider::ConfiguredProvidersLabel;
use buck2_core::provider::ProvidersLabel;
use buck2_core::provider::ProvidersName;
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
use starlark::starlark_complex_value;
use starlark::starlark_type;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::types::cell_root::CellRoot;
use crate::types::label_relative_path::LabelRelativePath;
use crate::types::target_label::StarlarkTargetLabel;

impl<V> LabelGen<V> {
    pub fn label(&self) -> &ConfiguredProvidersLabel {
        &self.label
    }
}

/// Container for `ConfiguredProvidersLabel` that gives users access to things like package, cell, etc. This can also be properly stringified by our forthcoming `CommandLine` object
#[derive(Clone, Debug, Coerce, Display, Trace, Freeze, ProvidesStaticType)]
#[display(fmt = "{}", label)]
#[repr(C)]
pub struct LabelGen<V> {
    // TODO(nmj): We don't really want to allocate these up front, but we don't
    //                 get the heap during get_attr(), so we have to for now. Revisit
    //                 later, because most people probably don't actually need this.
    package_string: V,
    name_string: V,
    provider_string: V,
    #[trace(unsafe_ignore)]
    #[freeze(identity)]
    label: ConfiguredProvidersLabel,
}

starlark_complex_value!(pub Label);

impl<V> Serialize for LabelGen<V> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.label.serialize(serializer)
    }
}

impl<'v> Label<'v> {
    pub fn new(heap: &'v Heap, label: ConfiguredProvidersLabel) -> Self {
        let package_string = heap.alloc(label.target().pkg().cell_relative_path().as_str());
        let name_string = heap.alloc(label.target().name().as_ref());
        let provider_string = match label.name() {
            ProvidersName::Default => Value::new_none(),
            ProvidersName::Named(s) => {
                heap.alloc_list_iter(s.iter().map(|p| heap.alloc(p.as_str())))
            }
            ProvidersName::UnrecognizedFlavor(_) => unreachable!(
                "This should have been an error when looking up the corresponding analysis (`{}`)",
                label
            ),
        };
        Label {
            package_string,
            name_string,
            provider_string,
            label,
        }
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for LabelGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("label");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_label_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        if let Some(other) = Label::from_value(other) {
            Ok(self.package_string.equals(other.package_string)?
                && self.name_string.equals(other.name_string)?
                && self.provider_string.equals(other.provider_string)?
                && self.label == other.label)
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
fn configured_label_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn package<'v>(this: &Label) -> anyhow::Result<Value<'v>> {
        Ok(this.package_string.to_value())
    }

    #[starlark(attribute)]
    fn name<'v>(this: &Label) -> anyhow::Result<Value<'v>> {
        Ok(this.name_string.to_value())
    }

    #[starlark(attribute)]
    fn sub_target<'v>(this: &Label) -> anyhow::Result<Value<'v>> {
        Ok(this.provider_string.to_value())
    }

    #[starlark(attribute)]
    fn path<'v>(this: &Label, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let path = LabelRelativePath(this.label.target().pkg().to_cell_path());
        Ok(path.alloc_value(heap))
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &Label, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let cell = this.label.target().pkg().cell_name().as_str();
        Ok(heap.alloc(cell))
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
}

impl<V> StarlarkProvidersLabelGen<V> {
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
    Serialize
)]
#[display(fmt = "{}", label)]
#[repr(C)]
pub struct StarlarkProvidersLabelGen<V> {
    #[serde(skip)]
    name_string: V,
    #[serde(skip)]
    provider_string: V,
    #[trace(unsafe_ignore)]
    #[freeze(identity)]
    #[serde(flatten)]
    label: ProvidersLabel,
}

starlark_complex_value!(pub StarlarkProvidersLabel);

impl<'v> StarlarkProvidersLabel<'v> {
    pub fn new(heap: &'v Heap, label: ProvidersLabel) -> Self {
        let name_string = heap.alloc(label.target().name().as_ref());
        let provider_string = match label.name() {
            ProvidersName::Default => Value::new_none(),
            ProvidersName::Named(s) => {
                heap.alloc_list_iter(s.iter().map(|p| heap.alloc(p.as_str())))
            }
            ProvidersName::UnrecognizedFlavor(_) => unreachable!(
                "This should have been an error when looking up the corresponding analysis (`{}`)",
                label
            ),
        };
        StarlarkProvidersLabel {
            name_string,
            provider_string,
            label,
        }
    }
}

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StarlarkProvidersLabelGen<V>
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
    fn name<'v>(this: &StarlarkProvidersLabel) -> anyhow::Result<Value<'v>> {
        Ok(this.name_string.to_value())
    }

    #[starlark(attribute)]
    fn sub_target<'v>(this: &StarlarkProvidersLabel) -> anyhow::Result<Value<'v>> {
        Ok(this.provider_string.to_value())
    }

    #[starlark(attribute)]
    fn path<'v>(this: &StarlarkProvidersLabel, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let path = LabelRelativePath(this.label.target().pkg().to_cell_path());
        Ok(path.alloc_value(heap))
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &StarlarkProvidersLabel) -> anyhow::Result<&'v str> {
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
