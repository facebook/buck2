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
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use derive_more::Display;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::collections::StarlarkHasher;
use starlark::docs::StarlarkDocs;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::AllocValue;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;

use crate::types::cell_root::CellRoot;
use crate::types::label_relative_path::LabelRelativePath;
use crate::types::project_root::ProjectRoot;
use crate::types::target_label::StarlarkConfiguredTargetLabel;
use crate::types::target_label::StarlarkTargetLabel;

impl StarlarkConfiguredProvidersLabel {
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
pub struct StarlarkConfiguredProvidersLabel {
    #[freeze(identity)]
    label: ConfiguredProvidersLabel,
}

starlark_simple_value!(StarlarkConfiguredProvidersLabel);

impl Serialize for StarlarkConfiguredProvidersLabel {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.label.serialize(serializer)
    }
}

impl StarlarkConfiguredProvidersLabel {
    pub fn new(label: ConfiguredProvidersLabel) -> Self {
        StarlarkConfiguredProvidersLabel { label }
    }

    pub fn inner(&self) -> &ConfiguredProvidersLabel {
        &self.label
    }
}

#[starlark_value(type = "label")]
impl<'v> StarlarkValue<'v> for StarlarkConfiguredProvidersLabel
where
    Self: ProvidesStaticType<'v>,
{
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_label_methods)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        Ok(match StarlarkConfiguredProvidersLabel::from_value(other) {
            Some(other) => self.label == other.label,
            None => false,
        })
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.label.hash(hasher);
        Ok(())
    }
}

/// A label is used to represent a configured target.
#[starlark_module]
fn configured_label_methods(builder: &mut MethodsBuilder) {
    /// For the label `fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)` this gives back `buck2/hello`
    #[starlark(attribute)]
    fn package<'v>(this: &'v StarlarkConfiguredProvidersLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.target().pkg().cell_relative_path().as_str())
    }

    /// For the label `fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)` this gives back `world`
    #[starlark(attribute)]
    fn name<'v>(this: &'v StarlarkConfiguredProvidersLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.target().name().as_str())
    }

    #[starlark(attribute)]
    fn sub_target<'v>(
        this: &'v StarlarkConfiguredProvidersLabel,
    ) -> anyhow::Result<Option<Vec<&'v str>>> {
        Ok(match this.label.name() {
            ProvidersName::Default => None,
            ProvidersName::NonDefault(box NonDefaultProvidersName::Named(s)) => {
                Some(s.iter().map(|p| p.as_str()).collect())
            }
            ProvidersName::NonDefault(box NonDefaultProvidersName::UnrecognizedFlavor(_)) => {
                unreachable!(
                    "This should have been an error when looking up the corresponding analysis (`{}`)",
                    this.label
                )
            }
        })
    }

    /// For the label `fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)` this gives back `fbcode/buck2/hello`
    #[starlark(attribute)]
    fn path<'v>(this: &StarlarkConfiguredProvidersLabel) -> anyhow::Result<LabelRelativePath> {
        Ok(LabelRelativePath(this.label.target().pkg().to_cell_path()))
    }

    /// For the label `fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)` this gives back `fbcode`
    #[starlark(attribute)]
    fn cell<'v>(this: &'v StarlarkConfiguredProvidersLabel) -> anyhow::Result<&'v str> {
        Ok(this.label.target().pkg().cell_name().as_str())
    }

    /// Obtain a reference to this target label's cell root. This can be used as if it were an
    /// artifact in places that expect one, such as `cmd_args().relative_to`.
    #[starlark(attribute)]
    fn cell_root<'v>(this: &StarlarkConfiguredProvidersLabel) -> anyhow::Result<CellRoot> {
        Ok(CellRoot::new(this.label.target().pkg().cell_name()))
    }

    /// Obtain a reference to the project's root. This can be used as if it were an artifact in
    /// places that expect one, such as `cmd_args().relative_to`.
    #[starlark(attribute)]
    fn project_root<'v>(this: &StarlarkConfiguredProvidersLabel) -> anyhow::Result<ProjectRoot> {
        Ok(ProjectRoot::new())
    }

    /// For the label `fbcode//buck2/hello:world (ovr_config//platform/linux:x86_64-fbcode-46b26edb4b80a905)` this returns the unconfigured underlying target label (`fbcode//buck2/hello:world`)
    fn raw_target(this: &StarlarkConfiguredProvidersLabel) -> anyhow::Result<StarlarkTargetLabel> {
        Ok(StarlarkTargetLabel::new(
            (*this.label.target().unconfigured()).dupe(),
        ))
    }

    /// Returns the underlying configured target label, dropping the sub target
    fn configured_target(
        this: &StarlarkConfiguredProvidersLabel,
    ) -> anyhow::Result<StarlarkConfiguredTargetLabel> {
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
    StarlarkDocs,
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

#[starlark_value(type = "providers_label")]
impl<'v> StarlarkValue<'v> for StarlarkProvidersLabel
where
    Self: ProvidesStaticType<'v>,
{
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
        Ok(this.label.target().name().as_str())
    }

    #[starlark(attribute)]
    fn sub_target<'v>(this: &'v StarlarkProvidersLabel) -> anyhow::Result<Option<Vec<&'v str>>> {
        Ok(match this.label.name() {
            ProvidersName::Default => None,
            ProvidersName::NonDefault(box NonDefaultProvidersName::Named(s)) => {
                Some(s.iter().map(|p| p.as_str()).collect())
            }
            ProvidersName::NonDefault(box NonDefaultProvidersName::UnrecognizedFlavor(_)) => {
                unreachable!(
                    "This should have been an error when looking up the corresponding analysis (`{}`)",
                    this.label
                )
            }
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

#[starlark_module]
pub fn register_providers_label(globals: &mut GlobalsBuilder) {
    const Label: StarlarkValueAsType<StarlarkConfiguredProvidersLabel> = StarlarkValueAsType::new();
    const ProvidersLabel: StarlarkValueAsType<StarlarkProvidersLabel> = StarlarkValueAsType::new();
}

#[cfg(test)]
mod tests {
    // Tests live in `buck2_build_api` crate because tests depend on `Tester` type
    // which depends on `buck2_build_api` heavily.
}
