/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use buck2_core::target::{ConfiguredTargetLabel, TargetLabel};
use derive_more::{Display, From};
use gazebo::{any::AnyLifetime, prelude::*};
use serde::Serialize;
use starlark::{
    collections::StarlarkHasher,
    environment::{Methods, MethodsBuilder, MethodsStatic},
    starlark_type,
    values::{Heap, StarlarkValue, Value, ValueError, ValueLike},
};

use crate::{
    interpreter::rule_defs::label_relative_path::LabelRelativePath, starlark::values::AllocValue,
};

#[derive(
    Clone,
    Dupe,
    Debug,
    Hash,
    Display,
    PartialEq,
    Eq,
    From,
    AnyLifetime,
    Serialize
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

impl<'v> StarlarkValue<'v> for StarlarkTargetLabel {
    starlark_type!("target_label");

    fn get_methods(&self) -> Option<&'static Methods> {
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
        Ok(this.label.name().value())
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &StarlarkTargetLabel, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let cell = this.label.pkg().cell_name().as_str();
        Ok(heap.alloc(cell))
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
    AnyLifetime,
    Serialize
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

impl<'v> StarlarkValue<'v> for StarlarkConfiguredTargetLabel {
    starlark_type!("configured_target_label");

    fn get_methods(&self) -> Option<&'static Methods> {
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
        Ok(this.label.name().value())
    }

    #[starlark(attribute)]
    fn cell<'v>(this: &StarlarkConfiguredTargetLabel, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let cell = this.label.pkg().cell_name().as_str();
        Ok(heap.alloc(cell))
    }

    #[starlark(attribute)]
    fn path<'v>(this: &StarlarkConfiguredTargetLabel, heap: &Heap) -> anyhow::Result<Value<'v>> {
        let path = LabelRelativePath(this.label.pkg().to_cell_path());
        Ok(path.alloc_value(heap))
    }

    /// Returns the unconfigured underlying target label.
    fn raw_target(this: &StarlarkConfiguredTargetLabel) -> anyhow::Result<StarlarkTargetLabel> {
        Ok(StarlarkTargetLabel::new(
            (*this.label.unconfigured()).dupe(),
        ))
    }
}
