/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_analysis::attrs::resolve::configured_attr::ConfiguredAttrExt;
use buck2_build_api::actions::query::PackageLabelOption;
use buck2_build_api::bxl::unconfigured_attribute::CoercedAttrExt;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use starlark::collections::SmallMap;
use starlark::values::none::NoneOr;
use starlark::values::Heap;
use starlark::values::StringValue;
use starlark::values::Value;

use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;

pub(crate) trait NodeAttributeGetter {
    fn get_attr<'v>(&self, key: &str, heap: &'v Heap) -> buck2_error::Result<NoneOr<Value<'v>>>;
    fn get_attrs<'v>(
        &self,
        heap: &'v Heap,
    ) -> buck2_error::Result<SmallMap<StringValue<'v>, Value<'v>>>;
    fn has_attr(&self, key: &str) -> bool;
}

impl NodeAttributeGetter for StarlarkTargetNode {
    fn get_attr<'v>(&self, key: &str, heap: &'v Heap) -> buck2_error::Result<NoneOr<Value<'v>>> {
        let node = &self.0;
        let pkg = node.label().pkg();
        match node.attr_or_none(key, AttrInspectOptions::All) {
            Some(attr) => Ok(NoneOr::Other(attr.value.to_value(pkg, heap)?)),
            None => Ok(NoneOr::None),
        }
    }

    fn get_attrs<'v>(
        &self,
        heap: &'v Heap,
    ) -> buck2_error::Result<SmallMap<StringValue<'v>, Value<'v>>> {
        let node = &self.0;
        let pkg = node.label().pkg();
        let attrs_iter = node.attrs(AttrInspectOptions::All);
        attrs_iter
            .map(|attr| {
                let name = heap.alloc_str_intern(attr.name);
                let value = attr.value.to_value(pkg, heap)?;
                Ok((name, value))
            })
            .collect::<buck2_error::Result<SmallMap<_, _>>>()
    }

    fn has_attr(&self, key: &str) -> bool {
        let node = &self.0;
        node.attr_or_none(key, AttrInspectOptions::All).is_some()
    }
}

impl NodeAttributeGetter for StarlarkConfiguredTargetNode {
    fn get_attr<'v>(&self, key: &str, heap: &'v Heap) -> buck2_error::Result<NoneOr<Value<'v>>> {
        let node = &self.0;
        let pkg = PackageLabelOption::PackageLabel(node.label().pkg());
        match node.get(key, AttrInspectOptions::All) {
            Some(attr) => Ok(NoneOr::Other(attr.value.to_value(pkg, heap)?)),
            None => Ok(NoneOr::None),
        }
    }

    fn get_attrs<'v>(
        &self,
        heap: &'v Heap,
    ) -> buck2_error::Result<SmallMap<StringValue<'v>, Value<'v>>> {
        let node = &self.0;
        let pkg = PackageLabelOption::PackageLabel(node.label().pkg());
        let attrs_iter = node.attrs(AttrInspectOptions::All);
        attrs_iter
            .map(|attr| {
                let name = heap.alloc_str_intern(attr.name);
                let value = attr.value.to_value(pkg, heap)?;
                Ok((name, value))
            })
            .collect::<buck2_error::Result<SmallMap<_, _>>>()
    }

    fn has_attr(&self, key: &str) -> bool {
        let node = &self.0;
        // attr coercion here is somewhat expensive, we need a more efficient way to check if an attr exists
        node.get(key, AttrInspectOptions::All).is_some()
    }
}
