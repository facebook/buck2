/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::types::opaque_metadata::OpaqueMetadata;
use buck2_node::attrs::attr_type::metadata::MetadataAttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::typing::Ty;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Value;

use crate::attrs::coerce::AttrTypeCoerce;

#[derive(Debug, thiserror::Error)]
enum MetadataAttrTypeCoerceError {
    #[error("Metadata attribute is not configurable (internal error)")]
    AttrTypeNotConfigurable,
    #[error("Metadata attribute is not implemented yet")]
    AttrTypeNotImplemented,
}

impl AttrTypeCoerce for MetadataAttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        _ctx: &dyn AttrCoercionContext,
        _value: Value,
    ) -> anyhow::Result<CoercedAttr> {
        if configurable == AttrIsConfigurable::Yes {
            return Err(MetadataAttrTypeCoerceError::AttrTypeNotConfigurable.into());
        }
        Err(MetadataAttrTypeCoerceError::AttrTypeNotImplemented.into())
    }

    fn starlark_type(&self) -> Ty {
        OpaqueMetadata::starlark_type_repr()
    }
}
