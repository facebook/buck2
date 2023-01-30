/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(Debug, thiserror::Error)]
enum AttributeAsStarlarkValueError {
    #[error("`attrs.default_only()` cannot be used in nested attributes")]
    DefaultOnlyInNested,
}

#[derive(
    derive_more::Display,
    Debug,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
pub struct AttributeAsStarlarkValue(Attribute);

starlark_simple_value!(AttributeAsStarlarkValue);

impl<'v> StarlarkValue<'v> for AttributeAsStarlarkValue {
    starlark_type!("attribute");
}

impl AttributeAsStarlarkValue {
    pub fn new(attr: Attribute) -> Self {
        Self(attr)
    }

    pub fn clone_attribute(&self) -> Attribute {
        self.0.clone()
    }

    /// Coercer to put into higher lever coercer (e. g. for `attrs.list(xxx)`).
    pub fn coercer_for_inner(&self) -> anyhow::Result<AttrType> {
        if self.0.is_default_only() {
            return Err(AttributeAsStarlarkValueError::DefaultOnlyInNested.into());
        }
        Ok(self.0.coercer().dupe())
    }

    pub fn default(&self) -> Option<&Arc<CoercedAttr>> {
        self.0.default()
    }
}
