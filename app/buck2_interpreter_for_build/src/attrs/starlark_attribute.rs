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
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum StarlarkAttributeError {
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
pub struct StarlarkAttribute(Attribute);

starlark_simple_value!(StarlarkAttribute);

/// Type of the attribute object returned by methods under [`attrs`](../attrs) namespace, e. g. `attrs.string()`.
#[starlark_module]
fn starlark_attribute_methods(builder: &mut MethodsBuilder) {}

#[starlark_value(type = "attribute")]
impl<'v> StarlarkValue<'v> for StarlarkAttribute {
    // Used to add type documentation to the generated documentation
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(starlark_attribute_methods)
    }
}

impl StarlarkAttribute {
    pub fn new(attr: Attribute) -> Self {
        Self(attr)
    }

    pub fn clone_attribute(&self) -> Attribute {
        self.0.clone()
    }

    /// Coercer to put into higher lever coercer (e. g. for `attrs.list(xxx)`).
    pub fn coercer_for_inner(&self) -> buck2_error::Result<AttrType> {
        if self.0.is_default_only() {
            return Err(StarlarkAttributeError::DefaultOnlyInNested.into());
        }
        Ok(self.0.coercer().dupe())
    }

    pub fn coercer_for_default_only(&self) -> AttrType {
        self.0.coercer().dupe()
    }

    pub fn default(&self) -> Option<&Arc<CoercedAttr>> {
        self.0.default()
    }
}

#[starlark_module]
pub(crate) fn register_attr_type(globals: &mut GlobalsBuilder) {
    /// Starlark type of the attribute object (for example, returned from `attrs.string()`).
    const Attr: StarlarkValueAsType<StarlarkAttribute> = StarlarkValueAsType::new();
}
