/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_core::execution_types::execution_group::ExecutionGroup;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::attr_type::AttrType;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkPagable;
use starlark::values::StarlarkValue;
use starlark::values::starlark_value;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum StarlarkAttributeError {
    #[error("`attrs.default_only()` cannot be used in nested attributes")]
    DefaultOnlyInNested,
    #[error("`attrs.exec_compatible_with()` cannot be used in nested attributes")]
    ExecCompatibleWithInNested,
    #[error("`attrs.default_only()` cannot wrap `attrs.exec_compatible_with()`")]
    ExecCompatibleWithInDefaultOnly,
}

#[derive(
    derive_more::Display,
    Debug,
    ProvidesStaticType,
    NoSerialize,
    Allocative,
    StarlarkPagable
)]
#[display("{}", attribute)]
pub struct StarlarkAttribute {
    #[starlark_pagable(pagable)]
    attribute: Attribute,
    /// The execution group whose platform the attribute's constraints apply to, for attributes
    /// created by `attrs.exec_compatible_with()`.
    #[starlark_pagable(pagable)]
    exec_compatible_with_group: Option<ExecutionGroup>,
}

starlark_simple_value!(StarlarkAttribute);

/// Type of the attribute object returned by methods under [`attrs`](../attrs) namespace, e. g. `attrs.string()`.
#[starlark_module]
fn starlark_attribute_methods(builder: &mut MethodsBuilder) {}

starlark::methods_static!(STARLARK_ATTRIBUTE_METHODS = starlark_attribute_methods);

#[starlark_value(type = "Attr")]
impl<'v> StarlarkValue<'v> for StarlarkAttribute {
    // Used to add type documentation to the generated documentation
    fn get_methods() -> Option<&'static Methods> {
        Some(STARLARK_ATTRIBUTE_METHODS.methods())
    }
}

impl StarlarkAttribute {
    pub fn new(attr: Attribute) -> Self {
        Self {
            attribute: attr,
            exec_compatible_with_group: None,
        }
    }

    pub(crate) fn new_exec_compatible_with(attr: Attribute, group: ExecutionGroup) -> Self {
        Self {
            attribute: attr,
            exec_compatible_with_group: Some(group),
        }
    }

    pub fn attribute(&self) -> &Attribute {
        &self.attribute
    }

    pub fn clone_attribute(&self) -> Attribute {
        self.attribute.clone()
    }

    pub(crate) fn exec_compatible_with_group(&self) -> Option<&ExecutionGroup> {
        self.exec_compatible_with_group.as_ref()
    }

    /// Coercer to put into higher lever coercer (e. g. for `attrs.list(xxx)`).
    pub fn coercer_for_inner(&self) -> buck2_error::Result<AttrType> {
        if self.attribute.is_default_only() {
            return Err(StarlarkAttributeError::DefaultOnlyInNested.into());
        }
        if self.exec_compatible_with_group.is_some() {
            return Err(StarlarkAttributeError::ExecCompatibleWithInNested.into());
        }
        Ok(self.attribute.coercer().dupe())
    }

    pub fn coercer_for_default_only(&self) -> buck2_error::Result<AttrType> {
        if self.exec_compatible_with_group.is_some() {
            return Err(StarlarkAttributeError::ExecCompatibleWithInDefaultOnly.into());
        }
        Ok(self.attribute.coercer().dupe())
    }
}

#[starlark_module]
#[starlark_types(StarlarkAttribute as Attr)]
pub(crate) fn register_attr_type(globals: &mut GlobalsBuilder) {}
