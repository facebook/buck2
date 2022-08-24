/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::attr_type::AttrTypeInner;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::Value;

use crate::attrs::coerce::coerced_attr::CoercedAttrExr;
use crate::attrs::coerce::AttrTypeCoerce;

pub(crate) mod any;
pub mod arg;
pub(crate) mod bool;
pub(crate) mod configuration_dep;
mod default_only;
pub(crate) mod dep;
mod dict;
mod enumeration;
pub(crate) mod int;
pub(crate) mod label;
mod list;
mod one_of;
mod option;
pub(crate) mod query;
pub(crate) mod source;
pub(crate) mod split_transition_dep;
mod string;
mod tuple;

pub(crate) trait AttrTypeExt {
    fn this(&self) -> &AttrType;

    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        self.this().0.coerce_item(configurable, ctx, value)
    }

    fn coerce(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<CoercedAttr> {
        self.coerce_with_default(configurable, ctx, value, None)
    }

    fn coerce_with_default(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
        default: Option<&CoercedAttr>,
    ) -> anyhow::Result<CoercedAttr> {
        CoercedAttr::coerce(self.this(), configurable, ctx, value, default)
    }

    fn starlark_type(&self) -> String {
        self.this().0.starlark_type()
    }
}

impl AttrTypeExt for AttrType {
    fn this(&self) -> &AttrType {
        self
    }
}

pub(crate) trait AttrTypeInnerExt {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>>;

    fn starlark_type(&self) -> String;
}

impl AttrTypeInnerExt for AttrTypeInner {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        match self {
            Self::Any(x) => x.coerce_item(configurable, ctx, value),
            Self::Arg(x) => x.coerce_item(configurable, ctx, value),
            Self::Bool(x) => x.coerce_item(configurable, ctx, value),
            Self::Int(x) => x.coerce_item(configurable, ctx, value),
            Self::Dep(x) => x.coerce_item(configurable, ctx, value),
            Self::Dict(x) => x.coerce_item(configurable, ctx, value),
            Self::List(x) => x.coerce_item(configurable, ctx, value),
            Self::Tuple(x) => x.coerce_item(configurable, ctx, value),
            Self::OneOf(x) => x.coerce_item(configurable, ctx, value),
            Self::Option(x) => x.coerce_item(configurable, ctx, value),
            Self::Source(x) => x.coerce_item(configurable, ctx, value),
            Self::String(x) => x.coerce_item(configurable, ctx, value),
            Self::Query(x) => x.coerce_item(configurable, ctx, value),
            Self::ConfigurationDep(x) => x.coerce_item(configurable, ctx, value),
            Self::ConfiguredDep(x) => x.coerce_item(configurable, ctx, value),
            Self::DefaultOnly(x) => x.coerce_item(configurable, ctx, value),
            Self::Enum(x) => x.coerce_item(configurable, ctx, value),
            Self::SplitTransitionDep(x) => x.coerce_item(configurable, ctx, value),
            Self::Label(x) => x.coerce_item(configurable, ctx, value),
        }
    }

    /// Returns a starlark-compatible typing string, e.g. `[str.type]` for values coerced by this
    /// attr.
    fn starlark_type(&self) -> String {
        match self {
            AttrTypeInner::Any(x) => x.starlark_type(),
            AttrTypeInner::Arg(x) => x.starlark_type(),
            AttrTypeInner::ConfigurationDep(x) => x.starlark_type(),
            AttrTypeInner::ConfiguredDep(x) => x.starlark_type(),
            AttrTypeInner::Bool(x) => x.starlark_type(),
            AttrTypeInner::Int(x) => x.starlark_type(),
            AttrTypeInner::Dep(x) => x.starlark_type(),
            AttrTypeInner::Dict(x) => x.starlark_type(),
            AttrTypeInner::Enum(x) => x.starlark_type(),
            AttrTypeInner::List(x) => x.starlark_type(),
            AttrTypeInner::Tuple(x) => x.starlark_type(),
            AttrTypeInner::OneOf(x) => x.starlark_type(),
            AttrTypeInner::Option(x) => x.starlark_type(),
            AttrTypeInner::Query(x) => x.starlark_type(),
            AttrTypeInner::Source(x) => x.starlark_type(),
            AttrTypeInner::String(x) => x.starlark_type(),
            AttrTypeInner::DefaultOnly(x) => x.starlark_type(),
            AttrTypeInner::SplitTransitionDep(x) => x.starlark_type(),
            AttrTypeInner::Label(x) => x.starlark_type(),
        }
    }
}
