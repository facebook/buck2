/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use attr_literal::AttrLiteral;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::id::ProviderId;
use configuration_dep::ConfigurationDepAttrType;
use default_only::DefaultOnlyAttrType;
use dep::DepAttrTransition;
use dep::DepAttrType;
use dep::ExplicitConfiguredDepAttrType;
use dict::DictAttrType;
use enumeration::EnumAttrType;
use gazebo::dupe::Dupe;
use label::LabelAttrType;
use one_of::OneOfAttrType;
use option::OptionAttrType;
use query::QueryAttrType;
use source::SourceAttrType;
use split_transition_dep::SplitTransitionDepAttrType;
use starlark::values::Value;
use string::StringAttrType;
use tuple::TupleAttrType;

use crate::attrs::attr_type::any::AnyAttrType;
use crate::attrs::attr_type::arg::ArgAttrType;
use crate::attrs::attr_type::bool::BoolAttrType;
use crate::attrs::attr_type::int::IntAttrType;
use crate::attrs::attr_type::list::ListAttrType;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::configurable::AttrIsConfigurable;
use crate::attrs::AttrCoercionContext;

pub(crate) mod any;
pub mod arg;
pub mod attr_literal;
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

#[derive(Clone, Dupe, Debug, Hash, Eq, PartialEq)]
pub struct AttrType(Arc<AttrTypeInner>);

#[derive(Debug, Hash, Eq, PartialEq)]
pub(crate) enum AttrTypeInner {
    Any(AnyAttrType),
    Arg(ArgAttrType),
    ConfigurationDep(ConfigurationDepAttrType),
    ConfiguredDep(ExplicitConfiguredDepAttrType),
    Bool(BoolAttrType),
    Int(IntAttrType),
    Dep(DepAttrType),
    Dict(DictAttrType),
    List(ListAttrType),
    Tuple(TupleAttrType),
    OneOf(OneOfAttrType),
    Option(OptionAttrType),
    Query(QueryAttrType),
    Source(SourceAttrType),
    SplitTransitionDep(SplitTransitionDepAttrType),
    String(StringAttrType),
    DefaultOnly(DefaultOnlyAttrType),
    Enum(EnumAttrType),
    Label(LabelAttrType),
}

impl AttrType {
    pub(crate) fn fmt_with_default(
        &self,
        f: &mut fmt::Formatter<'_>,
        default: Option<&str>,
    ) -> fmt::Result {
        let mut attr = |s| match default {
            None => write!(f, "attr.{}()", s),
            Some(default) => write!(f, "attr.{}(default={})", s, default),
        };
        let arg = || match default {
            None => String::new(),
            Some(x) => format!(", default={}", x),
        };

        match &*self.0 {
            AttrTypeInner::Any(_) => attr("any"),
            AttrTypeInner::Arg(_) => attr("arg"),
            AttrTypeInner::ConfigurationDep(_) => attr("configuration_dep"),
            AttrTypeInner::ConfiguredDep(_) => attr("configured_dep"),
            AttrTypeInner::Bool(_) => attr("bool"),
            AttrTypeInner::Int(_) => attr("int"),
            AttrTypeInner::Dep(_) => attr("dep"),
            AttrTypeInner::Query(_) => attr("query"),
            AttrTypeInner::Dict(x) => x.fmt_with_arg(f, &arg()),
            AttrTypeInner::List(x) => x.fmt_with_arg(f, &arg()),
            AttrTypeInner::Tuple(x) => x.fmt_with_arg(f, &arg()),
            AttrTypeInner::OneOf(x) => x.fmt_with_arg(f, &arg()),
            AttrTypeInner::Option(x) => x.fmt_with_arg(f, &arg()),
            AttrTypeInner::Enum(x) => x.fmt_with_arg(f, &arg()),
            AttrTypeInner::Source(_) => attr("source"),
            AttrTypeInner::SplitTransitionDep(_) => attr("split_transition_dep"),
            AttrTypeInner::String(_) => attr("string"),
            AttrTypeInner::DefaultOnly(_) => attr("default_only"),
            AttrTypeInner::Label(_) => attr("label"),
        }
    }
}

/// Invariant: All these displays look like function calls, so follow the pattern `attr.foo(...)`.
impl Display for AttrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_default(f, None)
    }
}

impl AttrType {
    pub(crate) fn any() -> Self {
        Self(Arc::new(AttrTypeInner::Any(AnyAttrType)))
    }

    /// An arg attribute. Args are similar to strings, but have built in support
    /// for string parameter macros and make variables. Command line
    /// builders used in rule implementations use args (and so an arg attribute
    /// can be directly added to them).
    pub(crate) fn arg() -> Self {
        Self(Arc::new(AttrTypeInner::Arg(ArgAttrType)))
    }

    pub(crate) fn enumeration(variants: Vec<String>) -> anyhow::Result<Self> {
        Ok(Self(Arc::new(AttrTypeInner::Enum(EnumAttrType::new(
            variants,
        )?))))
    }

    pub(crate) fn bool() -> Self {
        Self(Arc::new(AttrTypeInner::Bool(BoolAttrType)))
    }

    pub(crate) fn int() -> Self {
        Self(Arc::new(AttrTypeInner::Int(IntAttrType)))
    }

    pub(crate) fn configuration_dep() -> Self {
        Self(Arc::new(AttrTypeInner::ConfigurationDep(
            ConfigurationDepAttrType,
        )))
    }

    /// A TargetLabel attribute optionally with a specific provider/providers
    /// that are expected.
    ///
    /// If `required_providers` is non-empty, the dependency must return those providers
    /// from its implementation function. Otherwise an error will result at resolution time.
    pub(crate) fn dep(required_providers: Vec<Arc<ProviderId>>) -> Self {
        Self(Arc::new(AttrTypeInner::Dep(DepAttrType::new(
            required_providers,
            DepAttrTransition::Identity,
        ))))
    }

    /// An execution dependency attribute optionally with a specific provider/providers
    /// that are expected.
    ///
    /// If `required_providers` is non-empty, the dependency must return those providers
    /// from its implementation function. Otherwise an error will result at resolution time.
    pub(crate) fn exec_dep(required_providers: Vec<Arc<ProviderId>>) -> Self {
        Self(Arc::new(AttrTypeInner::Dep(DepAttrType::new(
            required_providers,
            DepAttrTransition::Exec,
        ))))
    }

    /// An a dependency attribute which changes the configuration optionally with a specific
    /// provider/providers that are expected.
    ///
    /// If `required_providers` is non-empty, the dependency must return those providers
    /// from its implementation function. Otherwise an error will result at resolution time.
    pub(crate) fn transition_dep(
        required_providers: Vec<Arc<ProviderId>>,
        cfg: Arc<TransitionId>,
    ) -> Self {
        Self(Arc::new(AttrTypeInner::Dep(DepAttrType::new(
            required_providers,
            DepAttrTransition::Transition(cfg),
        ))))
    }

    pub(crate) fn configured_dep(required_providers: Vec<Arc<ProviderId>>) -> Self {
        Self(Arc::new(AttrTypeInner::ConfiguredDep(
            ExplicitConfiguredDepAttrType::new(required_providers),
        )))
    }

    pub(crate) fn split_transition_dep(
        required_providers: Vec<Arc<ProviderId>>,
        cfg: Arc<TransitionId>,
    ) -> Self {
        Self(Arc::new(AttrTypeInner::SplitTransitionDep(
            SplitTransitionDepAttrType::new(required_providers, cfg),
        )))
    }

    /// A dict attribute containing keys and values of the specified types.
    pub(crate) fn dict(key: AttrType, value: AttrType, sorted: bool) -> Self {
        Self(Arc::new(AttrTypeInner::Dict(DictAttrType::new(
            key, value, sorted,
        ))))
    }

    /// A list attribute containing items of some inner type.
    pub(crate) fn list(inner: AttrType) -> Self {
        Self(Arc::new(AttrTypeInner::List(ListAttrType::new(inner))))
    }

    pub(crate) fn tuple(xs: Vec<AttrType>) -> Self {
        Self(Arc::new(AttrTypeInner::Tuple(TupleAttrType::new(xs))))
    }

    pub(crate) fn one_of(xs: Vec<AttrType>) -> Self {
        Self(Arc::new(AttrTypeInner::OneOf(OneOfAttrType::new(xs))))
    }

    pub(crate) fn option(value: AttrType) -> Self {
        Self(Arc::new(AttrTypeInner::Option(OptionAttrType::new(value))))
    }

    pub(crate) fn default_only() -> Self {
        Self(Arc::new(AttrTypeInner::DefaultOnly(DefaultOnlyAttrType)))
    }

    pub(crate) fn query() -> Self {
        Self(Arc::new(AttrTypeInner::Query(QueryAttrType::new(
            DepAttrType::new(vec![], DepAttrTransition::Identity),
        ))))
    }

    // A file attribute. This will accept paths or targets like
    /// `//some:target[inner]`. When contained within a list, one item may
    /// expand to multiple (e.g. an output group or a lazy glob).
    pub(crate) fn source(allow_directory: bool) -> Self {
        Self(Arc::new(AttrTypeInner::Source(SourceAttrType {
            allow_directory,
        })))
    }

    /// A string attribute. For flags passed to a command, an arg() attr is
    /// preferred to support macro and make variable substitution.
    pub(crate) fn string() -> Self {
        Self(Arc::new(AttrTypeInner::String(StringAttrType)))
    }

    pub(crate) fn label() -> Self {
        Self(Arc::new(AttrTypeInner::Label(LabelAttrType)))
    }

    pub(crate) fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        self.0.coerce_item(configurable, ctx, value)
    }

    /// Used when we first detect that concatenation is going to happen for an attr
    /// while loading a build file. Returning false here will make us provide an error
    /// during the loading phase at the point that the concatenation happens.
    ///
    /// In some cases, we can't detect that the concattenation isn't allowed at this
    /// point and can only provide an error when performing the actual concatenation.
    pub(crate) fn supports_concat(&self) -> bool {
        match &*self.0 {
            AttrTypeInner::Bool(_)
            | AttrTypeInner::Query(_)
            | AttrTypeInner::Source(_)
            | AttrTypeInner::DefaultOnly(_) => false,
            // Reject if none of the inner types support concat. Mismatched types are rejected later.
            AttrTypeInner::OneOf(inner) => inner.any_supports_concat(),
            _ => true,
        }
    }

    pub(crate) fn coerce(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<CoercedAttr> {
        self.coerce_with_default(configurable, ctx, value, None)
    }

    pub(crate) fn coerce_with_default(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
        default: Option<&CoercedAttr>,
    ) -> anyhow::Result<CoercedAttr> {
        CoercedAttr::coerce(self, configurable, ctx, value, default)
    }

    pub(crate) fn starlark_type(&self) -> String {
        self.0.starlark_type()
    }
}

impl AttrTypeInner {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AttrLiteral<CoercedAttr>> {
        match self {
            Self::Any(x) => x.coerce_item(ctx, value),
            Self::Arg(x) => x.coerce_item(ctx, value),
            Self::Bool(x) => x.coerce_item(ctx, value),
            Self::Int(x) => x.coerce_item(ctx, value),
            Self::Dep(x) => x.coerce_item(ctx, value),
            Self::Dict(x) => x.coerce_item(configurable, ctx, value),
            Self::List(x) => x.coerce_item(configurable, ctx, value),
            Self::Tuple(x) => x.coerce_item(configurable, ctx, value),
            Self::OneOf(x) => x.coerce_item(configurable, ctx, value),
            Self::Option(x) => x.coerce_item(configurable, ctx, value),
            Self::Source(x) => x.coerce_item(ctx, value),
            Self::String(x) => x.coerce_item(ctx, value),
            Self::Query(x) => x.coerce_item(ctx, value),
            Self::ConfigurationDep(x) => x.coerce_item(ctx, value),
            Self::ConfiguredDep(x) => x.coerce_item(ctx, value),
            Self::DefaultOnly(x) => x.coerce_item(ctx, value),
            Self::Enum(x) => x.coerce_item(ctx, value),
            Self::SplitTransitionDep(x) => x.coerce_item(ctx, value),
            Self::Label(x) => x.coerce_item(ctx, value),
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
