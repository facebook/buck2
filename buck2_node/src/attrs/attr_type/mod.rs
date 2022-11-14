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

use allocative::Allocative;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::id::ProviderId;
use gazebo::dupe::Dupe;

use crate::attrs::attr_type::any::AnyAttrType;
use crate::attrs::attr_type::arg::ArgAttrType;
use crate::attrs::attr_type::bool::BoolAttrType;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use crate::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use crate::attrs::attr_type::default_only::DefaultOnlyAttrType;
use crate::attrs::attr_type::dep::DepAttrTransition;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::dict::DictAttrType;
use crate::attrs::attr_type::enumeration::EnumAttrType;
use crate::attrs::attr_type::int::IntAttrType;
use crate::attrs::attr_type::label::LabelAttrType;
use crate::attrs::attr_type::list::ListAttrType;
use crate::attrs::attr_type::one_of::OneOfAttrType;
use crate::attrs::attr_type::option::OptionAttrType;
use crate::attrs::attr_type::query::QueryAttrType;
use crate::attrs::attr_type::source::SourceAttrType;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use crate::attrs::attr_type::string::StringAttrType;
use crate::attrs::attr_type::tuple::TupleAttrType;

pub mod any;
pub mod arg;
pub mod attr_config;
pub mod attr_like;
pub mod attr_literal;
pub mod bool;
pub mod configuration_dep;
pub mod configured_dep;
pub mod default_only;
pub mod dep;
pub mod dict;
pub mod enumeration;
pub mod int;
pub mod label;
pub mod list;
pub mod one_of;
pub mod option;
pub mod query;
pub mod source;
pub mod split_transition_dep;
pub mod string;
pub mod tuple;

#[derive(Clone, Dupe, Debug, Hash, Eq, PartialEq, Allocative)]
pub struct AttrType(pub Arc<AttrTypeInner>);

#[derive(Debug, Hash, Eq, PartialEq, Allocative)]
pub enum AttrTypeInner {
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
    pub fn fmt_with_default(
        &self,
        f: &mut fmt::Formatter<'_>,
        default: Option<&str>,
    ) -> fmt::Result {
        let mut attr = |s| match default {
            None => write!(f, "attrs.{}()", s),
            Some(default) => write!(f, "attrs.{}(default={})", s, default),
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

    pub fn any() -> Self {
        Self(Arc::new(AttrTypeInner::Any(AnyAttrType)))
    }

    /// An arg attribute. Args are similar to strings, but have built in support
    /// for string parameter macros and make variables. Command line
    /// builders used in rule implementations use args (and so an arg attribute
    /// can be directly added to them).
    pub fn arg() -> Self {
        Self(Arc::new(AttrTypeInner::Arg(ArgAttrType)))
    }

    pub fn enumeration(variants: Vec<String>) -> anyhow::Result<Self> {
        Ok(Self(Arc::new(AttrTypeInner::Enum(EnumAttrType::new(
            variants,
        )?))))
    }

    pub fn bool() -> Self {
        Self(Arc::new(AttrTypeInner::Bool(BoolAttrType)))
    }

    pub fn int() -> Self {
        Self(Arc::new(AttrTypeInner::Int(IntAttrType)))
    }

    pub fn configuration_dep() -> Self {
        Self(Arc::new(AttrTypeInner::ConfigurationDep(
            ConfigurationDepAttrType,
        )))
    }

    /// A TargetLabel attribute optionally with a specific provider/providers
    /// that are expected.
    ///
    /// If `required_providers` is non-empty, the dependency must return those providers
    /// from its implementation function. Otherwise an error will result at resolution time.
    pub fn dep(required_providers: Vec<Arc<ProviderId>>) -> Self {
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
    pub fn exec_dep(required_providers: Vec<Arc<ProviderId>>) -> Self {
        Self(Arc::new(AttrTypeInner::Dep(DepAttrType::new(
            required_providers,
            DepAttrTransition::Exec,
        ))))
    }

    /// A toolchain dependency attribute optionally with a specific provider/providers
    /// that are expected.
    ///
    /// If `required_providers` is non-empty, the dependency must return those providers
    /// from its implementation function. Otherwise an error will result at resolution time.
    pub fn toolchain_dep(required_providers: Vec<Arc<ProviderId>>) -> Self {
        Self(Arc::new(AttrTypeInner::Dep(DepAttrType::new(
            required_providers,
            DepAttrTransition::Toolchain,
        ))))
    }

    /// An a dependency attribute which changes the configuration optionally with a specific
    /// provider/providers that are expected.
    ///
    /// If `required_providers` is non-empty, the dependency must return those providers
    /// from its implementation function. Otherwise an error will result at resolution time.
    pub fn transition_dep(
        required_providers: Vec<Arc<ProviderId>>,
        cfg: Arc<TransitionId>,
    ) -> Self {
        Self(Arc::new(AttrTypeInner::Dep(DepAttrType::new(
            required_providers,
            DepAttrTransition::Transition(cfg),
        ))))
    }

    pub fn configured_dep(required_providers: Vec<Arc<ProviderId>>) -> Self {
        Self(Arc::new(AttrTypeInner::ConfiguredDep(
            ExplicitConfiguredDepAttrType::new(required_providers),
        )))
    }

    pub fn split_transition_dep(
        required_providers: Vec<Arc<ProviderId>>,
        cfg: Arc<TransitionId>,
    ) -> Self {
        Self(Arc::new(AttrTypeInner::SplitTransitionDep(
            SplitTransitionDepAttrType::new(required_providers, cfg),
        )))
    }

    /// A dict attribute containing keys and values of the specified types.
    pub fn dict(key: AttrType, value: AttrType, sorted: bool) -> Self {
        Self(Arc::new(AttrTypeInner::Dict(DictAttrType::new(
            key, value, sorted,
        ))))
    }

    /// A list attribute containing items of some inner type.
    pub fn list(inner: AttrType) -> Self {
        Self(Arc::new(AttrTypeInner::List(ListAttrType::new(inner))))
    }

    pub fn tuple(xs: Vec<AttrType>) -> Self {
        Self(Arc::new(AttrTypeInner::Tuple(TupleAttrType::new(xs))))
    }

    pub fn one_of(xs: Vec<AttrType>) -> Self {
        Self(Arc::new(AttrTypeInner::OneOf(OneOfAttrType::new(xs))))
    }

    pub fn option(value: AttrType) -> Self {
        Self(Arc::new(AttrTypeInner::Option(OptionAttrType::new(value))))
    }

    pub fn default_only() -> Self {
        Self(Arc::new(AttrTypeInner::DefaultOnly(DefaultOnlyAttrType)))
    }

    pub fn query() -> Self {
        Self(Arc::new(AttrTypeInner::Query(QueryAttrType::new(
            DepAttrType::new(vec![], DepAttrTransition::Identity),
        ))))
    }

    // A file attribute. This will accept paths or targets like
    /// `//some:target[inner]`. When contained within a list, one item may
    /// expand to multiple (e.g. an output group or a lazy glob).
    pub fn source(allow_directory: bool) -> Self {
        Self(Arc::new(AttrTypeInner::Source(SourceAttrType {
            allow_directory,
        })))
    }

    /// A string attribute. For flags passed to a command, an arg() attr is
    /// preferred to support macro and make variable substitution.
    pub fn string() -> Self {
        Self(Arc::new(AttrTypeInner::String(StringAttrType)))
    }

    pub fn label() -> Self {
        Self(Arc::new(AttrTypeInner::Label(LabelAttrType)))
    }

    /// Used when we first detect that concatenation is going to happen for an attr
    /// while loading a build file. Returning false here will make us provide an error
    /// during the loading phase at the point that the concatenation happens.
    ///
    /// In some cases, we can't detect that the concattenation isn't allowed at this
    /// point and can only provide an error when performing the actual concatenation.
    pub fn supports_concat(&self) -> bool {
        match &*self.0 {
            AttrTypeInner::Bool(_)
            | AttrTypeInner::Query(_)
            | AttrTypeInner::Source(_)
            | AttrTypeInner::DefaultOnly(_)
            | AttrTypeInner::ConfigurationDep(_)
            | AttrTypeInner::ConfiguredDep(_)
            | AttrTypeInner::Int(_)
            | AttrTypeInner::Dep(_)
            | AttrTypeInner::Tuple(_)
            | AttrTypeInner::SplitTransitionDep(_)
            | AttrTypeInner::Label(_)
            | AttrTypeInner::Enum(_) => false,
            AttrTypeInner::Any(_)
            | AttrTypeInner::Arg(_)
            | AttrTypeInner::Dict(_)
            | AttrTypeInner::List(_)
            | AttrTypeInner::String(_) => true,
            AttrTypeInner::Option(inner) => inner.inner.supports_concat(),
            // Reject if none of the inner types support concat. Mismatched types are rejected later.
            AttrTypeInner::OneOf(inner) => inner.any_supports_concat(),
        }
    }
}

/// Invariant: All these displays look like function calls, so follow the pattern `attrs.foo(...)`.
impl Display for AttrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_default(f, None)
    }
}
