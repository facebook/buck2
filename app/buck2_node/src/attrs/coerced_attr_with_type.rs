/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::TargetLabel;

use crate::attrs::attr_type::arg::ArgAttrType;
use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::bool::BoolAttrType;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use crate::attrs::attr_type::configured_dep::ExplicitConfiguredDepAttrType;
use crate::attrs::attr_type::configured_dep::UnconfiguredExplicitConfiguredDep;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dep::DepAttrType;
use crate::attrs::attr_type::dict::DictAttrType;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::enumeration::EnumAttrType;
use crate::attrs::attr_type::int::IntAttrType;
use crate::attrs::attr_type::label::LabelAttrType;
use crate::attrs::attr_type::list::ListAttrType;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::one_of::OneOfAttrType;
use crate::attrs::attr_type::option::OptionAttrType;
use crate::attrs::attr_type::query::QueryAttr;
use crate::attrs::attr_type::query::QueryAttrType;
use crate::attrs::attr_type::source::SourceAttrType;
use crate::attrs::attr_type::split_transition_dep::SplitTransitionDepAttrType;
use crate::attrs::attr_type::string::StringAttrType;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::tuple::TupleAttrType;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::attr_type::visibility::VisibilityAttrType;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::coerced_attr::CoercedSelector;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::visibility::VisibilitySpecification;

#[derive(Debug, thiserror::Error)]
enum CoercedAttrWithTypeError {
    #[error(
        "attr and type mismatch: {}, {}; ({:?}) (internal error)",
        _0.as_display_no_ctx(),
        _1,
        _0,
    )]
    Mismatch(CoercedAttr, AttrType),
    #[error("Unreachable: select already handled")]
    Select,
    #[error("Unreachable any")]
    Any,
}

/// Bind `CoercedAttr` to `AttrType`.
pub(crate) enum CoercedAttrWithType<'a, 't> {
    Selector(&'a CoercedSelector, &'t AttrType),
    Concat(&'a [CoercedAttr], &'t AttrType),

    Some(&'a CoercedAttr, &'t OptionAttrType),
    None,

    AnyList(&'a ListLiteral<CoercedAttr>),
    AnyTuple(&'a TupleLiteral<CoercedAttr>),
    AnyDict(&'a DictLiteral<CoercedAttr>),

    Bool(BoolLiteral, BoolAttrType),
    Int(i32, IntAttrType),
    String(&'a StringLiteral, StringAttrType),
    EnumVariant(&'a StringLiteral, &'t EnumAttrType),
    List(&'a ListLiteral<CoercedAttr>, &'t ListAttrType),
    Tuple(&'a TupleLiteral<CoercedAttr>, &'t TupleAttrType),
    Dict(&'a DictLiteral<CoercedAttr>, &'t DictAttrType),
    OneOf(&'a CoercedAttr, u32, &'t OneOfAttrType),
    Visibility(&'a VisibilitySpecification, VisibilityAttrType),
    ExplicitConfiguredDep(
        &'a UnconfiguredExplicitConfiguredDep,
        &'t ExplicitConfiguredDepAttrType,
    ),
    SplitTransitionDep(&'a ProvidersLabel, &'t SplitTransitionDepAttrType),
    ConfiguredDep(&'a DepAttr<ConfiguredProvidersLabel>),
    ConfigurationDep(&'a TargetLabel, ConfigurationDepAttrType),
    Dep(&'a ProvidersLabel, &'t DepAttrType),
    SourceLabel(&'a ProvidersLabel, SourceAttrType),
    Label(&'a ProvidersLabel, LabelAttrType),
    Arg(&'a StringWithMacros<ProvidersLabel>, ArgAttrType),
    Query(&'a QueryAttr<ProvidersLabel>, &'t QueryAttrType),
    SourceFile(&'a CoercedPath, SourceAttrType),
}

impl<'a, 't> CoercedAttrWithType<'a, 't> {
    #[inline]
    pub(crate) fn pack(
        attr: &'a CoercedAttr,
        ty: &'t AttrType,
    ) -> anyhow::Result<CoercedAttrWithType<'a, 't>> {
        match (attr, &*(ty.0)) {
            (CoercedAttr::Selector(s), _) => Ok(CoercedAttrWithType::Selector(s, ty)),
            (CoercedAttr::Concat(c), _) => Ok(CoercedAttrWithType::Concat(c, ty)),

            (CoercedAttr::None, _) => Ok(CoercedAttrWithType::None),
            (attr, AttrTypeInner::Option(t)) => Ok(CoercedAttrWithType::Some(attr, t)),

            (a, AttrTypeInner::Any(_)) => Self::pack_any(a),

            (CoercedAttr::Bool(b), AttrTypeInner::Bool(t)) => Ok(CoercedAttrWithType::Bool(*b, *t)),
            (CoercedAttr::Int(i), AttrTypeInner::Int(t)) => Ok(CoercedAttrWithType::Int(*i, *t)),
            (CoercedAttr::String(s), AttrTypeInner::String(t)) => {
                Ok(CoercedAttrWithType::String(s, *t))
            }
            (CoercedAttr::EnumVariant(s), AttrTypeInner::Enum(t)) => {
                Ok(CoercedAttrWithType::EnumVariant(s, t))
            }
            (CoercedAttr::List(l), AttrTypeInner::List(t)) => Ok(CoercedAttrWithType::List(l, t)),
            (CoercedAttr::Tuple(t), AttrTypeInner::Tuple(ty)) => {
                Ok(CoercedAttrWithType::Tuple(t, ty))
            }
            (CoercedAttr::Dict(d), AttrTypeInner::Dict(t)) => Ok(CoercedAttrWithType::Dict(d, t)),
            (CoercedAttr::OneOf(o, i), AttrTypeInner::OneOf(t)) => {
                Ok(CoercedAttrWithType::OneOf(o, *i, t))
            }
            (CoercedAttr::Visibility(v), AttrTypeInner::Visibility(t)) => {
                Ok(CoercedAttrWithType::Visibility(v, *t))
            }
            (CoercedAttr::ExplicitConfiguredDep(d), AttrTypeInner::ConfiguredDep(t)) => {
                Ok(CoercedAttrWithType::ExplicitConfiguredDep(d, t))
            }
            (CoercedAttr::SplitTransitionDep(d), AttrTypeInner::SplitTransitionDep(t)) => {
                Ok(CoercedAttrWithType::SplitTransitionDep(d, t))
            }
            (CoercedAttr::ConfiguredDep(d), _) => Ok(CoercedAttrWithType::ConfiguredDep(d)),
            (CoercedAttr::ConfigurationDep(d), AttrTypeInner::ConfigurationDep(t)) => {
                Ok(CoercedAttrWithType::ConfigurationDep(d, *t))
            }
            (CoercedAttr::Dep(d), AttrTypeInner::Dep(t)) => Ok(CoercedAttrWithType::Dep(d, t)),
            (CoercedAttr::SourceLabel(s), AttrTypeInner::Source(t)) => {
                Ok(CoercedAttrWithType::SourceLabel(s, *t))
            }
            (CoercedAttr::Label(l), AttrTypeInner::Label(t)) => {
                Ok(CoercedAttrWithType::Label(l, *t))
            }
            (CoercedAttr::Arg(a), AttrTypeInner::Arg(t)) => Ok(CoercedAttrWithType::Arg(a, *t)),
            (CoercedAttr::Query(q), AttrTypeInner::Query(t)) => {
                Ok(CoercedAttrWithType::Query(q, t))
            }
            (CoercedAttr::SourceFile(p), AttrTypeInner::Source(t)) => {
                Ok(CoercedAttrWithType::SourceFile(p, *t))
            }

            // Explicitly list the remaining pattern to make sure nothing is forgotten.
            (CoercedAttr::Bool(_), _)
            | (CoercedAttr::Int(_), _)
            | (CoercedAttr::String(_), _)
            | (CoercedAttr::EnumVariant(_), _)
            | (CoercedAttr::List(_), _)
            | (CoercedAttr::Tuple(_), _)
            | (CoercedAttr::Dict(_), _)
            | (CoercedAttr::OneOf(..), _)
            | (CoercedAttr::Visibility(_), _)
            | (CoercedAttr::ExplicitConfiguredDep(_), _)
            | (CoercedAttr::SplitTransitionDep(_), _)
            | (CoercedAttr::ConfigurationDep(_), _)
            | (CoercedAttr::Dep(_), _)
            | (CoercedAttr::SourceLabel(_), _)
            | (CoercedAttr::Label(_), _)
            | (CoercedAttr::Arg(_), _)
            | (CoercedAttr::Query(_), _)
            | (CoercedAttr::SourceFile(_), _) => {
                Err(CoercedAttrWithTypeError::Mismatch(attr.clone(), ty.clone()).into())
            }
        }
    }

    #[inline]
    fn pack_any(attr: &'a CoercedAttr) -> anyhow::Result<CoercedAttrWithType<'a, 't>> {
        match attr {
            CoercedAttr::Selector(_) | CoercedAttr::Concat(_) => {
                Err(CoercedAttrWithTypeError::Select.into())
            }
            CoercedAttr::Bool(b) => Ok(CoercedAttrWithType::Bool(*b, BoolAttrType)),
            CoercedAttr::Int(i) => Ok(CoercedAttrWithType::Int(*i, IntAttrType)),
            CoercedAttr::String(s) => Ok(CoercedAttrWithType::String(s, StringAttrType)),
            CoercedAttr::List(l) => Ok(CoercedAttrWithType::AnyList(l)),
            CoercedAttr::Tuple(t) => Ok(CoercedAttrWithType::AnyTuple(t)),
            CoercedAttr::Dict(d) => Ok(CoercedAttrWithType::AnyDict(d)),
            CoercedAttr::None => Ok(CoercedAttrWithType::None),
            CoercedAttr::OneOf(_, _)
            | CoercedAttr::Visibility(_)
            | CoercedAttr::ExplicitConfiguredDep(_)
            | CoercedAttr::SplitTransitionDep(_)
            | CoercedAttr::ConfiguredDep(_)
            | CoercedAttr::ConfigurationDep(_)
            | CoercedAttr::Dep(_)
            | CoercedAttr::SourceLabel(_)
            | CoercedAttr::Label(_)
            | CoercedAttr::Arg(_)
            | CoercedAttr::Query(_)
            | CoercedAttr::EnumVariant(_)
            | CoercedAttr::SourceFile(_) => Err(CoercedAttrWithTypeError::Any.into()),
        }
    }
}
