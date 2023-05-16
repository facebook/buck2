/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;
use std::fmt::Debug;
use std::iter;

use buck2_core::soft_error;
use buck2_node::attrs::attr_type::bool::BoolLiteral;
use buck2_node::attrs::attr_type::dep::DepAttr;
use buck2_node::attrs::attr_type::dict::DictAttrType;
use buck2_node::attrs::attr_type::dict::DictLiteral;
use buck2_node::attrs::attr_type::list::ListAttrType;
use buck2_node::attrs::attr_type::list::ListLiteral;
use buck2_node::attrs::attr_type::one_of::OneOfAttrType;
use buck2_node::attrs::attr_type::string::StringLiteral;
use buck2_node::attrs::attr_type::tuple::TupleAttrType;
use buck2_node::attrs::attr_type::tuple::TupleLiteral;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::attr_type::AttrTypeInner;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use dupe::Dupe;
use dupe::IterDupedExt;
use gazebo::prelude::SliceExt;
use starlark::values::dict::Dict;
use starlark::values::dict::DictRef;
use starlark::values::list::ListRef;
use starlark::values::string::STRING_TYPE;
use starlark::values::tuple::TupleRef;
use starlark::values::Value;

use crate::analysis::anon_target_attr::AnonTargetAttr;
use crate::interpreter::rule_defs::provider::dependency::Dependency;

pub trait AnonTargetAttrTypeCoerce {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        _ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AnonTargetAttr>;
}

impl AnonTargetAttrTypeCoerce for AttrType {
    fn coerce_item(
        &self,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
    ) -> anyhow::Result<AnonTargetAttr> {
        match self.0.as_ref() {
            AttrTypeInner::Any(_) => to_anon_target_any(value, ctx),
            AttrTypeInner::Bool(_) => match value.unpack_bool() {
                Some(s) => Ok(AnonTargetAttr::Bool(BoolLiteral(s))),
                None => Err(anyhow::anyhow!(AnonTargetCoercionError::type_error(
                    "bool", value
                ))),
            },
            AttrTypeInner::Int(_) => match value.unpack_int() {
                Some(x) => Ok(AnonTargetAttr::Int(x)),
                None => Err(anyhow::anyhow!(AnonTargetCoercionError::type_error(
                    "int", value
                ))),
            },
            AttrTypeInner::Dict(x) => to_anon_target_dict(x, configurable, ctx, value),
            AttrTypeInner::List(x) => to_anon_target_list(x, configurable, ctx, value),
            AttrTypeInner::Tuple(x) => to_anon_target_tuple(x, configurable, ctx, value),
            AttrTypeInner::OneOf(x) => to_anon_target_one_of(x, configurable, ctx, value),
            AttrTypeInner::Option(x) => {
                if value.is_none() {
                    Ok(AnonTargetAttr::None)
                } else {
                    Ok(x.inner.coerce_item(configurable, ctx, value)?)
                }
            }
            AttrTypeInner::String(_) => match value.unpack_str() {
                Some(s) => Ok(AnonTargetAttr::String(StringLiteral(ctx.intern_str(s)))),
                None => Err(anyhow::anyhow!(AnonTargetCoercionError::type_error(
                    STRING_TYPE,
                    value
                ))),
            },
            AttrTypeInner::Enum(x) => match value.unpack_str() {
                Some(s) => {
                    // Enum names in Buck can be specified upper or lower case,
                    // so we normalise them to lowercase to make rule implementations easier
                    let s = s.to_lowercase();
                    if let Some(s) = x.variants.get(s.as_str()) {
                        Ok(AnonTargetAttr::EnumVariant(StringLiteral(s.dupe())))
                    } else {
                        let wanted = x.variants.iter().map(|x| x.as_str().to_owned()).collect();
                        Err(AnonTargetCoercionError::InvalidEnumVariant(s, wanted).into())
                    }
                }
                None => Err(AnonTargetCoercionError::type_error(STRING_TYPE, value).into()),
            },
            AttrTypeInner::Dep(x) => match Dependency::from_value(value) {
                Some(dep) => {
                    let label = dep.label().inner().clone();

                    Ok(AnonTargetAttr::Dep(Box::new(DepAttr {
                        attr_type: x.clone(),
                        label,
                    })))
                }
                _ => {
                    return Err(
                        AnonTargetCoercionError::InvalidDep(value.get_type().to_owned()).into(),
                    );
                }
            },
            _ => {
                return Err(AnonTargetCoercionError::AttrTypeNotSupported(
                    value.get_type().to_owned(),
                )
                .into());
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum AnonTargetCoercionError {
    #[error("Expected value of type `{0}`, got value with type `{1}` (value was `{2}`)")]
    TypeError(String, String, String),
    #[error("Used one_of with an empty list.")]
    OneOfEmpty,
    #[error("one_of fails, the errors against each alternative in turn were:\n{}", .0.map(|x| format!("{:#}", x)).join("\n"))]
    OneOfMany(Vec<anyhow::Error>),
    #[error("enum called with `{0}`, only allowed: {}", .1.map(|x| format!("`{}`", x)).join(", "))]
    InvalidEnumVariant(String, Vec<String>),
    #[error("Invalid `attr.dep` value, expected `dependency`, got `{0}`")]
    InvalidDep(String),
    #[error("Cannot coerce value of type `{0}` to any: `{1}`")]
    CannotCoerceToAny(&'static str, String),
    #[error("Attr value of type `{0}` not supported")]
    AttrTypeNotSupported(String),
}

impl AnonTargetCoercionError {
    pub(crate) fn type_error(expected_type: &str, value: Value) -> AnonTargetCoercionError {
        AnonTargetCoercionError::TypeError(
            expected_type.to_owned(),
            value.get_type().to_owned(),
            value.to_repr(),
        )
    }

    pub fn one_of_many(mut errs: Vec<anyhow::Error>) -> anyhow::Error {
        if errs.is_empty() {
            AnonTargetCoercionError::OneOfEmpty.into()
        } else if errs.len() == 1 {
            errs.pop().unwrap()
        } else {
            AnonTargetCoercionError::OneOfMany(errs).into()
        }
    }
}

fn to_anon_target_any(
    value: Value,
    ctx: &dyn AttrCoercionContext,
) -> anyhow::Result<AnonTargetAttr> {
    if value.is_none() {
        Ok(AnonTargetAttr::None)
    } else if let Some(x) = value.unpack_bool() {
        Ok(AnonTargetAttr::Bool(BoolLiteral(x)))
    } else if let Some(x) = value.unpack_int() {
        Ok(AnonTargetAttr::Int(x))
    } else if let Some(x) = DictRef::from_value(value) {
        Ok(AnonTargetAttr::Dict(
            x.iter()
                .map(|(k, v)| Ok((to_anon_target_any(k, ctx)?, to_anon_target_any(v, ctx)?)))
                .collect::<anyhow::Result<_>>()?,
        ))
    } else if let Some(x) = TupleRef::from_value(value) {
        // TODO(wendyy) intern attr
        Ok(AnonTargetAttr::Tuple(TupleLiteral(
            x.iter()
                .map(|v| to_anon_target_any(v, ctx))
                .collect::<anyhow::Result<Vec<_>>>()?
                .into(),
        )))
    } else if let Some(x) = ListRef::from_value(value) {
        // TODO(wendyy) intern attr
        Ok(AnonTargetAttr::List(ListLiteral(
            x.iter()
                .map(|v| to_anon_target_any(v, ctx))
                .collect::<anyhow::Result<Vec<_>>>()?
                .into(),
        )))
    } else if let Some(s) = value.unpack_str() {
        Ok(AnonTargetAttr::String(StringLiteral(ctx.intern_str(s))))
    } else {
        soft_error!(
            "coerce_to_any",
            AnonTargetCoercionError::CannotCoerceToAny(value.get_type(), value.to_repr()).into()
        )?;
        Ok(AnonTargetAttr::String(StringLiteral(
            ctx.intern_str(&value.to_str()),
        )))
    }
}

fn to_anon_target_dict(
    dict_attr_type: &DictAttrType,
    configurable: AttrIsConfigurable,
    ctx: &dyn AttrCoercionContext,
    value: Value,
) -> anyhow::Result<AnonTargetAttr> {
    if let Some(dict) = DictRef::from_value(value) {
        let mut res = Vec::with_capacity(dict.len());
        if dict_attr_type.sorted {
            // First sort the values
            let mut items = dict.iter().collect::<Vec<_>>();
            // If two things are incompatible, just return Eq. The resulting order is undefined, but safely undefined.
            items.sort_by(|a, b| a.0.compare(b.0).unwrap_or(Ordering::Equal));

            for (k, v) in items {
                res.push((
                    dict_attr_type.key.coerce_item(configurable, ctx, k)?,
                    dict_attr_type.value.coerce_item(configurable, ctx, v)?,
                ));
            }
        } else {
            for (k, v) in dict.iter() {
                res.push((
                    dict_attr_type.key.coerce_item(configurable, ctx, k)?,
                    dict_attr_type.value.coerce_item(configurable, ctx, v)?,
                ));
            }
        }
        Ok(AnonTargetAttr::Dict(DictLiteral(res.into())))
    } else {
        Err(anyhow::anyhow!(AnonTargetCoercionError::type_error(
            Dict::TYPE,
            value,
        )))
    }
}

fn to_anon_target_one_of(
    one_of_attr_type: &OneOfAttrType,
    configurable: AttrIsConfigurable,
    ctx: &dyn AttrCoercionContext,
    value: Value,
) -> anyhow::Result<AnonTargetAttr> {
    let mut errs = Vec::new();
    // Bias towards the start of the list - try and use success/failure from first in preference
    for (i, x) in one_of_attr_type.xs.iter().enumerate() {
        match x.coerce_item(configurable, ctx, value) {
            Ok(v) => return Ok(AnonTargetAttr::OneOf(Box::new(v), i as u32)),
            Err(e) => {
                // TODO(nga): anyhow error creation is expensive.
                errs.push(e)
            }
        }
    }
    Err(AnonTargetCoercionError::one_of_many(errs))
}

fn to_anon_target_tuple(
    tuple_attr_type: &TupleAttrType,
    configurable: AttrIsConfigurable,
    ctx: &dyn AttrCoercionContext,
    value: Value,
) -> anyhow::Result<AnonTargetAttr> {
    let coerce = |value, items: &[Value]| {
        // Use comparison rather than equality below. If the tuple is too short,
        // it is implicitly extended using None.
        if items.len() <= tuple_attr_type.xs.len() {
            let mut res = Vec::with_capacity(tuple_attr_type.xs.len());
            for (c, v) in tuple_attr_type
                .xs
                .iter()
                .zip(items.iter().duped().chain(iter::repeat(Value::new_none())))
            {
                res.push(c.coerce_item(configurable, ctx, v)?);
            }
            // TODO(wendyy) intern attr
            Ok(AnonTargetAttr::Tuple(TupleLiteral(res.into())))
        } else {
            Err(anyhow::anyhow!(AnonTargetCoercionError::type_error(
                &format!("Tuple of at most length {}", tuple_attr_type.xs.len()),
                value
            )))
        }
    };
    if let Some(list) = TupleRef::from_value(value) {
        coerce(value, list.content())
    } else if let Some(list) = ListRef::from_value(value) {
        coerce(value, list.content())
    } else {
        Err(anyhow::anyhow!(AnonTargetCoercionError::type_error(
            TupleRef::TYPE,
            value,
        )))
    }
}

fn to_anon_target_list(
    list_attr_type: &ListAttrType,
    configurable: AttrIsConfigurable,
    ctx: &dyn AttrCoercionContext,
    value: Value,
) -> anyhow::Result<AnonTargetAttr> {
    if let Some(list) = ListRef::from_value(value) {
        Ok(AnonTargetAttr::List(ListLiteral(
            // TODO(wendyy) intern attr
            list.content()
                .try_map(|v| list_attr_type.inner.coerce_item(configurable, ctx, *v))?
                .into(),
        )))
    } else if let Some(list) = TupleRef::from_value(value) {
        Ok(AnonTargetAttr::List(ListLiteral(
            // TODO(wendyy) intern attr
            list.content()
                .try_map(|v| list_attr_type.inner.coerce_item(configurable, ctx, *v))?
                .into(),
        )))
    } else {
        Err(anyhow::anyhow!(AnonTargetCoercionError::type_error(
            ListRef::TYPE,
            value,
        )))
    }
}
