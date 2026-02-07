/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Contains the internal support within the attribute framework for `select()`.

use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_interpreter::types::select_fail::StarlarkSelectFail;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coerced_attr::CoercedConcat;
use buck2_node::attrs::coerced_attr::CoercedSelector;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use starlark::values::Value;
use starlark::values::dict::DictRef;

use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::interpreter::selector::StarlarkSelector;
use crate::interpreter::selector::StarlarkSelectorGen;

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum SelectError {
    #[error("select() condition was not a string, got `{0}`.")]
    KeyNotString(String),
    #[error("select() value was not a dict, got `{0}`.")]
    ValueNotDict(String),
    #[error("addition not supported for this attribute type `{0}`, got `{1}`.")]
    ConcatNotSupported(String, String),
    #[error("select() cannot be used in non-configurable attribute")]
    SelectCannotBeUsedForNonConfigurableAttr,
}

pub trait CoercedAttrExr: Sized {
    fn coerce(
        attr: &AttrType,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
        default_attr: Option<&Self>,
    ) -> buck2_error::Result<Self>;
}

impl CoercedAttrExr for CoercedAttr {
    fn coerce(
        attr: &AttrType,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
        default_attr: Option<&Self>,
    ) -> buck2_error::Result<Self> {
        // A Selector in starlark is currently implemented as simply a Value (holding a
        // dict if valid).
        //
        // TODO(cjhopman): the select() function itself should
        // perform the conversion of its case arguments to configuration labels.
        //
        // TODO(cjhopman): Selectable addition (__ladd__ and __radd__) should perform
        // verification that the two sides of the addition have the same type.
        // Even if it did, we still need to verify that the two sides
        // are actually compatible (i.e. selectable can ensure that both sides are
        // lists, we can ensure that  both sides are List<T>)
        if let Some(selector) = StarlarkSelector::from_value(value) {
            if let AttrIsConfigurable::No = configurable {
                return Err(SelectError::SelectCannotBeUsedForNonConfigurableAttr.into());
            }

            match *selector {
                StarlarkSelectorGen::Primary(v) => {
                    if let Some(dict) = DictRef::from_value(v.get()) {
                        let has_default = dict.get_str("DEFAULT").is_some();
                        let mut entries =
                            Vec::with_capacity(dict.len().saturating_sub(has_default as usize));

                        let mut default = None;
                        for (k, v) in dict.iter() {
                            let k = k
                                .unpack_str()
                                .ok_or_else(|| SelectError::KeyNotString(k.to_repr()))?;
                            let v = match default_attr {
                                Some(default_attr) if v.is_none() => default_attr.clone(),
                                _ => match CoercedAttr::coerce(
                                    attr,
                                    configurable,
                                    ctx,
                                    v,
                                    default_attr,
                                ) {
                                    Ok(v) => v,
                                    Err(e) => {
                                        if let Some(select_fail) = StarlarkSelectFail::from_value(v)
                                        {
                                            CoercedAttr::SelectFail(
                                                ctx.intern_str(select_fail.as_str()),
                                            )
                                        } else {
                                            return Err(e);
                                        }
                                    }
                                },
                            };
                            if k == "DEFAULT" {
                                if default.is_some() {
                                    return Err(internal_error!(
                                        "duplicate `\"DEFAULT\"` key in `select()`"
                                    ));
                                }
                                default = Some(v);
                            } else {
                                let label = ctx.coerce_providers_label(k)?;
                                entries.push((ConfigurationSettingKey(label), v));
                            }
                        }

                        assert_eq!(entries.capacity(), entries.len());

                        Ok(CoercedAttr::Selector(Box::new(CoercedSelector::new(
                            ctx.intern_select(entries),
                            default,
                        )?)))
                    } else {
                        Err(SelectError::ValueNotDict(v.get().to_repr()).into())
                    }
                }
                StarlarkSelectorGen::Sum(l, r) => {
                    if !attr.supports_concat() {
                        return Err(SelectError::ConcatNotSupported(
                            attr.to_string(),
                            format!("{l} + {r}"),
                        )
                        .into());
                    }
                    let l = CoercedAttr::coerce(attr, configurable, ctx, l, None)?;
                    let mut l = match l {
                        CoercedAttr::Concat(l) => l.0.into_vec(),
                        l => vec![l],
                    };
                    match CoercedAttr::coerce(attr, configurable, ctx, r, None)? {
                        CoercedAttr::Concat(r) => {
                            l.extend(r.0.into_vec());
                        }
                        r => l.push(r),
                    };

                    Ok(CoercedAttr::Concat(CoercedConcat(l.into_boxed_slice())))
                }
            }
        } else {
            Ok(attr
                .coerce_item(configurable, ctx, value)
                .with_buck_error_context(|| format!("Error coercing {value}"))?)
        }
    }
}
