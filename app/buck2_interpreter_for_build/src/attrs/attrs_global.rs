/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context as _;
use buck2_core::provider::label::ProvidersLabel;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::attr_type::any::AnyAttrType;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use buck2_node::provider_id_set::ProviderIdSet;
use dupe::Dupe;
use dupe::OptionDupedExt;
use gazebo::prelude::*;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::Value;
use starlark::values::ValueError;
use thiserror::Error;
use tracing::error;

use crate::attrs::attribute_as_starlark_value::AttributeAsStarlarkValue;
use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::attrs::coerce::ctx::BuildAttrCoercionContext;
use crate::interpreter::build_context::BuildContext;
use crate::provider::callable::ValueAsProviderCallableLike;
use crate::transition::transition_id_from_value;

const OPTION_NONE_EXPLANATION: &str = "`None` as an attribute value always picks the default. For `attrs.option`, if the default isn't `None`, there is no way to express `None`.";

#[derive(Error, Debug)]
enum AttrError {
    #[error(
        "`attrs.option` `default` parameter must be `None` or absent, got `{0}`.\n{}",
        OPTION_NONE_EXPLANATION
    )]
    OptionDefaultNone(String),
    #[error("`attrs.default_only` argument must have a default")]
    DefaultOnlyMustHaveDefault,
}

pub trait AttributeExt {
    /// Helper to create an attribute from attrs.foo functions
    fn attr<'v>(
        eval: &mut Evaluator<'v, '_>,
        default: Option<Value<'v>>,
        doc: &str,
        coercer: AttrType,
    ) -> anyhow::Result<AttributeAsStarlarkValue>;

    /// An `attr` which is not allowed to have a default as a relative label.
    fn check_not_relative_label<'v>(default: Option<Value<'v>>, attr: &str) -> anyhow::Result<()>;
}

impl AttributeExt for Attribute {
    /// Helper to create an attribute from attrs.foo functions
    fn attr<'v>(
        eval: &mut Evaluator<'v, '_>,
        default: Option<Value<'v>>,
        doc: &str,
        coercer: AttrType,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let default = match default {
            None => None,
            Some(x) => Some(Arc::new(
                coercer
                    .coerce(
                        AttrIsConfigurable::Yes,
                        &get_attr_coercion_context(eval)?,
                        x,
                    )
                    .context("When coercing attribute default")?,
            )),
        };
        Ok(AttributeAsStarlarkValue::new(Attribute::new(
            default, doc, coercer,
        )))
    }

    /// An `attr` which is not allowed to have a default as a relative label.
    fn check_not_relative_label<'v>(default: Option<Value<'v>>, attr: &str) -> anyhow::Result<()> {
        if let Some(as_str) = default.and_then(|x| x.unpack_str()) {
            if ProvidersLabel::maybe_relative_label(as_str) {
                return Err(DepError::DepRelativeDefault {
                    invalid_label: as_str.to_owned(),
                    attr: attr.to_owned(),
                }
                .into());
            }
        }
        Ok(())
    }
}

/// Grab a new coercion context object based on the main build file that is being evaluated.
/// This is used because we do not have access to a specific shared instance via ctx.extra
/// when evaluating .bzl files
pub fn get_attr_coercion_context<'v>(
    eval: &Evaluator<'v, '_>,
) -> anyhow::Result<BuildAttrCoercionContext> {
    Ok(BuildAttrCoercionContext::new_no_package(
        BuildContext::from_context(eval)?
            .cell_info()
            .cell_alias_resolver()
            .dupe(),
    ))
}

#[derive(Debug, thiserror::Error)]
enum DepError {
    #[error(
        "relative labels ('{invalid_label}') are not permitted as default values for `{attr}` \
        attributes. Use a fully qualified one like //foo:bar"
    )]
    DepRelativeDefault { invalid_label: String, attr: String },
}

/// Common code to handle `providers` argument of dep-like attrs.
fn dep_like_attr_handle_providers_arg(providers: Vec<Value>) -> anyhow::Result<ProviderIdSet> {
    Ok(ProviderIdSet::from(providers.try_map(
        |v| match v.as_provider_callable() {
            Some(callable) => callable.require_id(),
            None => Err(ValueError::IncorrectParameterTypeNamed("providers".to_owned()).into()),
        },
    )?))
}

/// Fields of `attrs` struct.
#[starlark_module]
fn attr_module(registry: &mut GlobalsBuilder) {
    fn string<'v>(
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[allow(unused_variables)]
        #[starlark(require = named)]
        validate: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::string())
    }

    fn list<'v>(
        #[starlark(require = pos)] inner: &AttributeAsStarlarkValue,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::list(inner.coercer_for_inner()?);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn exec_dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::check_not_relative_label(default, "attrs.exec_dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let coercer = AttrType::exec_dep(required_providers);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn toolchain_dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::check_not_relative_label(default, "attrs.toolchain_dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let coercer = AttrType::toolchain_dep(required_providers);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn transition_dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        cfg: Value<'v>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::check_not_relative_label(default, "attrs.transition_dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let transition_id = transition_id_from_value(cfg)?;
        let coercer = AttrType::transition_dep(required_providers, transition_id);

        let coerced_default = match default {
            None => None,
            Some(default) => {
                match coercer.coerce(
                    AttrIsConfigurable::Yes,
                    &get_attr_coercion_context(eval)?,
                    default,
                ) {
                    Ok(coerced_default) => Some(coerced_default),
                    Err(_) => return Err(ValueError::IncorrectParameterType.into()),
                }
            }
        };

        Ok(AttributeAsStarlarkValue::new(Attribute::new(
            coerced_default.map(Arc::new),
            doc,
            coercer,
        )))
    }

    fn configured_dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::check_not_relative_label(default, "attrs.configured_dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let coercer = AttrType::configured_dep(required_providers);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn split_transition_dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        cfg: Value<'v>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::check_not_relative_label(default, "attrs.split_transition_dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let transition_id = transition_id_from_value(cfg)?;
        let coercer = AttrType::split_transition_dep(required_providers, transition_id);

        let coerced_default = match default {
            None => None,
            Some(default) => {
                match coercer.coerce(
                    AttrIsConfigurable::Yes,
                    &get_attr_coercion_context(eval)?,
                    default,
                ) {
                    Ok(coerced_default) => Some(coerced_default),
                    Err(_) => return Err(ValueError::IncorrectParameterType.into()),
                }
            }
        };

        Ok(AttributeAsStarlarkValue::new(Attribute::new(
            coerced_default.map(Arc::new),
            doc,
            coercer,
        )))
    }

    fn dep<'v>(
        #[starlark(default = Vec::new())] providers: Vec<Value<'v>>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::check_not_relative_label(default, "attrs.dep")?;
        let required_providers = dep_like_attr_handle_providers_arg(providers)?;
        let coercer = AttrType::dep(required_providers);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn any<'v>(
        #[starlark(require = named, default = "")] doc: &str,
        #[starlark(require = named)] default: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::any())
    }

    fn bool<'v>(
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::bool())
    }

    fn option<'v>(
        inner: &AttributeAsStarlarkValue,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::option(inner.coercer_for_inner()?);
        let attr = Attribute::attr(eval, default, doc, coercer)?;

        match attr.default() {
            Some(default) if !default.may_return_none() => {
                Err(AttrError::OptionDefaultNone(default.as_display_no_ctx().to_string()).into())
            }
            _ => Ok(attr),
        }
    }

    fn default_only(
        inner: &AttributeAsStarlarkValue,
        #[starlark(require = named, default = "")] doc: &str,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let Some(default) = inner.default().duped() else {
            return Err(AttrError::DefaultOnlyMustHaveDefault.into());
        };
        Ok(AttributeAsStarlarkValue::new(Attribute::new(
            Some(default),
            doc,
            AttrType::default_only(),
        )))
    }

    fn label<'v>(
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::label())
    }

    fn dict<'v>(
        key: &AttributeAsStarlarkValue,
        value: &AttributeAsStarlarkValue,
        #[starlark(default = false)] sorted: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::dict(key.coercer_for_inner()?, value.coercer_for_inner()?, sorted);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn arg<'v>(
        #[allow(unused_variables)]
        #[starlark(default = false)]
        json: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::arg())
    }

    fn r#enum<'v>(
        #[starlark(require = pos)] variants: Vec<String>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        // Value seems to usually be a `[String]`, listing the possible values of the
        // enumeration. Unfortunately, for things like `exported_lang_preprocessor_flags`
        // it ends up being `Type` which doesn't match the data we see.
        Attribute::attr(eval, default, doc, AttrType::enumeration(variants)?)
    }

    fn configuration_label(
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        // TODO(nga): explain how this is different from `dep`.
        //   This probably meant to be similar to `label`, but not configurable.
        Attribute::attr(eval, None, doc, AttrType::dep(ProviderIdSet::EMPTY))
    }

    fn regex<'v>(
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::string())
    }

    fn set<'v>(
        value_type: &AttributeAsStarlarkValue,
        #[allow(unused_variables)]
        #[starlark(default = false)]
        sorted: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::list(value_type.coercer_for_inner()?);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn named_set<'v>(
        value_type: &AttributeAsStarlarkValue,
        #[starlark(default = false)] sorted: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let value_coercer = value_type.coercer_for_inner()?;
        let coercer = AttrType::one_of(vec![
            AttrType::dict(AttrType::string(), value_coercer.dupe(), sorted),
            AttrType::list(value_coercer),
        ]);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn one_of<'v>(
        #[starlark(args)] args: Vec<&AttributeAsStarlarkValue>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::one_of(args.into_try_map(|arg| arg.coercer_for_inner())?);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn tuple<'v>(
        #[starlark(args)] args: Vec<&AttributeAsStarlarkValue>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::tuple(args.into_try_map(|arg| arg.coercer_for_inner())?);
        Attribute::attr(eval, default, doc, coercer)
    }

    fn int<'v>(
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::int())
    }

    fn query(
        #[starlark(default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, None, doc, AttrType::query())
    }

    fn versioned(
        value_type: &AttributeAsStarlarkValue,
        #[starlark(require = named, default = "")] doc: &str,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        // A versioned field looks like:
        // [ ({"key":"value1"}, arg), ({"key":"value2"}, arg) ]
        let element_type = AttrType::tuple(vec![
            AttrType::dict(AttrType::string(), AttrType::string(), false),
            value_type.coercer_for_inner()?,
        ]);
        let coercer = AttrType::list(element_type.dupe());

        Ok(AttributeAsStarlarkValue::new(Attribute::new(
            Some(Arc::new(AnyAttrType::empty_list())),
            doc,
            coercer,
        )))
    }

    fn source<'v>(
        #[starlark(default = false)] allow_directory: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::check_not_relative_label(default, "attrs.source")?;
        Attribute::attr(eval, default, doc, AttrType::source(allow_directory))
    }
}

pub fn register_attrs(globals: &mut GlobalsBuilder) {
    globals.struct_("attrs", attr_module);
}
