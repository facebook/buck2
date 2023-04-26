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
use anyhow::Context as _;
use buck2_core::provider::label::ProvidersLabel;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::attr_type::any::AnyAttrType;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use buck2_node::provider_id_set::ProviderIdSet;
use derive_more::Display;
use dupe::Dupe;
use dupe::OptionDupedExt;
use gazebo::prelude::*;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::StarlarkDocs;
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
                    .context("Error coercing attribute default")?,
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

/// This type is available as a global `attrs` symbol, to allow the definition of attributes to the `rule` function.
///
/// As an example:
///
/// ```python
/// rule(impl = _impl, attrs = {"foo": attrs.string(), "bar": attrs.int(default = 42)})
/// ```
///
/// Most attributes take at least two optional parameters:
///
/// * A `doc` parameter, which specifies documentation for the attribute.
///
/// * A `default` parameter, which if present specifies the default value for the attribute if omitted.
///   If there is no default, the user of the rule must supply that parameter.
///
/// Each attribute defines what values it accepts from the user, and which values it gives to the rule.
/// For simple types like `attrs.string` these are the same, for more complex types like `attrs.dep` these
/// are different (string from the user, dependency to the rule).
#[starlark_module]
fn attr_module(registry: &mut MethodsBuilder) {
    /// Takes a string from the user, supplies a string to the rule.
    fn string<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[allow(unused_variables)]
        #[starlark(require = named)]
        validate: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::string())
    }

    /// Takes a list from the user, supplies a list to the rule.
    fn list<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(require = pos)] inner: &AttributeAsStarlarkValue,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::list(inner.coercer_for_inner()?);
        Attribute::attr(eval, default, doc, coercer)
    }

    /// Takes a target from the user, as a string, and supplies a dependency to the rule.
    /// The dependency will transition to the execution platform. Use `exec_dep` if you
    /// plan to execute things from this dependency as part of the compilation.
    fn exec_dep<'v>(
        #[starlark(this)] _this: Value<'v>,
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

    /// Takes a target from the user, as a string, and supplies a dependency to the rule.
    /// The dependency will be a toolchain dependency, meaning that its execution platform
    /// dependencies will be used to select the execution platform for this rule.
    fn toolchain_dep<'v>(
        #[starlark(this)] _this: Value<'v>,
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
        #[starlark(this)] _this: Value<'v>,
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
        #[starlark(this)] _this: Value<'v>,
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
        #[starlark(this)] _this: Value<'v>,
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

    /// Takes a target from the user, as a string, and supplies a dependency to the rule.
    /// A target can be specified as an absolute dependency `foo//bar:baz`, omitting the
    /// cell (`//bar:baz`) or omitting the package name (`:baz`).
    ///
    /// If supplied the `providers` argument ensures that specific providers will be present
    /// on the dependency.
    fn dep<'v>(
        #[starlark(this)] _this: Value<'v>,
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

    /// Takes most builtin literals and passes them to the rule as a string.
    /// Discouraged, as it provides little type safety and destroys the structure.
    fn any<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(require = named, default = "")] doc: &str,
        #[starlark(require = named)] default: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::any())
    }

    /// Takes a boolean and passes it to the rule as a boolean.
    fn bool<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::bool())
    }

    /// Takes a value that may be `None` or some inner type, and passes either `None` or the
    /// value corresponding to the inner to the rule. Often used to make a rule optional:
    ///
    /// ```python
    /// attrs.option(attr.string(), default = None)
    /// ```
    fn option<'v>(
        #[starlark(this)] _this: Value<'v>,
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

    /// Rejects all values and uses the default for the inner argument.
    /// Often used to resolve dependencies, which otherwise can't be resolved inside a rule.
    ///
    /// ```python
    /// attrs.default_only(attrs.dep(default = "foo//my_package:my_target"))
    /// ```
    fn default_only<'v>(
        #[starlark(this)] _this: Value<'v>,
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

    /// Takes a target (as per `deps`) and passes a `label` to the rule.
    /// Validates that the target exists, but does not introduce a dependency on it.
    fn label<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::label())
    }

    /// Takes a dict from the user, supplies a dict to the rule.
    fn dict<'v>(
        #[starlark(this)] _this: Value<'v>,
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

    /// Takes a command line argument from the user and supplies a `cmd_args` compatible value to the rule.
    /// The argument may contain special macros such as `$(location :my_target)` or `$(exe :my_target)` which
    /// will be replaced with references to those values in the rule.
    fn arg<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[allow(unused_variables)]
        #[starlark(default = false)]
        json: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::arg())
    }

    /// Takes a string from one of the variants given, and gives that string to the rule.
    /// Strings are matched case-insensitively, and always passed to the rule lowercase.
    fn r#enum<'v>(
        #[starlark(this)] _this: Value<'v>,
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

    fn configuration_label<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        // TODO(nga): explain how this is different from `dep`.
        //   This probably meant to be similar to `label`, but not configurable.
        Attribute::attr(eval, None, doc, AttrType::dep(ProviderIdSet::EMPTY))
    }

    /// Currently an alias for `attrs.string`.
    fn regex<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::string())
    }

    fn set<'v>(
        #[starlark(this)] _this: Value<'v>,
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
        #[starlark(this)] _this: Value<'v>,
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

    /// Given a list of alternative attributes, selects the first that matches and gives that to the rule.
    fn one_of<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(args)] args: Vec<&AttributeAsStarlarkValue>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::one_of(args.into_try_map(|arg| arg.coercer_for_inner())?);
        Attribute::attr(eval, default, doc, coercer)
    }

    /// Takes a tuple of values and gives a tuple to the rule.
    fn tuple<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(args)] args: Vec<&AttributeAsStarlarkValue>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::tuple(args.into_try_map(|arg| arg.coercer_for_inner())?);
        Attribute::attr(eval, default, doc, coercer)
    }

    /// Takes an int from the user, supplies an int to the rule.
    fn int<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, default, doc, AttrType::int())
    }

    fn query<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, None, doc, AttrType::query())
    }

    fn versioned<'v>(
        #[starlark(this)] _this: Value<'v>,
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

    /// Takes a source file from the user, supplies an artifact to the rule.
    /// The source file may be specified as a literal string
    /// (representing the path within this package), or a target (which must have a
    /// `DefaultInfo` with a `default_outputs` value).
    fn source<'v>(
        #[starlark(this)] _this: Value<'v>,
        #[starlark(default = false)] allow_directory: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::check_not_relative_label(default, "attrs.source")?;
        Attribute::attr(eval, default, doc, AttrType::source(allow_directory))
    }
}

#[derive(
    Display,
    Debug,
    StarlarkDocs,
    Allocative,
    ProvidesStaticType,
    NoSerialize
)]
#[display(fmt = "<attrs>")]
struct Attrs;

impl<'v> StarlarkValue<'v> for Attrs {
    starlark_type!("attrs");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(attr_module)
    }
}

pub fn register_attrs(globals: &mut GlobalsBuilder) {
    globals.set("attrs", globals.frozen_heap().alloc_simple(Attrs));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::interpreter::testing::Tester;

    #[test]
    fn test_attr_display() -> anyhow::Result<()> {
        let mut tester = Tester::new().unwrap();
        tester.additional_globals(register_attrs);
        tester.run_starlark_bzl_test(r#"
def assert_eq(a, b):
    if a != b:
        fail(a + " != " + b)

assert_eq(repr(attrs.bool(default = True)), "attrs.bool(default=True)")
assert_eq(repr(attrs.string()), "attrs.string()")
assert_eq(repr(attrs.list(attrs.string())), "attrs.list(attrs.string())")
assert_eq(repr(attrs.dict(attrs.string(), attrs.string())), "attrs.dict(attrs.string(), attrs.string(), sorted=False)")
assert_eq(repr(attrs.one_of(attrs.string())), "attrs.one_of(attrs.string())")
assert_eq(repr(attrs.tuple(attrs.string())), "attrs.tuple(attrs.string())")
assert_eq(repr(attrs.option(attrs.string())), "attrs.option(attrs.string())")

def test(): pass
"#)?;
        Ok(())
    }
}
