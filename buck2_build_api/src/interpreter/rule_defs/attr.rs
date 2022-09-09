/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::provider::id::ProviderId;
use buck2_core::provider::label::ProvidersLabel;
use buck2_interpreter::extra::BuildContext;
use buck2_interpreter_for_build::attrs::attribute_as_starlark_value::AttributeAsStarlarkValue;
use buck2_interpreter_for_build::attrs::coerce::attr_type::AttrTypeExt;
use buck2_interpreter_for_build::attrs::coerce::ctx::BuildAttrCoercionContext;
use buck2_interpreter_for_build::provider::callable::ValueAsProviderCallableLike;
use buck2_interpreter_for_build::transition::transition_id_from_value;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::attr_type::any::AnyAttrType;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use gazebo::prelude::*;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::none::NoneType;
use starlark::values::Value;
use starlark::values::ValueError;
use thiserror::Error;
use tracing::error;

use crate::query::analysis::environment::ConfiguredGraphQueryEnvironment;

const OPTION_NONE_EXPLANATION: &str = "`None` as an attribute value always picks the default. For `attrs.option`, if the default isn't `None`, there is no way to express `None`.";

#[derive(Error, Debug)]
enum AttrError {
    #[error(
        "`attrs.option` `default` parameter must be `None` or absent, got `{0}`.\n{}",
        OPTION_NONE_EXPLANATION
    )]
    OptionDefaultNone(String),
}

pub(crate) trait AttributeExt {
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
                    .map_err(|_| ValueError::IncorrectParameterType)?,
            )),
        };
        Ok(AttributeAsStarlarkValue(Attribute {
            default,
            doc: doc.to_owned(),
            coercer,
        }))
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
pub(crate) fn get_attr_coercion_context<'v>(
    eval: &Evaluator<'v, '_>,
) -> anyhow::Result<BuildAttrCoercionContext> {
    Ok(BuildAttrCoercionContext::new_no_package(
        BuildContext::from_context(eval)?
            .cell_info()
            .cell_alias_resolver()
            .dupe(),
        Arc::new(ConfiguredGraphQueryEnvironment::functions()),
    ))
}

fn attr_any<'v>(doc: &'v str) -> AttributeAsStarlarkValue {
    let coercer = AttrType::any();

    AttributeAsStarlarkValue(Attribute {
        default: Some(Arc::new(AnyAttrType::empty_string())),
        doc: doc.to_owned(),
        coercer,
    })
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
fn dep_like_attr_handle_providers_arg(
    providers: Vec<Value>,
) -> anyhow::Result<Vec<Arc<ProviderId>>> {
    providers.try_map(|v| match v.as_provider_callable() {
        Some(callable) => callable.require_id(),
        None => Err(ValueError::IncorrectParameterTypeNamed("providers".to_owned()).into()),
    })
}

#[starlark_module]
pub(crate) fn attr_module(registry: &mut GlobalsBuilder) {
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
        let coercer = AttrType::list(inner.coercer.dupe());
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

        Ok(AttributeAsStarlarkValue(Attribute {
            default: coerced_default.map(Arc::new),
            doc: doc.to_owned(),
            coercer,
        }))
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

        Ok(AttributeAsStarlarkValue(Attribute {
            default: coerced_default.map(Arc::new),
            doc: doc.to_owned(),
            coercer,
        }))
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

    fn any(
        #[starlark(require = named, default = "")] doc: &str,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Ok(attr_any(doc))
    }

    fn bool<'v>(
        #[starlark(require = named, default = false)] default: Value<'v>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Attribute::attr(eval, Some(default), doc, AttrType::bool())
    }

    fn option<'v>(
        inner: &AttributeAsStarlarkValue,
        #[starlark(require = named, default = NoneType)] default: Value<'v>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::option(inner.coercer.dupe());
        let attr = Attribute::attr(eval, Some(default), doc, coercer)?;

        if attr.default.as_ref().map_or(true, |x| x.may_return_none()) {
            Ok(attr)
        } else {
            Err(AttrError::OptionDefaultNone(default.to_string()).into())
        }
    }

    fn default_only(
        inner: &AttributeAsStarlarkValue,
        #[starlark(require = named, default = "")] doc: &str,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        Ok(AttributeAsStarlarkValue(Attribute {
            default: inner.default.dupe(),
            doc: doc.to_owned(),
            coercer: AttrType::default_only(),
        }))
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
        let coercer = AttrType::dict(key.coercer.dupe(), value.coercer.dupe(), sorted);
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
        Attribute::attr(eval, None, doc, AttrType::dep(Vec::new()))
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
        let coercer = AttrType::list(value_type.coercer.dupe());
        Attribute::attr(eval, default, doc, coercer)
    }

    fn named_set<'v>(
        value_type: &AttributeAsStarlarkValue,
        #[starlark(default = false)] sorted: bool,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let value_coercer = value_type.coercer.dupe();
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
        let coercer = AttrType::one_of(args.into_map(|arg| arg.coercer.dupe()));
        Attribute::attr(eval, default, doc, coercer)
    }

    fn tuple<'v>(
        #[starlark(args)] args: Vec<&AttributeAsStarlarkValue>,
        #[starlark(require = named)] default: Option<Value<'v>>,
        #[starlark(require = named, default = "")] doc: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AttributeAsStarlarkValue> {
        let coercer = AttrType::tuple(args.into_map(|arg| arg.coercer.dupe()));
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
            value_type.coercer.dupe(),
        ]);
        let coercer = AttrType::list(element_type.dupe());

        Ok(AttributeAsStarlarkValue(Attribute {
            default: Some(Arc::new(AnyAttrType::empty_list(element_type))),
            doc: doc.to_owned(),
            coercer,
        }))
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

pub fn register_attr_module(registry: &mut GlobalsBuilder) {
    attr_module(registry)
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_common::package_listing::listing::testing::PackageListingExt;
    use buck2_common::package_listing::listing::PackageListing;
    use buck2_common::result::SharedResult;
    use buck2_core::buck_path::BuckPath;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::Package;
    use buck2_interpreter_for_build::attrs::coerce::attr_type::AttrTypeExt;
    use buck2_interpreter_for_build::attrs::coerce::ctx::BuildAttrCoercionContext;
    use buck2_node::attrs::attr_type::AttrType;
    use buck2_node::attrs::coercion_context::AttrCoercionContext;
    use buck2_node::attrs::configurable::AttrIsConfigurable;
    use gazebo::prelude::*;
    use indoc::indoc;
    use starlark::values::Heap;

    use crate::interpreter::testing::cells;
    use crate::interpreter::testing::run_starlark_bzl_test;
    use crate::interpreter::testing::run_starlark_bzl_test_expecting_error;
    use crate::nodes::hacks::value_to_string;
    use crate::query::analysis::environment::ConfiguredGraphQueryEnvironment;

    #[test]
    fn string_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen = attrs.string(default="something", doc = "foo")
            def test():
                assert_eq('attrs.string(default="something")', repr(attrs.string(default="something", doc = "foo")))
                assert_eq('attrs.string(default="something")', repr(frozen))
            "#
        ))
    }

    #[test]
    fn boolean_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen = attrs.bool()
            def test():
                assert_eq('attrs.bool(default=True)', repr(attrs.bool(default=True, doc = "foo")))
                assert_eq('attrs.bool(default=False)', repr(frozen))
            "#
        ))
    }

    #[test]
    fn test_attr_module_registered() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            def test():
                assert_eq(True, attrs.string != None)
            "#
        ))
    }

    #[test]
    fn list_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen = attrs.list(
                attrs.string(default = "something", doc = "foo"),
                default=["1", "2"],
                doc = "foo",
            )
            def test():
                not_frozen = attrs.list(
                    attrs.string(default = "something", doc = "foo"),
                    default=[],
                    doc = "foo",
                )

                assert_eq('attrs.list(attrs.string(), default=[])', repr(not_frozen))
                assert_eq('attrs.list(attrs.string(), default=["1","2"])', repr(frozen))
            "#
        ))
    }

    #[test]
    fn enum_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen = attrs.enum(["red", "green", "blue"])
            def test():
                not_frozen = attrs.enum(["yes", "no"], default="no")
                assert_eq('attrs.enum(["red","green","blue"])', repr(frozen))
                assert_eq('attrs.enum(["yes","no"], default="no")', repr(not_frozen))
            "#
        ))
    }

    #[test]
    fn attr_coercer_coerces() -> anyhow::Result<()> {
        let heap = Heap::new();
        let some_cells = cells(None)?;
        let cell_alias_resolver = some_cells.0;
        let enclosing_package = (
            Package::new(
                cell_alias_resolver.resolve_self(),
                CellRelativePath::unchecked_new("foo"),
            ),
            PackageListing::testing_empty(),
        );
        let coercer_ctx = BuildAttrCoercionContext::new_with_package(
            cell_alias_resolver,
            enclosing_package,
            false,
            Arc::new(ConfiguredGraphQueryEnvironment::functions()),
        );
        let label_coercer = AttrType::dep(Vec::new());
        let string_coercer = AttrType::string();
        let enum_coercer = AttrType::enumeration(vec![
            "red".to_owned(),
            "green".to_owned(),
            "blue".to_owned(),
        ])?;
        assert!(AttrType::enumeration(vec!["UPPER".to_owned()]).is_err());
        assert!(
            AttrType::enumeration(vec![
                "repeated".to_owned(),
                "and".to_owned(),
                "repeated".to_owned()
            ])
            .is_err()
        );

        let label_value1 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc("root//foo:bar"),
        )?;
        let label_value2 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc("root//foo:bar[baz]"),
        )?;
        let label_value3 =
            label_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc(":bar"))?;
        let label_value4 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc(":bar[baz]"),
        )?;
        let invalid_label_value1 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc("root//foo/..."),
        );
        let invalid_label_value2 = label_coercer.coerce(
            AttrIsConfigurable::Yes,
            &coercer_ctx,
            heap.alloc("root//foo:"),
        );
        let invalid_label_value3 =
            label_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("1"));

        assert_eq!("root//foo:bar", value_to_string(&label_value1)?);
        assert_eq!("root//foo:bar[baz]", value_to_string(&label_value2)?);
        assert_eq!("root//foo:bar", value_to_string(&label_value3)?);
        assert_eq!("root//foo:bar[baz]", value_to_string(&label_value4)?);
        assert!(invalid_label_value1.is_err());
        assert!(invalid_label_value2.is_err());
        assert!(invalid_label_value3.is_err());

        let string_value1 =
            string_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("str"))?;
        assert_eq!("str", value_to_string(&string_value1)?);

        let enum_valid1 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("red"))?;
        let enum_valid2 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("green"))?;
        let enum_valid3 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("RED"))?;
        let enum_invalid1 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc("orange"));
        let enum_invalid2 =
            enum_coercer.coerce(AttrIsConfigurable::Yes, &coercer_ctx, heap.alloc(false));
        assert_eq!("red", value_to_string(&enum_valid1)?);
        assert_eq!("green", value_to_string(&enum_valid2)?);
        assert_eq!("red", value_to_string(&enum_valid3)?);
        assert!(enum_invalid1.is_err());
        assert!(enum_invalid2.is_err());

        Ok(())
    }

    #[test]
    fn dep_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen1 = attrs.dep(default="root//foo:bar")
            frozen2 = attrs.dep(default="//foo:bar")
            def test():
                assert_eq('attrs.dep(default="root//foo:bar")', repr(attrs.dep(default="//foo:bar")))
                assert_eq('attrs.dep(default="root//foo:bar")', repr(frozen1))
                assert_eq('attrs.dep(default="root//foo:bar")', repr(frozen2))
            "#
        ))?;

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                attrs.dep(default="notatarget")
            "#
            ),
            "Type of parameter",
        );

        // Relative targets are disallowed; there is no build file for them to be relative to
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                attrs.dep(default=":reltarget")
            "#
            ),
            "Use a fully qualified",
        );
        Ok(())
    }

    #[test]
    fn source_works() -> SharedResult<()> {
        run_starlark_bzl_test(indoc!(
            r#"
            frozen1 = attrs.source(default="root//foo:bar")
            frozen2 = attrs.source(default="//foo:bar")
            def test():
                assert_eq('attrs.source(default="root//foo:bar")', repr(attrs.source(default="root//foo:bar")))
                assert_eq('attrs.source(default="root//foo:bar")', repr(frozen1))
                assert_eq('attrs.source(default="root//foo:bar")', repr(frozen2))
            "#
        ))?;

        // Relative targets are disallowed; there is no build file for them to be relative to
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                attrs.source(default=":reltarget")
            "#
            ),
            "Use a fully qualified",
        );
        Ok(())
    }

    #[test]
    fn coercing_src_to_path_works() -> anyhow::Result<()> {
        let cell_alias_resolver = cells(None).unwrap().0;
        let package = Package::new(
            cell_alias_resolver.resolve("")?,
            CellRelativePath::unchecked_new("foo/bar"),
        );
        let package_ctx = BuildAttrCoercionContext::new_with_package(
            cell_alias_resolver.dupe(),
            (
                package.dupe(),
                PackageListing::testing_files(&["baz/quz.cpp"]),
            ),
            false,
            Arc::new(ConfiguredGraphQueryEnvironment::functions()),
        );
        let no_package_ctx = BuildAttrCoercionContext::new_no_package(
            cell_alias_resolver,
            Arc::new(ConfiguredGraphQueryEnvironment::functions()),
        );

        let err = no_package_ctx
            .coerce_path("baz/quz.cpp", false)
            .unwrap_err();
        assert!(err.to_string().contains("Expected a package"));

        let err = package_ctx
            .coerce_path("/invalid/absolute/path", false)
            .unwrap_err();
        assert!(format!("{:#}", err).contains("absolute path"), "{:?}", err);

        let err = package_ctx
            .coerce_path("../upward/traversal", false)
            .unwrap_err();
        assert!(err.to_string().contains("normalized path"));

        let expected = BuckPath::new(
            package,
            PackageRelativePathBuf::unchecked_new("baz/quz.cpp".to_owned()),
        );
        assert_eq!(
            &expected,
            package_ctx
                .coerce_path("baz/quz.cpp", false)
                .unwrap()
                .path()
        );
        Ok(())
    }
}
