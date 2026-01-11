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
use buck2_core::package::PackageLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_node::attrs::coerced_attr::CoercedConcat;
use buck2_node::attrs::coerced_attr::CoercedSelector;
use buck2_node::attrs::coerced_attr::CoercedSelectorKeyRef;
use buck2_node::attrs::display::AttrDisplayWithContext;
use buck2_node::attrs::fmt_context::AttrFmtContext;
use buck2_node::attrs::serialize::AttrSerializeWithContext;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use either::Either;
use serde::Serialize;
use starlark::__derive_refs::serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::const_frozen_string;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::FrozenStringValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;

use crate::bxl::unconfigured_attribute::CoercedAttrExt;

type SelectDictKey = Either<StarlarkProvidersLabel, FrozenStringValue>;

#[derive(ProvidesStaticType, Derivative, Trace, Allocative, Clone, Debug)]
pub struct StarlarkSelectDict {
    selector: CoercedSelector,
    pkg: PackageLabel,
}

impl Display for StarlarkSelectDict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ctx = AttrFmtContext {
            package: Some(self.pkg),
            options: Default::default(),
        };
        self.selector.fmt(&ctx, f)
    }
}

impl Serialize for StarlarkSelectDict {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let ctx = AttrFmtContext {
            package: Some(self.pkg),
            options: Default::default(),
        };
        self.selector.serialize_with_ctx(&ctx, serializer)
    }
}

#[derive(StarlarkTypeRepr, UnpackValue, derive_more::Display, Clone, Dupe)]
enum SelectDictKeyArg<'v> {
    #[display("{}", _0)]
    Label(&'v StarlarkProvidersLabel),
    #[display("{}", _0)]
    Str(&'v str),
}

impl StarlarkSelectDict {
    pub fn new(selector: CoercedSelector, pkg: PackageLabel) -> Self {
        Self { selector, pkg }
    }

    fn get<'v>(
        &self,
        key: SelectDictKeyArg<'v>,
        heap: Heap<'v>,
    ) -> buck2_error::Result<NoneOr<Value<'v>>> {
        match key {
            SelectDictKeyArg::Label(label) => {
                let key = ConfigurationSettingKey(label.label().dupe());
                let key_ref = CoercedSelectorKeyRef::Target(&key);
                self.selector
                    .all_entries()
                    .find(|(k, _)| *k == key_ref)
                    .map_or_else(
                        || Ok(NoneOr::None),
                        |(_, v)| v.to_value(self.pkg, heap).map(NoneOr::Other),
                    )
            }
            SelectDictKeyArg::Str(key) => self
                .selector
                .all_entries()
                .find(|(k, _)| k.to_string() == key)
                .map_or_else(
                    || Ok(NoneOr::None),
                    |(_, v)| v.to_value(self.pkg, heap).map(NoneOr::Other),
                ),
        }
    }
}

starlark_simple_value!(StarlarkSelectDict);

#[starlark_value(type = "bxl.SelectDict")]
impl<'v> StarlarkValue<'v> for StarlarkSelectDict {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(select_dict_methods)
    }
}

#[inline(always)]
fn key_to_starlark_type(key: &CoercedSelectorKeyRef) -> SelectDictKey {
    match key {
        CoercedSelectorKeyRef::Target(configuration_setting_key) => Either::Left(
            StarlarkProvidersLabel::new(configuration_setting_key.0.dupe()),
        ),
        CoercedSelectorKeyRef::Default => {
            Either::Right(const_frozen_string!(CoercedSelectorKeyRef::DEFAULT_KEY_STR))
        }
    }
}

/// In bxl, `Select = bxl.SelectDict | bxl.SelectConcat`. `bxl.SelectDict` is a dict-like object that represents a select.
/// One example of this type is:
///
/// ```python
/// select({
///      "root//constraints:a": ["--foo"],
///      "root//constraints:b": ["--bar"],
///      "DEFAULT": ["baz"]
///  })
/// ```
///
/// You can:
///
/// * Iterate over its keys (e.g., `for key in select_dict.select_keys():`).
/// * Iterate over key-value pairs using select_dict.select_items() (e.g., `for key, value in select_dict.select_items():`).
/// * Get the select entry with a string or a ProvidersLabel (e.g., `select_dict.get_select_entry("root//constraints:a")`).
/// * Check its type using `isinstance(select_dict, bxl.SelectDict)``.
#[starlark_module]
fn select_dict_methods(builder: &mut MethodsBuilder) {
    /// Return the key-value pairs of the select. The key is either a string (for `DEFAULT`) or a `ProvidersLabel`.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_select_dict(ctx):
    ///     node = ctx.lazy.unconfigured_target_node("root//:select_dict").resolve()
    ///     attr = node.get_attr("select_attr")
    ///     for key, value in attr.select_items():
    ///         ctx.output.print(f"{key} -> {value}")
    /// ```
    fn select_items<'v>(
        this: &'v StarlarkSelectDict,
        heap: Heap<'v>,
    ) -> starlark::Result<Vec<(SelectDictKey, Value<'v>)>> {
        let items: Vec<(SelectDictKey, Value)> = this
            .selector
            .all_entries()
            .map(|(k, v)| {
                v.to_value(this.pkg.dupe(), heap)
                    .map(|v| (key_to_starlark_type(&k), v))
            })
            .collect::<buck2_error::Result<_>>()?;
        Ok(items)
    }

    /// Return the keys of SelectDict. The key is either a string (for `DEFAULT`) or a `ProvidersLabel`.
    ///
    /// Sample usage:
    ///
    /// ```python
    /// def _impl_select_dict(ctx):
    ///     node = ctx.lazy.unconfigured_target_node("root//:select_dict").resolve()
    ///     attr = node.get_attr("select_attr")
    ///     for key in attr.select_keys():
    ///         ctx.output.print(key)
    /// ```
    fn select_keys<'v>(this: &'v StarlarkSelectDict) -> starlark::Result<Vec<SelectDictKey>> {
        let keys = this
            .selector
            .all_entries()
            .map(|(k, _)| key_to_starlark_type(&k))
            .collect();
        Ok(keys)
    }

    /// Return the entry of the select for the given key. It accepts either a string or a `ProvidersLabel`.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_select_dict(ctx):
    ///    node = ctx.lazy.unconfigured_target_node("root//:select_dict").resolve()
    ///    attr = node.get_attr("select_attr")
    ///    ctx.output.print(attr.get_select_entry("root//constraints:a"))
    ///    ctx.output.print(attr.get_select_entry("DEFAULT"))
    ///    # provider_label's type here is `ProvidersLabel`
    ///    ctx.output.print(attr.get_select_entry(provider_label))
    /// ```
    fn get_select_entry<'v>(
        this: &'v StarlarkSelectDict,
        #[starlark(require = pos)] key: SelectDictKeyArg<'v>,
        heap: Heap<'v>,
    ) -> starlark::Result<NoneOr<Value<'v>>> {
        Ok(this.get(key, heap)?)
    }
}

#[derive(ProvidesStaticType, Derivative, Trace, Allocative, Clone, Debug)]
pub struct StarlarkSelectConcat {
    concat: CoercedConcat,
    pkg: PackageLabel,
}

impl StarlarkSelectConcat {
    pub fn new(concat: CoercedConcat, pkg: PackageLabel) -> Self {
        Self { concat, pkg }
    }
}

impl Serialize for StarlarkSelectConcat {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let ctx = AttrFmtContext {
            package: Some(self.pkg),
            options: Default::default(),
        };
        self.concat.serialize_with_ctx(&ctx, serializer)
    }
}

impl Display for StarlarkSelectConcat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ctx = AttrFmtContext {
            package: Some(self.pkg),
            options: Default::default(),
        };
        self.concat.fmt(&ctx, f)
    }
}

starlark_simple_value!(StarlarkSelectConcat);

#[starlark_value(type = "bxl.SelectConcat")]
impl<'v> StarlarkValue<'v> for StarlarkSelectConcat {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(select_concat_methods)
    }

    fn length(&self) -> starlark::Result<i32> {
        Ok(self.concat.0.len() as i32)
    }
}

/// In bxl, `Select = bxl.SelectDict | bxl.SelectConcat`. `bxl.SelectConcat` is a list-like object that represents a select.
/// One example of this type is:
///
/// ```python
/// ["--flags"] + select({
///     "root//constraints:a": ["--foo"],
///     "root//constraints:b": ["--bar"],
///     "DEFAULT": ["baz"]
/// })
/// ```
///
/// You can:
///
/// * Iterate over the values of this object (e.g. `for item in select_concat.select_iter():`)
/// * Get the length (e.g. `len(select_concat)`)
/// * Check its type using `isinstance(select_concat, bxl.SelectConcat)`.
///
/// Sample usage:
/// ```python
/// def _impl_select_concat(ctx):
///     node = ctx.lazy.unconfigured_target_node("root//:select_concat").resolve()
///     attr = node.get_attr("select_attr")
///     for value in attr:
///         if isinstance(value, bxl.SelectDict):
///             for key, value in value.items():
///                 ctx.output.print(f"{key} -> {value}")
///         else:
///             ctx.output.print(value)
///     ctx.output.print(attr[0])
/// ```
#[starlark_module]
fn select_concat_methods(builder: &mut MethodsBuilder) {
    /// Return the values of the SelectConcat.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_select_concat(ctx):
    ///     node = ctx.lazy.unconfigured_target_node("root//:select_concat").resolve()
    ///     attr = node.get_attr("select_attr")
    ///     for value in attr.select_iter():
    ///         ctx.output.print(value)
    /// ```
    fn select_iter<'v>(
        this: &'v StarlarkSelectConcat,
        heap: Heap<'v>,
    ) -> starlark::Result<Vec<Value<'v>>> {
        let list = this
            .concat
            .iter()
            .map(|a| a.to_value(this.pkg.dupe(), heap))
            .collect::<buck2_error::Result<_>>()?;
        Ok(list)
    }

    /// Returns the length of a SelectConcat, defined as the number of items being concatenated
    /// at the select level (not the total number of elements across all lists).
    ///
    /// For example, `[1, 2] + select({"DEFAULT": [3, 4]}` returns 2 instead of 4.
    /// Note: You can use `len()` to get the length too.
    #[starlark(attribute)]
    fn length(this: &StarlarkSelectConcat) -> starlark::Result<i32> {
        Ok(this.concat.0.len() as i32)
    }
}
