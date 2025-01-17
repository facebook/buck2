/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::package::PackageLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
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
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::FrozenStringValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;

use crate::bxl::starlark_defs::nodes::unconfigured::attribute::CoercedAttrExt;

type SelectDictKey = Either<StarlarkProvidersLabel, FrozenStringValue>;

#[derive(ProvidesStaticType, Derivative, Trace, Allocative, Clone, Debug)]
pub(crate) struct StarlarkSelectDict {
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
    pub(crate) fn new(selector: CoercedSelector, pkg: PackageLabel) -> Self {
        Self { selector, pkg }
    }

    fn get<'v>(
        &self,
        key: SelectDictKeyArg<'v>,
        heap: &'v Heap,
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
/// One example of this type is
/// ```python
/// select({
//      "root//constraints:a": ["--foo"],
//      "root//constraints:b": ["--bar"],
//      "DEFAULT": ["baz"]
//  })
/// ```
/// You can:
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
        heap: &'v Heap,
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

    // Return the keys of SelectDict. The key is either a string (for `DEFAULT`) or a `ProvidersLabel`.
    //
    // Sample usage:
    // ```python
    // def _impl_select_dict(ctx):
    //     node = ctx.lazy.unconfigured_target_node("root//:select_dict").resolve()
    //     attr = node.get_attr("select_attr")
    //     for key in attr.select_keys():
    //         ctx.output.print(key)
    //```
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
        heap: &'v Heap,
    ) -> starlark::Result<NoneOr<Value<'v>>> {
        Ok(this.get(key, heap)?)
    }
}
