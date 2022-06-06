/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use buck2_build_api_derive::internal_provider;
use either::Either;
use gazebo::{any::ProvidesStaticType, coerce::Coerce};
use starlark::{
    collections::SmallMap,
    environment::GlobalsBuilder,
    values::{
        dict::{Dict, FrozenDict},
        Freeze, FrozenRef, FrozenValue, Trace, Value,
    },
};
use thiserror::Error;

use crate::interpreter::rule_defs::{
    cmd_args::{FrozenCommandLineArgLike, ValueAsCommandLineLike, ValueAsFrozenCommandLineLike},
    provider::FrozenProviderCollectionValue,
};

#[derive(Debug, Error)]
enum TemplatePlaceholderInfoError {
    #[error(
        "Expected TemplatePlaceholderInfo.{field_key} to be a dict of String->arg-like Value, got `{value_repr}`."
    )]
    VariablesNotADict {
        field_key: String,
        value_repr: String,
    },
    #[error(
        "Expected TemplatePlaceholderInfo.{field_key} to be a dict of String->Value, where each Value is ArgLike or is itself a dict of String->ArgLike but {key} isn't either of those."
    )]
    ValueNotValidKeyedPlaceholder { field_key: String, key: String },
    #[error(
        "Expected TemplatePlaceholderInfo.{field_key}[{key}] to be a dict of String->Arglike but {inner_key} isn't an ArgLike Value."
    )]
    InnerValueNotCommandLineLike {
        field_key: String,
        key: String,
        inner_key: String,
    },
}
/// A provider that is used for expansions in string attribute templates
///
/// String attribute templates allow two types of user-defined placeholders, "unkeyed placeholders"
/// like `$(CXX)` or `$(aapt)` and "keyed placeholders"  that include a target key like
/// `$(cxxppflags //some:target)`. The expansion of each of these types is based on the
/// `TemplatePlaceholderInfo` providers.
///
/// "keyed placeholders" are used for the form `$(<key> <target>)` or `$(<key> <target> <arg>)`. In both cases
/// the lookup will expect a `TemplatePlaceholderInfo` in the providers of `<target>`. It will then lookup
/// `<key>` in the keyed_variables (call this the `value`). There are then four valid possibilities:
///  1. no-arg placeholder, an arg-like `value`: resolve to `value`
///  2. no-arg placeholder, a dictionary `value`: resolve to `value["DEFAULT"]`
///  3. arg placeholder, a non-dictionary `value`: this is an error
///  4. arg placeholder, a dictionary `value`: resolve to `value[<arg>]`
///
/// "unkeyed placeholders" are resolved by matching to any of the deps of the target. `$(CXX)` will resolve
/// to the "CXX" value in any dep's `TemplateProviderInfo.unkeyed_variables`
///
/// Fields:
///  - unkeyed_variables: A mapping of names to arg-like values. These are used for "unkeyed placeholder" expansion.
///  - keyed_variables: A mapping of names to arg-like values or dictionary of string to
///        arg-like values. These are used for "keyed placeholder" expansion.
#[internal_provider(template_placeholder_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType)]
#[repr(C)]
pub(crate) struct TemplatePlaceholderInfoGen<V> {
    // Dict[String, CommandLineArg]
    unkeyed_variables: V,
    // Dict[String, Either<CommandLineArg, Dict[String, CommandLineArg]>]
    keyed_variables: V,
}

impl FrozenTemplatePlaceholderInfo {
    pub fn unkeyed_variables(
        &self,
    ) -> SmallMap<FrozenRef<'static, str>, FrozenRef<'static, dyn FrozenCommandLineArgLike>> {
        FrozenDict::from_frozen_value(&self.unkeyed_variables)
            .expect("should be a dict-like object")
            .iter()
            .map(|(k, v)| {
                (
                    k.downcast_frozen_str().expect("should have string keys"),
                    v.as_frozen_command_line().expect("should be command line"),
                )
            })
            .collect()
    }

    pub fn _unkeyed_variables_raw(&self) -> &FrozenDict {
        FrozenDict::from_frozen_value(&self.unkeyed_variables)
            .expect("Type should have been checked on construction.")
    }

    pub fn keyed_variables(
        &self,
    ) -> SmallMap<
        FrozenRef<'static, str>,
        Either<
            FrozenRef<'static, dyn FrozenCommandLineArgLike>,
            SmallMap<FrozenRef<'static, str>, FrozenRef<'static, dyn FrozenCommandLineArgLike>>,
        >,
    > {
        FrozenDict::from_frozen_value(&self.keyed_variables)
            .expect("should be a dict-like object")
            .iter()
            .map(|(k, v)| {
                (k.downcast_frozen_str().expect("should have string keys"), {
                    let either = if let Some(dict) = FrozenDict::from_frozen_value(&v) {
                        Either::Right(
                            dict.iter()
                                .map(|(k, v)| {
                                    (
                                        k.downcast_frozen_str().expect("should have string keys"),
                                        v.as_frozen_command_line().expect("should be command line"),
                                    )
                                })
                                .collect(),
                        )
                    } else if let Some(cmd) = v.as_frozen_command_line() {
                        Either::Left(cmd)
                    } else {
                        unreachable!("should be dict or command line")
                    };

                    either
                })
            })
            .collect()
    }

    pub fn _keyed_variables_raw(&self) -> &FrozenDict {
        FrozenDict::from_frozen_value(&self.keyed_variables)
            .expect("Type should have been checked on construction.")
    }

    pub fn _lookup_provider_raw(providers: &FrozenProviderCollectionValue) -> Option<&FrozenValue> {
        let provider_id = TemplatePlaceholderInfoCallable::provider_id();
        providers
            .provider_collection()
            .get_provider_raw(provider_id)
    }
}

fn verify_variables_type(field_key: &str, variables: Value) -> anyhow::Result<()> {
    match Dict::from_value(variables) {
        None => Err(TemplatePlaceholderInfoError::VariablesNotADict {
            field_key: field_key.to_owned(),
            value_repr: variables.to_repr(),
        }
        .into()),
        Some(dict) => {
            for (key, value) in dict.iter() {
                if value.as_command_line().is_some() {
                    continue;
                }

                if let Some(dict) = Dict::from_value(value) {
                    for (inner_key, value) in dict.iter() {
                        if value.as_command_line().is_none() {
                            return Err(
                                TemplatePlaceholderInfoError::InnerValueNotCommandLineLike {
                                    field_key: field_key.to_owned(),
                                    key: key.to_string(),
                                    inner_key: inner_key.to_string(),
                                }
                                .into(),
                            );
                        }
                    }
                    continue;
                }

                return Err(
                    TemplatePlaceholderInfoError::ValueNotValidKeyedPlaceholder {
                        field_key: field_key.to_owned(),
                        key: key.to_string(),
                    }
                    .into(),
                );
            }

            Ok(())
        }
    }
}

impl<'v> TemplatePlaceholderInfo<'v> {
    fn new(unkeyed_variables: Value<'v>, keyed_variables: Value<'v>) -> anyhow::Result<Self> {
        verify_variables_type("unkeyed_variables", unkeyed_variables)?;
        verify_variables_type("keyed_variables", keyed_variables)?;
        Ok(Self {
            unkeyed_variables,
            keyed_variables,
        })
    }
}

#[starlark_module]
fn template_placeholder_info_creator(builder: &mut GlobalsBuilder) {
    #[starlark(type = "TemplatePlaceholderInfo")]
    fn TemplatePlaceholderInfo<'v>(
        #[starlark(default = FrozenDict::default())] unkeyed_variables: Value<'v>,
        #[starlark(default = FrozenDict::default())] keyed_variables: Value<'v>,
    ) -> anyhow::Result<TemplatePlaceholderInfo<'v>> {
        TemplatePlaceholderInfo::new(unkeyed_variables, keyed_variables)
    }
}
