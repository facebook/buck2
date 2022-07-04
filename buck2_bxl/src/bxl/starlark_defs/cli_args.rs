/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Command line arguments definition for bxl functions

use std::collections::HashSet;
use std::fmt::Display;
use std::fmt::Formatter;
use std::sync::Arc;

use anyhow::Context as _;
use async_std_ext::prelude::AsyncOptionExt;
use buck2_build_api::bxl::types::CliArgValue;
use buck2_build_api::calculation::load_patterns;
use buck2_core::pattern::lex_target_pattern;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::pattern::TargetPattern;
use buck2_interpreter::types::label::StarlarkProvidersLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use derive_more::Display;
use futures::future::BoxFuture;
use futures::FutureExt;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use gazebo::variants::VariantName;
use itertools::Itertools;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::float::StarlarkFloat;
use starlark::values::list::List;
use starlark::values::none::NoneType;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::bxl::eval::CliResolutionCtx;

/// Defines the cli args for the bxl function
#[derive(Clone, Debug, Display, ProvidesStaticType, NoSerialize)]
#[display(fmt = "{:?}", self)]
pub(crate) struct CliArgs {
    /// The default value. If None, the value is not optional and must be provided by the user
    pub(crate) default: Option<Arc<CliArgValue>>,
    /// Documentation for what the attribute actually means
    doc: String,
    /// The coercer to take this parameter's value from Starlark value -> an
    /// internal representation
    coercer: CliArgType,
}

starlark_simple_value!(CliArgs);

impl<'v> StarlarkValue<'v> for CliArgs {
    starlark_type!("bxl-cli-args");
}

impl CliArgs {
    /// Helper to create an attribute from attrs.foo functions
    fn new<'v>(default: Option<Value<'v>>, doc: &str, coercer: CliArgType) -> anyhow::Result<Self> {
        let default = match default {
            None => None,
            Some(x) => Some(Arc::new(
                coercer
                    .coerce_value(x)
                    .map_err(|_| ValueError::IncorrectParameterType)?,
            )),
        };
        Ok(Self {
            default,
            doc: doc.to_owned(),
            coercer,
        })
    }

    pub fn to_clap<'a>(&'a self, arg: clap::Arg<'a>) -> clap::Arg<'a> {
        let arg = self.coercer.to_clap(arg.help(self.doc.as_str()));
        if self.default.is_some() {
            arg.required(false)
        } else {
            arg
        }
    }

    pub async fn parse_clap<'a>(
        &self,
        clap: ArgAccessor<'a>,
        ctx: &CliResolutionCtx<'a>,
    ) -> anyhow::Result<CliArgValue> {
        Ok(match self.coercer.parse_clap(clap, ctx).await? {
            None => (**self.default.as_ref().ok_or(CliArgError::MissingCliArg)?).clone(),
            Some(v) => v,
        })
    }
}

pub trait CliArgValueExt {
    fn as_starlark<'v>(&self, heap: &'v Heap) -> Value<'v>;
}

impl CliArgValueExt for CliArgValue {
    fn as_starlark<'v>(&self, heap: &'v Heap) -> Value<'v> {
        match self {
            CliArgValue::Bool(b) => Value::new_bool(*b),
            CliArgValue::Int(i) => Value::new_int(*i),
            CliArgValue::Float(f) => heap.alloc(f.parse::<f64>().expect("already verified")),
            CliArgValue::String(s) => heap.alloc(s),
            CliArgValue::List(l) => heap.alloc_list_iter(l.iter().map(|v| v.as_starlark(heap))),
            CliArgValue::None => Value::new_none(),
            CliArgValue::TargetLabel(t) => heap.alloc(StarlarkTargetLabel::new(t.dupe())),
            CliArgValue::ProvidersLabel(p) => {
                heap.alloc(StarlarkProvidersLabel::new(heap, p.clone()))
            }
        }
    }
}

#[derive(Debug, VariantName, Clone, Dupe)]
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))] // recursive type
enum CliArgType {
    Bool,
    Int,
    Float,
    String,
    Enumeration(Arc<HashSet<String>>),
    List(Arc<CliArgType>),
    Option(Arc<CliArgType>),
    TargetLabel,
    TargetExpr,
    SubTarget,
}

impl Display for CliArgType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CliArgType::Enumeration(t) => {
                write!(
                    f,
                    "{}(variants={})",
                    self.variant_name(),
                    t.iter().join(",")
                )
            }
            CliArgType::List(t) => {
                write!(f, "{}({})", self.variant_name(), t)
            }
            CliArgType::Option(t) => {
                write!(f, "{}({})", self.variant_name(), t)
            }
            x => write!(f, "{}", x.variant_name()),
        }
    }
}

impl CliArgType {
    fn bool() -> Self {
        CliArgType::Bool
    }

    fn int() -> Self {
        CliArgType::Int
    }

    fn float() -> Self {
        CliArgType::Float
    }

    fn string() -> Self {
        CliArgType::String
    }

    fn list(t: CliArgType) -> Self {
        CliArgType::List(Arc::new(t))
    }

    fn option(t: CliArgType) -> Self {
        CliArgType::Option(Arc::new(t))
    }

    fn target_label() -> Self {
        CliArgType::TargetLabel
    }

    fn target_expr() -> Self {
        CliArgType::TargetExpr
    }

    fn sub_target() -> Self {
        CliArgType::SubTarget
    }

    fn enumeration(vs: HashSet<String>) -> Self {
        CliArgType::Enumeration(Arc::new(vs))
    }
}

#[derive(Debug, Error)]
enum CliArgError {
    #[error("Expected default value of type `{}`, but got `{}`", _0, _1)]
    DefaultValueTypeError(CliArgType, String),
    #[error("Missing cli arg from command line that isn't optional nor has any default values")]
    MissingCliArg,
    #[error("`{0}` is not a {1} label.")]
    NotALabel(String, &'static str),
    #[error("Defaults are not allowed for cli arg type `{0}`.")]
    NoDefaultsAllowed(CliArgType),
}

impl CliArgType {
    fn coerce_value<'v>(&self, value: Value<'v>) -> anyhow::Result<CliArgValue> {
        Ok(match self {
            CliArgType::Bool => CliArgValue::Bool(value.unpack_bool().ok_or_else(|| {
                CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
            })?),
            CliArgType::Int => CliArgValue::Int(value.unpack_int().ok_or_else(|| {
                CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
            })?),
            CliArgType::Float => CliArgValue::Float(
                StarlarkFloat::unpack_value(value)
                    .ok_or_else(|| {
                        CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
                    })?
                    .0
                    .to_string(),
            ),
            CliArgType::String => CliArgValue::String(
                value
                    .unpack_str()
                    .ok_or_else(|| {
                        CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
                    })?
                    .to_owned(),
            ),
            CliArgType::Enumeration(vs) => {
                let v = value
                    .unpack_str()
                    .ok_or_else(|| {
                        CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
                    })?
                    .to_owned();
                if vs.contains(&v) {
                    CliArgValue::String(v)
                } else {
                    return Err(anyhow::anyhow!(CliArgError::DefaultValueTypeError(
                        self.dupe(),
                        v
                    )));
                }
            }
            CliArgType::List(inner) => {
                let list = List::from_value(value).ok_or_else(|| {
                    CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
                })?;
                let mut result = vec![];
                for item in list.iter() {
                    result.push(inner.coerce_value(item)?);
                }

                CliArgValue::List(result)
            }
            CliArgType::Option(inner) => {
                if value.is_none() {
                    CliArgValue::None
                } else {
                    inner.coerce_value(value)?
                }
            }
            CliArgType::TargetLabel => {
                let label = value.downcast_ref::<StarlarkTargetLabel>().ok_or_else(|| {
                    CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
                })?;
                CliArgValue::TargetLabel(label.label().dupe())
            }
            CliArgType::SubTarget => {
                let label = value
                    .downcast_ref::<StarlarkProvidersLabel>()
                    .ok_or_else(|| {
                        CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
                    })?;
                CliArgValue::ProvidersLabel((*label.label()).clone())
            }
            CliArgType::TargetExpr => {
                return Err(anyhow::anyhow!(CliArgError::NoDefaultsAllowed(
                    CliArgType::TargetExpr
                )));
            }
        })
    }

    #[allow(deprecated)] // TODO(nga): fix.
    pub fn to_clap<'a>(&'a self, clap: clap::Arg<'a>) -> clap::Arg<'a> {
        match self {
            CliArgType::Bool => clap
                .takes_value(true)
                .required(false)
                .default_missing_value("false")
                .validator(|x| x.parse::<bool>()),
            CliArgType::Int => clap.takes_value(true).validator(|x| x.parse::<i32>()),
            CliArgType::Float => clap.takes_value(true).validator(|x| x.parse::<f64>()),
            CliArgType::String => clap.takes_value(true),
            CliArgType::Enumeration(variants) => clap
                .takes_value(true)
                .possible_values(variants.iter().map(String::as_str)),
            CliArgType::List(inner) => inner.to_clap(clap.takes_value(true).multiple_values(true)),
            CliArgType::Option(inner) => inner.to_clap(clap.required(false)),
            CliArgType::TargetLabel => clap.takes_value(true).validator(|x| {
                lex_target_pattern::<TargetPattern>(x, false)
                    .and_then(|parsed| parsed.pattern.infer_target())
                    .and_then(|parsed| {
                        parsed
                            .target()
                            .context(CliArgError::NotALabel(x.to_owned(), "target"))
                            .map(|_| ())
                    })
            }),
            CliArgType::SubTarget => clap.takes_value(true).validator(|x| {
                lex_target_pattern::<ProvidersPattern>(x, false)
                    .and_then(|parsed| parsed.pattern.infer_target())
                    .and_then(|parsed| {
                        parsed
                            .target()
                            .context(CliArgError::NotALabel(x.to_owned(), "target"))
                            .map(|_| ())
                    })
            }),
            CliArgType::TargetExpr => clap.takes_value(true),
        }
    }

    pub fn parse_clap<'a>(
        &'a self,
        clap: ArgAccessor<'a>,
        ctx: &'a CliResolutionCtx<'a>,
    ) -> BoxFuture<'a, anyhow::Result<Option<CliArgValue>>> {
        async move {
            Ok(match self {
                CliArgType::Bool => Some(CliArgValue::Bool(
                    clap.value_of().unwrap_or("false").parse::<bool>()?,
                )),
                CliArgType::Int => clap.value_of().map_or(Ok(None), |x| {
                    let r: anyhow::Result<_> = try { CliArgValue::Int(x.parse::<i32>()?) };
                    r.map(Some)
                })?,
                CliArgType::Float => clap.value_of().map_or(Ok(None), |x| {
                    let r: anyhow::Result<_> = try {
                        CliArgValue::Float({
                            x.parse::<f64>()?;
                            x.to_owned()
                        })
                    };
                    r.map(Some)
                })?,
                CliArgType::String => clap.value_of().map(|s| CliArgValue::String(s.to_owned())),
                CliArgType::Enumeration(_variants) => {
                    // the validators should have already made sure the strings are only of the variants
                    clap.value_of().map(|s| CliArgValue::String(s.to_owned()))
                }
                CliArgType::List(inner) => {
                    let r: anyhow::Result<_> = clap
                        .values_of()
                        .async_map_or(Ok(None), async move |values| {
                            futures::future::join_all(values.map(async move |v| try {
                                inner
                                    .parse_clap(ArgAccessor::Literal(v), ctx)
                                    .await?
                                    .expect("shouldn't be empty when parsing list items")
                            }))
                            .await
                            .into_iter()
                            .collect::<Result<_, anyhow::Error>>()
                            .map(Some)
                        })
                        .await;
                    r?.map(CliArgValue::List)
                }
                CliArgType::Option(inner) => Some(if clap.value_of().is_some() {
                    inner
                        .parse_clap(clap, ctx)
                        .await?
                        .expect("arg is verified to be present")
                } else {
                    CliArgValue::None
                }),
                CliArgType::TargetLabel => clap.value_of().map_or(Ok(None), |x| {
                    let r: anyhow::Result<_> = try {
                        CliArgValue::TargetLabel(
                            ParsedPattern::<TargetPattern>::parse_relaxed(
                                &ctx.target_alias_resolver,
                                &ctx.cell_resolver,
                                &ctx.relative_dir,
                                x,
                            )?
                            .as_target_label(x)?,
                        )
                    };
                    r.map(Some)
                })?,
                CliArgType::SubTarget => clap.value_of().map_or(Ok(None), |x| {
                    let r: anyhow::Result<_> = try {
                        CliArgValue::ProvidersLabel(
                            ParsedPattern::<ProvidersPattern>::parse_relaxed(
                                &ctx.target_alias_resolver,
                                &ctx.cell_resolver,
                                &ctx.relative_dir,
                                x,
                            )?
                            .as_providers_label(x)?,
                        )
                    };
                    r.map(Some)
                })?,
                CliArgType::TargetExpr => {
                    let x = clap.value_of().unwrap_or("");
                    let pattern = ParsedPattern::<TargetPattern>::parse_relaxed(
                        &ctx.target_alias_resolver,
                        &ctx.cell_resolver,
                        &ctx.relative_dir,
                        x,
                    )?;

                    let loaded = load_patterns(ctx.dice, vec![pattern]).await?;
                    Some(CliArgValue::List(
                        loaded
                            .iter_loaded_targets()
                            .map(|t| CliArgValue::TargetLabel(t.label().dupe()))
                            .collect(),
                    ))
                }
            })
        }
        .boxed()
    }
}

#[starlark_module]
pub(crate) fn cli_args_module(registry: &mut GlobalsBuilder) {
    fn string<'v>(
        default: Option<Value<'v>>,
        #[starlark(default = "")] doc: &str,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(default, doc, CliArgType::string())
    }

    fn list<'v>(
        #[starlark(require = pos)] inner: &CliArgs,
        default: Option<Value<'v>>,
        #[starlark(default = "")] doc: &str,
    ) -> anyhow::Result<CliArgs> {
        let coercer = CliArgType::list(inner.coercer.dupe());
        CliArgs::new(default, doc, coercer)
    }

    fn bool<'v>(
        #[starlark(default = false)] default: Value<'v>,
        #[starlark(default = "")] doc: &str,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(Some(default), doc, CliArgType::bool())
    }

    fn int<'v>(
        default: Option<Value<'v>>,
        #[starlark(default = "")] doc: &str,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(default, doc, CliArgType::int())
    }

    fn float<'v>(
        default: Option<Value<'v>>,
        #[starlark(default = "")] doc: &str,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(default, doc, CliArgType::float())
    }

    fn option<'v>(
        inner: &CliArgs,
        #[starlark(default = "")] doc: &str,
        #[starlark(default = NoneType)] default: Value<'v>,
    ) -> anyhow::Result<CliArgs> {
        let coercer = CliArgType::option(inner.coercer.dupe());
        CliArgs::new(Some(default), doc, coercer)
    }

    fn r#enum<'v>(
        #[starlark(require = pos)] variants: Vec<String>,
        default: Option<Value<'v>>,
        #[starlark(default = "")] doc: &str,
    ) -> anyhow::Result<CliArgs> {
        // Value seems to usually be a `[String]`, listing the possible values of the
        // enumeration. Unfortunately, for things like `exported_lang_preprocessor_flags`
        // it ends up being `Type` which doesn't match the data we see.
        CliArgs::new(
            default,
            doc,
            CliArgType::enumeration(variants.into_iter().collect()),
        )
    }

    fn target_label(#[starlark(default = "")] doc: &str) -> anyhow::Result<CliArgs> {
        CliArgs::new(None, doc, CliArgType::target_label())
    }

    fn sub_target(#[starlark(default = "")] doc: &str) -> anyhow::Result<CliArgs> {
        CliArgs::new(None, doc, CliArgType::sub_target())
    }

    fn target_expr(#[starlark(default = "")] doc: &str) -> anyhow::Result<CliArgs> {
        CliArgs::new(None, doc, CliArgType::target_expr())
    }
}

pub fn register_cli_args_module(registry: &mut GlobalsBuilder) {
    cli_args_module(registry)
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use buck2_core::provider::label::testing::ProvidersLabelTestExt;
    use buck2_core::provider::label::ProvidersLabel;
    use buck2_core::target::testing::TargetLabelExt;
    use buck2_core::target::TargetLabel;
    use buck2_interpreter::types::label::StarlarkProvidersLabel;
    use buck2_interpreter::types::target_label::StarlarkTargetLabel;
    use starlark::values::Heap;
    use starlark::values::Value;

    use crate::bxl::starlark_defs::cli_args::CliArgType;
    use crate::bxl::starlark_defs::cli_args::CliArgValue;

    #[test]
    fn coerce_starlark() -> anyhow::Result<()> {
        let heap = Heap::new();

        assert_eq!(
            CliArgType::bool().coerce_value(Value::new_bool(true))?,
            CliArgValue::Bool(true)
        );

        assert_eq!(
            CliArgType::int().coerce_value(Value::new_int(42))?,
            CliArgValue::Int(42)
        );

        assert_eq!(
            CliArgType::float().coerce_value(heap.alloc(4.2))?,
            CliArgValue::Float("4.2".to_owned())
        );

        assert_eq!(
            CliArgType::string().coerce_value(heap.alloc("foobar"))?,
            CliArgValue::String("foobar".to_owned())
        );

        assert_eq!(
            CliArgType::enumeration(HashSet::from_iter([
                "a".to_owned(),
                "b".to_owned(),
                "c".to_owned()
            ]))
            .coerce_value(heap.alloc("a"))?,
            CliArgValue::String("a".to_owned())
        );

        assert_eq!(
            CliArgType::option(CliArgType::int()).coerce_value(Value::new_none())?,
            CliArgValue::None
        );

        assert_eq!(
            CliArgType::option(CliArgType::bool()).coerce_value(Value::new_bool(true))?,
            CliArgValue::Bool(true)
        );

        assert_eq!(
            CliArgType::bool().coerce_value(Value::new_bool(false))?,
            CliArgValue::Bool(false)
        );

        assert_eq!(
            CliArgType::list(CliArgType::int()).coerce_value(heap.alloc(vec![1, 4, 2]))?,
            CliArgValue::List(vec![
                CliArgValue::Int(1),
                CliArgValue::Int(4),
                CliArgValue::Int(2)
            ])
        );

        assert_eq!(
            CliArgType::target_label().coerce_value(heap.alloc(StarlarkTargetLabel::new(
                TargetLabel::testing_parse("//foo:bar")
            )))?,
            CliArgValue::TargetLabel(TargetLabel::testing_parse("//foo:bar"))
        );

        assert_eq!(
            CliArgType::sub_target().coerce_value(heap.alloc(StarlarkProvidersLabel::new(
                &heap,
                ProvidersLabel::testing_new("foo", "pkg", "bar", Some(&["a", "b"]))
            )))?,
            CliArgValue::ProvidersLabel(ProvidersLabel::testing_new(
                "foo",
                "pkg",
                "bar",
                Some(&["a", "b"])
            ))
        );

        Ok(())
    }
}

pub(crate) enum ArgAccessor<'a> {
    Clap {
        clap: &'a clap::ArgMatches,
        arg: &'a str,
    },
    Literal(&'a str),
}

#[allow(deprecated)] // TODO(nga): fix.
impl<'a> ArgAccessor<'a> {
    fn value_of(&self) -> Option<&str> {
        match self {
            ArgAccessor::Clap { clap, arg } => clap.value_of(arg),
            ArgAccessor::Literal(s) => Some(s),
        }
    }

    fn values_of(&self) -> Option<impl Iterator<Item = &str>> {
        match self {
            ArgAccessor::Clap { clap, arg } => clap.values_of(arg).map(itertools::Either::Left),
            ArgAccessor::Literal(s) => Some(itertools::Either::Right(std::iter::once(*s))),
        }
    }
}
