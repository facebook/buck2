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

use allocative::Allocative;
use anyhow::Context as _;
use buck2_common::result::SharedResult;
use buck2_core::pattern::lex_target_pattern;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_interpreter::types::label::StarlarkProvidersLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use derive_more::Display;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::FutureExt;
use gazebo::variants::VariantName;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::float::StarlarkFloat;
use starlark::values::list::AllocList;
use starlark::values::list::ListRef;
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
#[derive(Clone, Debug, Display, ProvidesStaticType, NoSerialize, Allocative)]
#[display(fmt = "{:?}", self)]
pub(crate) struct CliArgs {
    /// The default value. If None, the value is not optional and must be provided by the user
    pub(crate) default: Option<Arc<CliArgValue>>,
    /// Documentation for what the attribute actually means
    doc: String,
    /// The coercer to take this parameter's value from Starlark value -> an
    /// internal representation
    coercer: CliArgType,
    /// The shorthand representation of the CLI arg
    #[allocative(skip)]
    pub(crate) short: Option<char>,
}

starlark_simple_value!(CliArgs);

impl<'v> StarlarkValue<'v> for CliArgs {
    starlark_type!("bxl_cli_args");
}

impl CliArgs {
    /// Helper to create an attribute from attrs.foo functions
    fn new<'v>(
        default: Option<Value<'v>>,
        doc: &str,
        coercer: CliArgType,
        short: Option<Value<'v>>,
    ) -> anyhow::Result<Self> {
        let default = match default {
            None => None,
            Some(x) => Some(Arc::new(
                coercer
                    .coerce_value(x)
                    .map_err(|_| ValueError::IncorrectParameterType)?,
            )),
        };

        let short = match short {
            None => None,
            Some(s) => match s.unpack_str() {
                Some(s) => {
                    if s.len() != 1 {
                        return Err(ValueError::IncorrectParameterType.into());
                    }
                    Some(s.chars().next().unwrap())
                }
                None => return Err(ValueError::IncorrectParameterType.into()),
            },
        };

        Ok(Self {
            default,
            doc: doc.to_owned(),
            coercer,
            short,
        })
    }

    pub fn to_clap<'a>(&'a self, arg: clap::Arg<'a>) -> clap::Arg<'a> {
        let mut arg = self.coercer.to_clap(arg.help(self.doc.as_str()));
        if let Some(short) = self.short {
            arg = arg.short(short);
        }

        if self.default.is_some() {
            arg = arg.required(false);
        }

        arg
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

#[derive(
    Debug,
    derive_more::Display,
    PartialEq,
    Eq,
    Clone,
    Hash,
    Ord,
    PartialOrd,
    Allocative
)]
pub(crate) enum CliArgValue {
    Bool(bool),
    Int(i32),
    // store this as a string here for eq, hash since generally this comes from cmdline.
    // Note that this means `3.0` and `3.00` would not be equal. The string should already have
    // been verified to be a f64.
    Float(String),
    String(String),
    // Type of list elements is used to verify that concatenation is valid.
    // That only can be checked after configuration took place,
    // so pass the type info together with values to be used later.
    #[display(fmt = "{}", "_0.iter().map(|v| v.to_string()).join(\",\")")]
    List(Vec<CliArgValue>),
    None,
    TargetLabel(TargetLabel),
    ProvidersLabel(ProvidersLabel),
}

impl CliArgValue {
    pub(crate) fn as_starlark<'v>(&self, heap: &'v Heap) -> Value<'v> {
        match self {
            CliArgValue::Bool(b) => Value::new_bool(*b),
            CliArgValue::Int(i) => heap.alloc(*i),
            CliArgValue::Float(f) => heap.alloc(f.parse::<f64>().expect("already verified")),
            CliArgValue::String(s) => heap.alloc(s),
            CliArgValue::List(l) => heap.alloc(AllocList(l.iter().map(|v| v.as_starlark(heap)))),
            CliArgValue::None => Value::new_none(),
            CliArgValue::TargetLabel(t) => heap.alloc(StarlarkTargetLabel::new(t.dupe())),
            CliArgValue::ProvidersLabel(p) => heap.alloc(StarlarkProvidersLabel::new(p.clone())),
        }
    }
}

#[derive(Debug, VariantName, Clone, Dupe, Allocative)]
#[cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_arc_on_dupe))] // recursive type
pub(crate) enum CliArgType {
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
    SubTargetExpr,
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

    fn sub_target_expr() -> Self {
        CliArgType::SubTargetExpr
    }

    fn enumeration(vs: HashSet<String>) -> Self {
        CliArgType::Enumeration(Arc::new(vs))
    }
}

#[derive(Debug, Error)]
pub(crate) enum CliArgError {
    #[error("Expected default value of type `{}`, but got `{}`", _0, _1)]
    DefaultValueTypeError(CliArgType, String),
    #[error("Missing cli arg from command line that isn't optional nor has any default values")]
    MissingCliArg,
    #[error("`{0}` is not a {1} label.")]
    NotALabel(String, &'static str),
    #[error("Defaults are not allowed for cli arg type `{0}`.")]
    NoDefaultsAllowed(CliArgType),
    #[error("Duplicate short args are not allowed: `{0}` was already used")]
    DuplicateShort(char),
    #[error("An argument can be kebab-case OR snake-case, not both: `{0}`")]
    DefinedBothKebabAndSnakeCase(String),
}

impl CliArgType {
    fn coerce_value<'v>(&self, value: Value<'v>) -> anyhow::Result<CliArgValue> {
        Ok(match self {
            CliArgType::Bool => CliArgValue::Bool(value.unpack_bool().ok_or_else(|| {
                CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
            })?),
            CliArgType::Int => CliArgValue::Int(value.unpack_i32().ok_or_else(|| {
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
                let list = ListRef::from_value(value).ok_or_else(|| {
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
            CliArgType::SubTargetExpr => {
                return Err(anyhow::anyhow!(CliArgError::NoDefaultsAllowed(
                    CliArgType::SubTargetExpr
                )));
            }
        })
    }

    #[allow(deprecated)] // TODO(nga): fix.
    pub fn to_clap<'a>(&'a self, clap: clap::Arg<'a>) -> clap::Arg<'a> {
        match self {
            CliArgType::Bool => clap.takes_value(true).validator(|x| x.parse::<bool>()),
            CliArgType::Int => clap.takes_value(true).validator(|x| x.parse::<i32>()),
            CliArgType::Float => clap.takes_value(true).validator(|x| x.parse::<f64>()),
            CliArgType::String => clap.takes_value(true),
            CliArgType::Enumeration(variants) => clap
                .takes_value(true)
                .possible_values(variants.iter().map(String::as_str)),
            CliArgType::List(inner) => inner.to_clap(clap.takes_value(true).multiple(true)),
            CliArgType::Option(inner) => inner.to_clap(clap.required(false)),
            CliArgType::TargetLabel => clap.takes_value(true).validator(|x| {
                lex_target_pattern::<TargetPatternExtra>(x, false)
                    .and_then(|parsed| parsed.pattern.infer_target())
                    .and_then(|parsed| {
                        parsed
                            .target()
                            .context(CliArgError::NotALabel(x.to_owned(), "target"))
                            .map(|_| ())
                    })
            }),
            CliArgType::SubTarget => clap.takes_value(true).validator(|x| {
                lex_target_pattern::<ProvidersPatternExtra>(x, false)
                    .and_then(|parsed| parsed.pattern.infer_target())
                    .and_then(|parsed| {
                        parsed
                            .target()
                            .context(CliArgError::NotALabel(x.to_owned(), "target"))
                            .map(|_| ())
                    })
            }),
            CliArgType::TargetExpr => clap.takes_value(true),
            CliArgType::SubTargetExpr => clap.takes_value(true),
        }
    }

    pub fn parse_clap<'a>(
        &'a self,
        clap: ArgAccessor<'a>,
        ctx: &'a CliResolutionCtx<'a>,
    ) -> BoxFuture<'a, anyhow::Result<Option<CliArgValue>>> {
        async move {
            Ok(match self {
                CliArgType::Bool => clap.value_of().map_or(Ok(None), |x| {
                    let r: anyhow::Result<_> = try { CliArgValue::Bool(x.parse::<bool>()?) };
                    r.map(Some)
                })?,
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
                CliArgType::List(inner) => match clap.values_of() {
                    None => None,
                    Some(values) => Some(CliArgValue::List(
                        futures::future::join_all(values.map(async move |v| try {
                            inner
                                .parse_clap(ArgAccessor::Literal(v), ctx)
                                .await?
                                .expect("shouldn't be empty when parsing list items")
                        }))
                        .await
                        .into_iter()
                        .collect::<anyhow::Result<_>>()?,
                    )),
                },
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
                            ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                                &ctx.target_alias_resolver,
                                ctx.relative_dir.as_cell_path(),
                                x,
                                &ctx.cell_resolver,
                            )?
                            .as_target_label(x)?,
                        )
                    };
                    r.map(Some)
                })?,
                CliArgType::SubTarget => clap.value_of().map_or(Ok(None), |x| {
                    let r: anyhow::Result<_> = try {
                        CliArgValue::ProvidersLabel(
                            ParsedPattern::<ProvidersPatternExtra>::parse_relaxed(
                                &ctx.target_alias_resolver,
                                ctx.relative_dir.as_cell_path(),
                                x,
                                &ctx.cell_resolver,
                            )?
                            .as_providers_label(x)?,
                        )
                    };
                    r.map(Some)
                })?,
                CliArgType::TargetExpr => {
                    let x = clap.value_of().unwrap_or("");
                    let pattern = ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                        &ctx.target_alias_resolver,
                        ctx.relative_dir.as_cell_path(),
                        x,
                        &ctx.cell_resolver,
                    )?;
                    let loaded =
                        load_patterns(ctx.dice, vec![pattern], MissingTargetBehavior::Fail).await?;
                    Some(CliArgValue::List(
                        loaded
                            .iter_loaded_targets()
                            .map_ok(|t| CliArgValue::TargetLabel(t.label().dupe()))
                            .collect::<SharedResult<_>>()?,
                    ))
                }
                CliArgType::SubTargetExpr => {
                    let x = clap.value_of().unwrap_or("");
                    let pattern = ParsedPattern::<ProvidersPatternExtra>::parse_relaxed(
                        &ctx.target_alias_resolver,
                        ctx.relative_dir.as_cell_path(),
                        x,
                        &ctx.cell_resolver,
                    )?;
                    let loaded =
                        load_patterns(ctx.dice, vec![pattern], MissingTargetBehavior::Fail).await?;

                    Some(CliArgValue::List(
                        loaded
                            .into_iter()
                            .flat_map(|(pkg, result)| match result {
                                Ok(res) => res
                                    .keys()
                                    .map(|(target, pattern)| {
                                        Ok(CliArgValue::ProvidersLabel(
                                            pattern
                                                .to_owned()
                                                .into_providers_label(pkg.dupe(), target.as_ref()),
                                        ))
                                    })
                                    .collect::<Vec<_>>(),
                                Err(e) => vec![Err(e.dupe())],
                            })
                            .collect::<SharedResult<Vec<_>>>()?,
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
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(default, doc, CliArgType::string(), short)
    }

    fn list<'v>(
        #[starlark(require = pos)] inner: &CliArgs,
        default: Option<Value<'v>>,
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        let coercer = CliArgType::list(inner.coercer.dupe());
        CliArgs::new(default, doc, coercer, short)
    }

    fn bool<'v>(
        #[starlark(default = false)] default: Value<'v>,
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(Some(default), doc, CliArgType::bool(), short)
    }

    fn int<'v>(
        default: Option<Value<'v>>,
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(default, doc, CliArgType::int(), short)
    }

    fn float<'v>(
        default: Option<Value<'v>>,
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(default, doc, CliArgType::float(), short)
    }

    fn option<'v>(
        inner: &CliArgs,
        #[starlark(default = "")] doc: &str,
        #[starlark(default = NoneType)] default: Value<'v>,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        let coercer = CliArgType::option(inner.coercer.dupe());
        CliArgs::new(Some(default), doc, coercer, short)
    }

    fn r#enum<'v>(
        #[starlark(require = pos)] variants: Vec<String>,
        default: Option<Value<'v>>,
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        // Value seems to usually be a `[String]`, listing the possible values of the
        // enumeration. Unfortunately, for things like `exported_lang_preprocessor_flags`
        // it ends up being `Type` which doesn't match the data we see.
        CliArgs::new(
            default,
            doc,
            CliArgType::enumeration(variants.into_iter().collect()),
            short,
        )
    }

    fn target_label<'v>(
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(None, doc, CliArgType::target_label(), short)
    }

    fn sub_target<'v>(
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(None, doc, CliArgType::sub_target(), short)
    }

    fn target_expr<'v>(
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(None, doc, CliArgType::target_expr(), short)
    }

    fn sub_target_expr<'v>(
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(None, doc, CliArgType::sub_target_expr(), short)
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
    use buck2_core::target::label::TargetLabel;
    use buck2_interpreter::types::label::StarlarkProvidersLabel;
    use buck2_interpreter::types::target_label::StarlarkTargetLabel;
    use starlark::values::Heap;
    use starlark::values::Value;

    use crate::bxl::starlark_defs::cli_args::CliArgType;
    use crate::bxl::starlark_defs::cli_args::CliArgValue;

    #[test]
    fn print_cli_arg_list() -> anyhow::Result<()> {
        let args = vec![
            CliArgValue::Bool(true),
            CliArgValue::String("test".to_owned()),
            CliArgValue::Int(1),
        ];

        let cli_arg = CliArgValue::List(args);
        let printed = format!("{}", cli_arg);
        assert_eq!(printed, "true,test,1");

        Ok(())
    }

    #[test]
    fn coerce_starlark() -> anyhow::Result<()> {
        let heap = Heap::new();

        assert_eq!(
            CliArgType::bool().coerce_value(Value::new_bool(true))?,
            CliArgValue::Bool(true)
        );

        assert_eq!(
            CliArgType::int().coerce_value(heap.alloc(42))?,
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
                TargetLabel::testing_parse("root//foo:bar")
            )))?,
            CliArgValue::TargetLabel(TargetLabel::testing_parse("root//foo:bar"))
        );

        assert_eq!(
            CliArgType::sub_target().coerce_value(heap.alloc(StarlarkProvidersLabel::new(
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
