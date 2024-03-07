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
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context as _;
use buck2_core::pattern::lex_target_pattern;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use derive_more::Display;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::FutureExt;
use gazebo::variants::VariantName;
use itertools::Itertools;
use num_bigint::BigInt;
use serde::Serialize;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::dict::Dict;
use starlark::values::float::StarlarkFloat;
use starlark::values::list::AllocList;
use starlark::values::list::ListRef;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::none::NoneType;
use starlark::values::starlark_value;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use starlark_map::ordered_map::OrderedMap;
use starlark_map::small_map::SmallMap;

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

#[starlark_value(type = "bxl_cli_args")]
impl<'v> StarlarkValue<'v> for CliArgs {}

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

    pub(crate) fn to_clap<'a>(&'a self, arg: clap::Arg<'a>) -> clap::Arg<'a> {
        let mut arg = self.coercer.to_clap(arg.help(self.doc.as_str()));
        if let Some(short) = self.short {
            arg = arg.short(short);
        }

        if self.default.is_some() {
            arg = arg.required(false);
        }

        arg
    }

    pub(crate) async fn parse_clap<'a>(
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

// Wrapper around `serde_json::Value`s, making sure that we keep the values ordered when working with
// `serde_json::Object`s so that hashing is deterministic for BXL keys.
#[derive(
    derive_more::Display,
    Debug,
    Clone,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd,
    Allocative,
    Serialize
)]
pub(crate) enum JsonCliArgValueData {
    None,
    Bool(bool),
    // Preserve BigInts and floats as strings for eq/hash for BXL key
    Float(String),
    Int(String),
    String(String),
    #[display(fmt = "{}", "_0.iter().map(|v| v.to_string()).join(\",\")")]
    List(Vec<JsonCliArgValueData>),
    #[display(
        fmt = "{}",
        "_0.iter().map(|(k, v)| format!(\"(k={},v={})\", k, v.to_string())).join(\",\")"
    )]
    Object(OrderedMap<String, JsonCliArgValueData>),
}

impl JsonCliArgValueData {
    fn from_serde_value(val: &serde_json::Value) -> Self {
        match val {
            serde_json::Value::Null => JsonCliArgValueData::None,
            serde_json::Value::Bool(x) => JsonCliArgValueData::Bool(*x),
            // serde_json::Number can be a u64, i64, or f64. Let's try i64 first, and if that doesn't
            // work then we will default to f64.
            serde_json::Value::Number(x) => {
                if x.is_i64() {
                    JsonCliArgValueData::Int(x.as_i64().unwrap().to_string())
                } else {
                    JsonCliArgValueData::Float(x.as_f64().unwrap().to_string())
                }
            }
            serde_json::Value::String(x) => JsonCliArgValueData::String(x.clone()),
            serde_json::Value::Array(xs) => {
                let mut vec = Vec::with_capacity(xs.len());
                for x in xs {
                    vec.push(Self::from_serde_value(x))
                }
                JsonCliArgValueData::List(vec)
            }
            serde_json::Value::Object(mp) => {
                let mut res = OrderedMap::new();
                for (k, v) in mp.into_iter() {
                    res.insert(k.clone(), Self::from_serde_value(v));
                }
                JsonCliArgValueData::Object(res)
            }
        }
    }

    pub(crate) fn as_starlark<'v>(&self, heap: &'v Heap) -> Value<'v> {
        match self {
            Self::Bool(b) => Value::new_bool(*b),
            // Verified when we constructed these from the `serde_json::Value`s
            Self::Float(i) => heap.alloc(i.parse::<f64>().expect("already verified")),
            Self::Int(i) => heap.alloc(BigInt::from(i.parse::<i64>().expect("already verified"))),
            Self::String(s) => heap.alloc(s),
            Self::List(l) => heap.alloc(AllocList(l.iter().map(|v| v.as_starlark(heap)))),
            Self::None => Value::new_none(),
            Self::Object(mp) => {
                let mut res = SmallMap::with_capacity(mp.len());
                for (k, v) in mp.into_iter() {
                    res.insert_hashed(
                        heap.alloc_str(k).to_value().get_hashed().unwrap(),
                        v.as_starlark(heap),
                    );
                }
                heap.alloc(Dict::new(res))
            }
        }
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
    Int(BigInt),
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
    // For json CLI arg, we do not allow defaults, and we only allow primitives that can be
    // deserialized into a `serde_json::Value`.
    Json(JsonCliArgValueData),
}

impl CliArgValue {
    pub(crate) fn as_starlark<'v>(&self, heap: &'v Heap) -> Value<'v> {
        match self {
            CliArgValue::Bool(b) => Value::new_bool(*b),
            CliArgValue::Int(i) => heap.alloc(i.clone()),
            CliArgValue::Float(f) => heap.alloc(f.parse::<f64>().expect("already verified")),
            CliArgValue::String(s) => heap.alloc(s),
            CliArgValue::List(l) => heap.alloc(AllocList(l.iter().map(|v| v.as_starlark(heap)))),
            CliArgValue::None => Value::new_none(),
            CliArgValue::TargetLabel(t) => heap.alloc(StarlarkTargetLabel::new(t.dupe())),
            CliArgValue::ProvidersLabel(p) => heap.alloc(StarlarkProvidersLabel::new(p.clone())),
            CliArgValue::Json(j) => heap.alloc(j.as_starlark(heap)),
        }
    }
}

#[derive(Debug, VariantName, Clone, Dupe, Allocative)]
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
    Json,
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

    fn json() -> Self {
        CliArgType::Json
    }
}

#[derive(Debug, buck2_error::Error)]
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
    #[error("Expecting json object. Got: `{0}`")]
    NotAJsonObject(String),
}

impl CliArgType {
    fn coerce_value<'v>(&self, value: Value<'v>) -> anyhow::Result<CliArgValue> {
        Ok(match self {
            CliArgType::Bool => CliArgValue::Bool(value.unpack_bool().ok_or_else(|| {
                CliArgError::DefaultValueTypeError(self.dupe(), value.get_type().to_owned())
            })?),
            CliArgType::Int => CliArgValue::Int(BigInt::unpack_value(value).ok_or_else(|| {
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
            CliArgType::Json => {
                return Err(anyhow::anyhow!(CliArgError::NoDefaultsAllowed(
                    CliArgType::Json
                )));
            }
        })
    }

    #[allow(deprecated)] // TODO(nga): fix.
    pub(crate) fn to_clap<'a>(&'a self, clap: clap::Arg<'a>) -> clap::Arg<'a> {
        match self {
            CliArgType::Bool => clap.takes_value(true).validator(|x| x.parse::<bool>()),
            CliArgType::Int => clap.takes_value(true).validator(|x| x.parse::<BigInt>()),
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
            CliArgType::Json => clap.takes_value(true),
        }
    }

    pub(crate) fn parse_clap<'a>(
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
                    let r: anyhow::Result<_> = try { CliArgValue::Int(x.parse::<BigInt>()?) };
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
                                &ctx.cell_alias_resolver,
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
                                &ctx.cell_alias_resolver,
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
                        &ctx.cell_alias_resolver,
                    )?;
                    let loaded = load_patterns(
                        &mut ctx.dice.clone(),
                        vec![pattern],
                        MissingTargetBehavior::Fail,
                    )
                    .await?;
                    Some(CliArgValue::List(
                        loaded
                            .iter_loaded_targets()
                            .map_ok(|t| CliArgValue::TargetLabel(t.label().dupe()))
                            .collect::<buck2_error::Result<_>>()?,
                    ))
                }
                CliArgType::SubTargetExpr => {
                    let x = clap.value_of().unwrap_or("");
                    let pattern = ParsedPattern::<ProvidersPatternExtra>::parse_relaxed(
                        &ctx.target_alias_resolver,
                        ctx.relative_dir.as_cell_path(),
                        x,
                        &ctx.cell_resolver,
                        &ctx.cell_alias_resolver,
                    )?;
                    let loaded = load_patterns(
                        &mut ctx.dice.clone(),
                        vec![pattern],
                        MissingTargetBehavior::Fail,
                    )
                    .await?;

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
                            .collect::<buck2_error::Result<Vec<_>>>()?,
                    ))
                }
                CliArgType::Json => match clap.value_of() {
                    None => None,
                    Some(value) => {
                        let json: serde_json::Value = serde_json::from_str(value)?;
                        let data = JsonCliArgValueData::from_serde_value(&json);
                        if let JsonCliArgValueData::Object(_) = data {
                            Some(CliArgValue::Json(data))
                        } else {
                            return Err(CliArgError::NotAJsonObject(json.to_string()).into());
                        }
                    }
                },
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
        #[starlark(require = pos)] variants: UnpackListOrTuple<String>,
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

    fn json<'v>(
        #[starlark(default = "")] doc: &str,
        #[starlark(require = named)] short: Option<Value<'v>>,
    ) -> anyhow::Result<CliArgs> {
        CliArgs::new(None, doc, CliArgType::json(), short)
    }
}

pub(crate) fn register_cli_args_module(registry: &mut GlobalsBuilder) {
    cli_args_module(registry)
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

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use buck2_core::provider::label::testing::ProvidersLabelTestExt;
    use buck2_core::provider::label::ProvidersLabel;
    use buck2_core::target::label::TargetLabel;
    use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
    use buck2_interpreter::types::target_label::StarlarkTargetLabel;
    use num_bigint::BigInt;
    use starlark::values::Heap;
    use starlark::values::Value;
    use starlark_map::ordered_map::OrderedMap;

    use crate::bxl::starlark_defs::cli_args::CliArgType;
    use crate::bxl::starlark_defs::cli_args::CliArgValue;
    use crate::bxl::starlark_defs::cli_args::JsonCliArgValueData;

    #[test]
    fn print_cli_arg_list() -> anyhow::Result<()> {
        let args = vec![
            CliArgValue::Bool(true),
            CliArgValue::String("test".to_owned()),
            CliArgValue::Int(BigInt::from(1)),
        ];

        let cli_arg = CliArgValue::List(args);
        let printed = format!("{}", cli_arg);
        assert_eq!(printed, "true,test,1");

        Ok(())
    }

    #[test]
    fn print_cli_arg_json() -> anyhow::Result<()> {
        let mut args = OrderedMap::new();
        args.insert("my_bool".to_owned(), JsonCliArgValueData::Bool(true));
        args.insert(
            "my_string".to_owned(),
            JsonCliArgValueData::String("test".to_owned()),
        );
        args.insert(
            "my_array".to_owned(),
            JsonCliArgValueData::List(vec![
                JsonCliArgValueData::String("a".to_owned()),
                JsonCliArgValueData::String("b".to_owned()),
                JsonCliArgValueData::String("c".to_owned()),
            ]),
        );

        args.insert(
            "nested".to_owned(),
            JsonCliArgValueData::Object(args.clone()),
        );

        let printed = format!("{}", JsonCliArgValueData::Object(args));
        assert_eq!(
            printed,
            "(k=my_bool,v=true),(k=my_string,v=test),(k=my_array,v=a,b,c),(k=nested,v=(k=my_bool,v=true),(k=my_string,v=test),(k=my_array,v=a,b,c))"
        );

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
            CliArgValue::Int(BigInt::from(42))
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
                CliArgValue::Int(BigInt::from(1)),
                CliArgValue::Int(BigInt::from(4)),
                CliArgValue::Int(BigInt::from(2))
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
