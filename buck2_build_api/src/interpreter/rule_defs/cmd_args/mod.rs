/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    any::TypeId,
    cell::RefCell,
    convert::TryInto,
    fmt::{self, Debug, Display},
    ptr,
};

use anyhow::anyhow;
use buck2_core::fs::{
    paths::{RelativePath, RelativePathBuf},
    project::ProjectRelativePathBuf,
};
use derive_more::Display;
use either::Either;
use gazebo::{
    any::AnyLifetime,
    cell::ARef,
    coerce::{coerce_ref, Coerce},
    prelude::*,
};
use indexmap::IndexSet;
use serde::{Serialize, Serializer};
use starlark::{
    environment::{GlobalsBuilder, Methods, MethodsBuilder, MethodsStatic},
    starlark_type,
    values::{
        list::List, Freeze, Freezer, FrozenRef, FrozenValue, NoSerialize, StarlarkValue, Trace,
        Value, ValueError, ValueLike,
    },
};

use crate::{
    actions::artifact::ArtifactFs,
    artifact_groups::ArtifactGroup,
    attrs::attr_type::arg::value::ResolvedStringWithMacros,
    interpreter::rule_defs::{
        artifact::{
            FrozenStarlarkOutputArtifact, StarlarkArtifact, StarlarkArtifactLike,
            StarlarkDeclaredArtifact, StarlarkOutputArtifact, ValueAsArtifactLike,
        },
        artifact_tagging::{FrozenTaggedArtifacts, TaggedArtifacts},
        cmd_args::builder::{
            CommandLineArgLike, CommandLineArtifactVisitor, CommandLineBuilder,
            CommandLineBuilderContext, CommandLineLocation, SimpleCommandLineArtifactVisitor,
            WriteToFileMacroVisitor,
        },
        label_relative_path::LabelRelativePath,
        provider::run_info::{FrozenRunInfo, RunInfo},
        test_cwd::TestCwd,
        transitive_set::{FrozenTransitiveSetArgsProjection, TransitiveSetArgsProjection},
        util::commas,
    },
};

pub mod builder;

/// A tiny wrapper around `Value`/`FrozenValue` that proxies `CommandLineArgLike` calls.
///
/// This should be unnecessary, however I'm not smart enough to figure out how to get
/// things to live long enough, in `ValueAsCommandLineArgLike`, so I'm moving on with my life
/// for now. All values contained in here are guaranteed to implement `CommandLineArgLike`.
#[derive(Debug, Clone, Copy, Dupe, Trace, Display, Serialize)]
#[serde(bound = "V: Display", transparent)]
struct CommandLineArgGen<V>(#[serde(serialize_with = "serialize_as_display")] V);

#[derive(Debug, thiserror::Error)]
enum CommandLineArgError {
    #[error(
        "expected command line item to be a string, artifact, or label, or list thereof, not `{repr}`"
    )]
    InvalidItemType { repr: String },
    #[error("too many .parent() calls")]
    TooManyParentCalls,
    #[error("Unknown quoting style `{0}`")]
    UnknownQuotingStyle(String),
}

impl<'v> CommandLineArgGen<Value<'v>> {
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<CommandLineArgGen<FrozenValue>> {
        Ok(CommandLineArgGen(self.0.freeze(freezer)?))
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgGen<V> {
    fn try_from_value(value: V) -> anyhow::Result<Self> {
        value.to_value().as_command_line_err()?;
        Ok(Self(value))
    }

    fn visit_inner<R>(&self, f: impl FnOnce(&dyn CommandLineArgLike) -> R) -> R {
        f(&*self.0.to_value().as_command_line().unwrap())
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for CommandLineArgGen<V> {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        self.visit_inner(|x| x.add_to_command_line(cli))
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        self.visit_inner(|x| x.visit_artifacts(visitor))
    }

    fn contains_arg_attr(&self) -> bool {
        self.visit_inner(|x| x.contains_arg_attr())
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        self.visit_inner(|x| x.visit_write_to_file_macros(visitor))
    }
}

pub(crate) trait ValueAsCommandLineLike<'v> {
    fn as_command_line(&self) -> Option<&'v dyn CommandLineArgLike>;
    fn as_command_line_err(&self) -> anyhow::Result<&'v dyn CommandLineArgLike>;
}

pub trait FrozenCommandLineArgLike:
    CommandLineArgLike + Send + Sync + Debug + Display + 'static
{
    fn ptr_eq(&self, other: &dyn FrozenCommandLineArgLike) -> bool;
}

impl<T> FrozenCommandLineArgLike for T
where
    T: CommandLineArgLike + Send + Sync + Debug + Display + 'static,
{
    fn ptr_eq(&self, other: &dyn FrozenCommandLineArgLike) -> bool {
        ptr::eq(
            self as *const T,
            other as *const dyn FrozenCommandLineArgLike as *const T,
        )
    }
}

impl PartialEq for dyn FrozenCommandLineArgLike {
    fn eq(&self, other: &Self) -> bool {
        // use simple ptr eq, which is the default behaviour for starlark values
        self.ptr_eq(other)
    }
}

pub(crate) trait ValueAsFrozenCommandLineLike {
    fn as_frozen_command_line(&self) -> Option<FrozenRef<'static, dyn FrozenCommandLineArgLike>>;
}

impl<'v> ValueAsCommandLineLike<'v> for Value<'v> {
    fn as_command_line(&self) -> Option<&'v dyn CommandLineArgLike> {
        if let Some(x) = self.to_value().unpack_starlark_str() {
            return Some(x as &dyn CommandLineArgLike);
        }

        let aref = self.to_value().as_dyn_any();
        let ty = aref.static_type_of();

        macro_rules! check {
            ($t:ty) => {
                if ty == TypeId::of::<$t>() {
                    return Some(aref.downcast_ref::<$t>().unwrap() as &dyn CommandLineArgLike);
                }
            };
        }

        check!(StarlarkCommandLine);
        check!(FrozenStarlarkCommandLine);
        check!(StarlarkArtifact);
        check!(StarlarkDeclaredArtifact);
        check!(StarlarkOutputArtifact);
        check!(FrozenStarlarkOutputArtifact);
        check!(ResolvedStringWithMacros);
        check!(RunInfo);
        check!(FrozenRunInfo);
        check!(LabelRelativePath);
        check!(FrozenTransitiveSetArgsProjection);
        check!(TransitiveSetArgsProjection);
        check!(FrozenTaggedArtifacts);
        check!(TaggedArtifacts);
        None
    }

    fn as_command_line_err(&self) -> anyhow::Result<&'v dyn CommandLineArgLike> {
        self.as_command_line().ok_or_else(|| {
            CommandLineArgError::InvalidItemType {
                repr: self.to_value().to_repr(),
            }
            .into()
        })
    }
}

impl ValueAsFrozenCommandLineLike for FrozenValue {
    fn as_frozen_command_line(&self) -> Option<FrozenRef<'static, dyn FrozenCommandLineArgLike>> {
        if let Some(x) = self.downcast_frozen_starlark_str() {
            return Some(x.map(|s| s as &dyn FrozenCommandLineArgLike));
        }

        macro_rules! check {
            ($t:ty) => {
                if let Some(x) = self.downcast_frozen_ref::<$t>() {
                    return Some(x.map(|v| v as &dyn FrozenCommandLineArgLike));
                }
            };
        }

        check!(FrozenStarlarkCommandLine);
        check!(StarlarkArtifact);
        check!(FrozenStarlarkOutputArtifact);
        check!(ResolvedStringWithMacros);
        check!(FrozenRunInfo);
        check!(LabelRelativePath);
        check!(FrozenTransitiveSetArgsProjection);
        None
    }
}

/// Supported ways of quoting arguments.
#[derive(Debug, Clone, Dupe, Trace, Serialize)]
pub enum QuoteStyle {
    /// Quote arguments for Unix shell:
    /// <https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html>
    Shell,
}

impl Display for QuoteStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Shell => write!(f, "shell"),
        }
    }
}

impl QuoteStyle {
    pub fn parse(s: &str) -> anyhow::Result<QuoteStyle> {
        match s {
            "shell" => Ok(QuoteStyle::Shell),
            _ => Err(anyhow!(CommandLineArgError::UnknownQuotingStyle(
                s.to_owned()
            ))),
        }
    }
}

/// Simple struct to help determine extra options for formatting
/// (whether to join items, how to join them, format strings, etc)
#[derive(Debug, Default_, Clone, Trace, Serialize)]
pub struct FormattingOptions {
    concat: Option<String>,
    format_string: Option<String>,
    quote: Option<QuoteStyle>,
    prepend: Option<String>,
}

impl Display for FormattingOptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut comma = commas();
        if let Some(v) = &self.concat {
            comma(f)?;
            write!(f, "concat = {:?}", v)?;
        }
        if let Some(v) = &self.format_string {
            comma(f)?;
            write!(f, "absolute_prefix = {:?}", v)?;
        }
        if let Some(v) = &self.quote {
            comma(f)?;
            write!(f, "quote = \"{}\"", v)?;
        }
        if let Some(v) = &self.prepend {
            comma(f)?;
            write!(f, "prepend = \"{}\"", v)?;
        }
        Ok(())
    }
}

impl FormattingOptions {
    /// If any of the params passed in are meaningful, return
    /// an instance of `FormattingOptions`, else just return `None`.
    ///
    /// This lets us shortcut some formatting logic later on if no meaningful
    /// formatting needs to be done (the common case)
    pub fn maybe_new(
        concat_items: bool,
        concat_delimiter: Option<String>,
        format_string: Option<String>,
        quote: Option<QuoteStyle>,
        prepend: Option<String>,
    ) -> Option<Self> {
        match (
            concat_items,
            concat_delimiter,
            format_string,
            quote,
            prepend,
        ) {
            (false, None, None, None, None) => None,
            (concat_items, concat_delimiter, format_string, quote, prepend) => Some(Self {
                concat: if concat_items {
                    Some(concat_delimiter.unwrap_or_default())
                } else {
                    concat_delimiter
                },
                format_string,
                quote,
                prepend,
            }),
        }
    }

    fn extra_memory(&self) -> usize {
        let mut ret = 0;
        if let Some(f) = self.format_string.as_ref() {
            ret += f.capacity();
        }
        if let Some(f) = self.prepend.as_ref() {
            ret += f.capacity();
        }
        ret
    }
}

#[derive(Debug, Default_, Clone, Trace, Serialize)]
#[repr(C)]
struct CommandLineOptions<V> {
    #[serde(bound = "V: Display", serialize_with = "serialize_opt_display")]
    relative_to: Option<(V, usize)>,
    absolute_prefix: Option<String>,
    absolute_suffix: Option<String>,
    parent: usize,
    ignore_artifacts: bool,
    formatting: Option<FormattingOptions>,
}

fn serialize_opt_display<V: Display, S>(v: &Option<(V, usize)>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match v.as_ref() {
        Some((v, u)) => s.serialize_some(&(format!("{}", v), u)),
        None => s.serialize_none(),
    }
}

fn serialize_as_display<V: Display, S>(v: &V, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    s.collect_str(v)
}

impl<V: Display> Display for CommandLineOptions<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut comma = commas();
        if let Some((v, i)) = &self.relative_to {
            comma(f)?;
            write!(f, "relative_to = {}", v)?;
            if *i != 0 {
                comma(f)?;
                write!(f, "relative_to_parent = {}", i)?;
            }
        }
        if let Some(v) = &self.absolute_prefix {
            comma(f)?;
            write!(f, "absolute_prefix = {}", v)?;
        }
        if let Some(v) = &self.absolute_suffix {
            comma(f)?;
            write!(f, "absolute_prefix = {}", v)?;
        }
        if self.parent != 0 {
            comma(f)?;
            write!(f, "parent = {}", self.parent)?;
        }
        if self.ignore_artifacts {
            comma(f)?;
            write!(f, "ignore_artifacts = True")?;
        }
        if let Some(x) = &self.formatting {
            comma(f)?;
            Display::fmt(x, f)?;
        }
        Ok(())
    }
}

impl<V> CommandLineOptions<V> {
    fn extra_memory(&self) -> usize {
        let mut ret = 0;
        if let Some(p) = self.absolute_prefix.as_ref() {
            ret += p.capacity();
        }
        if let Some(p) = self.absolute_suffix.as_ref() {
            ret += p.capacity();
        }
        if let Some(f) = self.formatting.as_ref() {
            ret += f.extra_memory();
        }
        ret
    }
}

/// Starlark object returned by `cmd_args()`
/// A container for all of the args and nested command lines that a users adds to `ctx.args()`
///
/// This allows more efficient iterative argument building, including O(1) insertion of
/// `CommandLine` / `FrozenCommandLine` args.
///
/// When frozen, a `FrozenCommandLine` is created.
///
/// `items` contains strings, artifacts, command line args (frozen and not), but does not
///         contain any builders.
#[derive(Debug, Default_, Clone, Trace, AnyLifetime, Serialize)]
#[serde(bound = "V: Display")]
#[repr(C)]
pub struct StarlarkCommandLineDataGen<V> {
    items: Vec<CommandLineArgGen<V>>,
    hidden: Vec<CommandLineArgGen<V>>,
    options: Option<Box<CommandLineOptions<V>>>,
}

impl<V: Display> Display for StarlarkCommandLineDataGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut comma = commas();
        write!(f, "cmd_args(")?;
        for x in &self.items {
            comma(f)?;
            x.fmt(f)?;
        }
        if !self.hidden.is_empty() {
            comma(f)?;
            let mut hidden_commas = commas();
            write!(f, "hidden = [")?;
            for x in &self.hidden {
                hidden_commas(f)?;
                x.fmt(f)?;
            }
            write!(f, "]")?;
        }
        if let Some(opts) = &self.options {
            comma(f)?;
            opts.fmt(f)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl<V> StarlarkCommandLineDataGen<V> {
    fn relative_to(&self) -> Option<&(V, usize)> {
        self.options.as_ref()?.relative_to.as_ref()
    }

    fn relative_to_mut(&mut self) -> &mut Option<(V, usize)> {
        &mut self.options_mut().relative_to
    }

    fn absolute_prefix(&self) -> Option<&String> {
        self.options.as_ref()?.absolute_prefix.as_ref()
    }

    fn absolute_prefix_mut(&mut self) -> &mut Option<String> {
        &mut self.options_mut().absolute_prefix
    }

    fn absolute_suffix(&self) -> Option<&String> {
        self.options.as_ref()?.absolute_suffix.as_ref()
    }

    fn absolute_suffix_mut(&mut self) -> &mut Option<String> {
        &mut self.options_mut().absolute_suffix
    }

    fn parent(&self) -> usize {
        self.options.as_ref().map(|o| o.parent).unwrap_or_default()
    }

    fn parent_mut(&mut self) -> &mut usize {
        &mut self.options_mut().parent
    }

    fn ignore_artifacts(&self) -> bool {
        self.options
            .as_ref()
            .map(|o| o.ignore_artifacts)
            .unwrap_or_default()
    }

    fn ignore_artifacts_mut(&mut self) -> &mut bool {
        &mut self.options_mut().ignore_artifacts
    }

    fn formatting(&self) -> Option<&FormattingOptions> {
        self.options.as_ref()?.formatting.as_ref()
    }

    fn formatting_mut(&mut self) -> &mut Option<FormattingOptions> {
        &mut self.options_mut().formatting
    }

    fn options_mut(&mut self) -> &mut CommandLineOptions<V> {
        if self.options.is_none() {
            self.options = Some(box Default::default());
        }
        self.options.as_mut().unwrap()
    }
}

impl<T> StarlarkCommandLineDataGen<T> {
    fn is_concat(&self) -> bool {
        if let Some(x) = &self.options {
            if let Some(x) = &x.formatting {
                return x.concat.is_some();
            }
        }
        false
    }
}

impl<'v> StarlarkCommandLine<'v> {
    pub(crate) fn is_concat(&self) -> bool {
        self.0.borrow().is_concat()
    }
}

impl FrozenStarlarkCommandLine {
    pub(crate) fn is_concat(&self) -> bool {
        self.0.is_concat()
    }
}

unsafe impl<To, From: Coerce<To>> Coerce<StarlarkCommandLineDataGen<To>>
    for StarlarkCommandLineDataGen<From>
{
}

#[derive(Debug, Default, Clone, Trace, AnyLifetime, Serialize)]
#[serde(bound = "T : Serialize", transparent)]
pub struct StarlarkCommandLineGen<T>(T);

pub type StarlarkCommandLine<'v> =
    StarlarkCommandLineGen<RefCell<StarlarkCommandLineDataGen<Value<'v>>>>;
pub type FrozenStarlarkCommandLine =
    StarlarkCommandLineGen<StarlarkCommandLineDataGen<FrozenValue>>;

starlark_complex_values!(StarlarkCommandLine);

impl<'v> Display for StarlarkCommandLine<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.try_borrow() {
            Ok(x) => Display::fmt(&x, f),
            Err(_) => write!(f, "<cmd_args borrowed>"),
        }
    }
}

impl Display for FrozenStarlarkCommandLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<'v> StarlarkCommandLine<'v> {
    pub fn short_name(&self) -> String {
        self.0.borrow().short_name()
    }
}

impl<'v, V: ValueLike<'v>> StarlarkCommandLineDataGen<V> {
    fn short_name_cmdarg(v: &CommandLineArgGen<V>) -> Option<&'v str> {
        if let Some(x) = v.0.to_value().unpack_str() {
            Some(x)
        } else {
            match StarlarkCommandLine::from_value(v.0.to_value())? {
                Either::Left(x) => x.0.borrow().short_name_cmdline(),
                Either::Right(x) => x.0.short_name_cmdline(),
            }
        }
    }

    fn short_name_cmdline(&self) -> Option<&'v str> {
        Self::short_name_cmdarg(self.items.first()?)
    }

    /// Find a suitable short-name for the command, if the user didn't supply one.
    /// We use the first argument to the command, if there is one.
    fn short_name(&self) -> String {
        match self.short_name_cmdline() {
            Some(v) if v.len() < 100 => format!("run {}", v),
            _ => "run command".to_owned(),
        }
    }

    fn relative_to_path<C>(&self, ctx: &C) -> anyhow::Result<Option<RelativePathBuf>>
    where
        C: CommandLineBuilderContext + ?Sized,
    {
        let (value, parent) = match self.relative_to() {
            Some((v, p)) => (*v, *p),
            None => return Ok(None),
        };

        let origin = RelativeOrigin::from_value(value)
            .expect("Must be a valid RelativeOrigin as this was checked in the setter");
        let mut relative_path = origin.resolve(ctx)?;
        for _ in 0..parent {
            if !relative_path.pop() {
                return Err(
                    anyhow!(CommandLineArgError::TooManyParentCalls).context(format!(
                        "Error accessing {}-th parent of {}",
                        parent, origin
                    )),
                );
            }
        }

        Ok(Some(relative_path))
    }
}

impl<'v> StarlarkValue<'v> for StarlarkCommandLine<'v> {
    starlark_type!("cmd_args");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(command_line_builder_methods)
    }

    fn extra_memory(&self) -> usize {
        self.0.borrow().extra_memory()
    }
}

impl<'v> StarlarkValue<'v> for FrozenStarlarkCommandLine {
    starlark_type!("cmd_args");

    fn get_methods(&self) -> Option<&'static Methods> {
        // We return the same methods for frozen command lines, even though some of them fail,
        // so the methods remain consistent during freezing
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(command_line_builder_methods)
    }

    fn extra_memory(&self) -> usize {
        self.0.extra_memory()
    }
}

impl<'v, V: ValueLike<'v>> CommandLineArgLike for StarlarkCommandLineDataGen<V> {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        struct Extras<'a> {
            cli: &'a mut dyn CommandLineBuilder,
            relative_to: Option<RelativePathBuf>,
            absolute_prefix: Option<&'a String>,
            absolute_suffix: Option<&'a String>,
            parent: usize,
            // Auxiliary field to store concatenation result (when arguments are concatenated) and
            // a flag stating that the result is not yet started to be computated (i.e. the first
            // argument to be concatenated is not yet processed).
            concatenation_context: Option<(String, bool)>,
            formatting: FormattingOptions,
        }

        impl<'a> Extras<'a> {
            /// If any items need to be concatted/formatted and added to the original CLI,
            /// do it here
            fn finalize_args(mut self) -> Self {
                if let Some((concatted_items, _)) = self.concatenation_context.take() {
                    self.cli.add_arg_string(self.format(concatted_items));
                }
                self
            }

            fn format(&self, mut arg: String) -> String {
                if let Some(format) = &self.formatting.format_string {
                    arg = format.replace("{}", &arg);
                }
                match &self.formatting.quote {
                    Some(QuoteStyle::Shell) => {
                        arg = shlex::quote(&arg).into_owned();
                    }
                    _ => {}
                }
                arg
            }
        }

        impl<'a> CommandLineBuilderContext for Extras<'a> {
            fn resolve_project_path(
                &self,
                path: ProjectRelativePathBuf,
            ) -> anyhow::Result<CommandLineLocation> {
                let Self {
                    cli,
                    parent,
                    absolute_prefix,
                    absolute_suffix,
                    relative_to,
                    ..
                } = self;

                let resolved = cli.resolve_project_path(path)?;

                if *parent == 0
                    && absolute_prefix.is_none()
                    && absolute_suffix.is_none()
                    && relative_to.is_none()
                {
                    return Ok(resolved);
                }

                let mut x = resolved.into_relative();
                if let Some(relative_to) = relative_to {
                    x = relative_to.relative(x);
                }
                let mut parent_ref = x.as_relative_path();
                for _ in 0..*parent {
                    parent_ref = parent_ref
                        .parent()
                        .ok_or(CommandLineArgError::TooManyParentCalls)?;
                }
                x = parent_ref.to_owned();
                if absolute_prefix.is_some() || absolute_suffix.is_some() {
                    x = RelativePath::new(&format!(
                        "{}{}{}",
                        absolute_prefix.as_ref().map_or("", |s| s.as_str()),
                        x,
                        absolute_suffix.as_ref().map_or("", |s| s.as_str()),
                    ))
                    .to_owned();
                }
                Ok(CommandLineLocation::from_relative_path(x))
            }

            fn fs(&self) -> &ArtifactFs {
                self.cli.fs()
            }

            fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
                let macro_path = self.cli.next_macro_file_path()?;
                if let Some(relative_to_path) = &self.relative_to {
                    Ok(relative_to_path.relative(macro_path))
                } else {
                    Ok(macro_path)
                }
            }
        }

        impl<'a> CommandLineBuilder for Extras<'a> {
            fn add_arg_string(&mut self, s: String) {
                if let Some((concatted_items, initital_state)) = self.concatenation_context.as_mut()
                {
                    if *initital_state {
                        *initital_state = false;
                    } else {
                        concatted_items
                            .push_str(self.formatting.concat.as_ref().map_or("", String::as_str));
                    }
                    concatted_items.push_str(&s)
                } else {
                    // NOTE: This doesn't go through formatting since to give users more
                    // flexibility. Since the prepended string is a static string, they _can_ format
                    // it ahead of time if they need to.
                    if let Some(i) = self.formatting.prepend.as_ref() {
                        self.cli.add_arg_string(i.to_owned());
                    }
                    self.cli.add_arg_string(self.format(s))
                }
            }
        }

        match (
            self.relative_to_path(cli).transpose(),
            self.absolute_prefix(),
            self.absolute_suffix(),
            self.parent(),
            self.formatting(),
        ) {
            (None, None, None, 0, None) => {
                for item in &self.items {
                    item.add_to_command_line(cli)?;
                }
            }
            (relative_to, absolute_prefix, absolute_suffix, parent, formatting) => {
                let concatenation_context = match formatting {
                    Some(opts) if opts.concat.is_some() => Some((String::new(), true)),
                    _ => None,
                };
                let formatting = match formatting {
                    Some(f) => (*f).clone(),
                    None => FormattingOptions::default(),
                };
                let mut cli_extras = Extras {
                    cli,
                    relative_to: relative_to.transpose()?,
                    absolute_prefix,
                    absolute_suffix,
                    parent,
                    concatenation_context,
                    formatting,
                };
                for item in &self.items {
                    item.add_to_command_line(&mut cli_extras)?;
                }
                cli_extras.finalize_args();
            }
        }
        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        if !self.ignore_artifacts() {
            for item in &self.items {
                item.visit_artifacts(visitor)?;
            }
            for item in &self.hidden {
                item.visit_artifacts(visitor)?;
            }
        }
        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        self.items.iter().any(|x| x.contains_arg_attr())
            || self.hidden.iter().any(|x| x.contains_arg_attr())
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        visitor.set_current_relative_to_path(&|ctx| self.relative_to_path(ctx))?;

        for item in &self.items {
            item.visit_write_to_file_macros(visitor)?;
        }
        for item in &self.hidden {
            item.visit_write_to_file_macros(visitor)?;
        }
        Ok(())
    }
}

impl<'v> CommandLineArgLike for StarlarkCommandLine<'v> {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        self.0.borrow().add_to_command_line(cli)
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        self.0.borrow().visit_artifacts(visitor)
    }

    fn contains_arg_attr(&self) -> bool {
        self.0.borrow().contains_arg_attr()
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        self.0.borrow().visit_write_to_file_macros(visitor)
    }
}

impl<'v> CommandLineArgLike for FrozenStarlarkCommandLine {
    fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        self.0.add_to_command_line(cli)
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        self.0.visit_artifacts(visitor)
    }

    fn contains_arg_attr(&self) -> bool {
        self.0.contains_arg_attr()
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        self.0.visit_write_to_file_macros(visitor)
    }
}

impl<'v> Freeze for StarlarkCommandLine<'v> {
    type Frozen = StarlarkCommandLineGen<StarlarkCommandLineDataGen<FrozenValue>>;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let StarlarkCommandLineDataGen {
            items,
            hidden,
            options,
        } = self.0.into_inner();

        let items = items.into_try_map(|x| x.freeze(freezer))?;
        let hidden = hidden.into_try_map(|x| x.freeze(freezer))?;

        let options = options.into_try_map(|options| {
            let CommandLineOptions {
                relative_to,
                absolute_prefix,
                absolute_suffix,
                parent,
                ignore_artifacts,
                formatting,
            } = *options;

            let relative_to =
                relative_to.into_try_map(|(x, p)| anyhow::Ok((x.freeze(freezer)?, p)))?;

            anyhow::Ok(box CommandLineOptions {
                relative_to,
                absolute_prefix,
                absolute_suffix,
                parent,
                ignore_artifacts,
                formatting,
            })
        })?;

        Ok(StarlarkCommandLineGen(StarlarkCommandLineDataGen {
            items,
            hidden,
            options,
        }))
    }
}

impl<'v> StarlarkCommandLine<'v> {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Create a slightly more advanced builder.
    pub fn new_with_options(options: Option<FormattingOptions>) -> Self {
        let mut gen = StarlarkCommandLineDataGen::default();
        if let Some(options) = options {
            *gen.formatting_mut() = Some(options);
        }
        Self(RefCell::new(gen))
    }

    pub(crate) fn try_from_value(value: Value<'v>) -> anyhow::Result<Self> {
        let mut builder = Self::new();
        builder.0.get_mut().add_value(value)?;
        Ok(builder)
    }

    fn try_from_values_with_options(
        value: &[Value<'v>],
        formatting: Option<FormattingOptions>,
    ) -> anyhow::Result<Self> {
        let mut builder = Self::new_with_options(formatting);
        let b = builder.0.get_mut();
        for v in value {
            b.add_value(*v)?;
        }
        Ok(builder)
    }
}

impl<V> StarlarkCommandLineDataGen<V> {
    fn extra_memory(&self) -> usize {
        let args_extra_memory = (self.items.capacity() + self.hidden.capacity())
            * std::mem::size_of::<CommandLineArgGen<V>>();

        let opts_extra_memory = if let Some(opts) = self.options.as_ref() {
            std::mem::size_of::<CommandLineOptions<V>>() + opts.extra_memory()
        } else {
            0
        };

        args_extra_memory + opts_extra_memory
    }
}

impl<'v> StarlarkCommandLineDataGen<Value<'v>> {
    fn add_value(&mut self, value: Value<'v>) -> anyhow::Result<()> {
        if let Some(values) = List::from_value(value) {
            self.add_values(values.content())?;
        } else {
            self.items.push(CommandLineArgGen::try_from_value(value)?);
        }
        Ok(())
    }

    /// Check the types of a list of values, and modify `data` accordingly
    ///
    /// The values must be one of: CommandLineArgLike or a list thereof.
    fn add_values(&mut self, values: &[Value<'v>]) -> anyhow::Result<()> {
        self.items.reserve(values.len());
        for value in values {
            self.add_value(*value)?
        }
        Ok(())
    }

    /// Add values to the artifact that don't show up on the command line, but do for dependency
    fn add_hidden(&mut self, values: &[Value<'v>]) -> anyhow::Result<()> {
        for value in values {
            if let Some(values) = List::from_value(*value) {
                self.add_hidden(values.content())?;
            } else {
                self.hidden.push(CommandLineArgGen::try_from_value(*value)?);
            }
        }
        Ok(())
    }
}

fn cmd_args_mut<'v>(x: Value<'v>) -> anyhow::Result<&'v StarlarkCommandLine<'v>> {
    if let Some(v) = x.downcast_ref::<StarlarkCommandLine>() {
        Ok(v)
    } else {
        Err(ValueError::CannotMutateImmutableValue.into())
    }
}

fn cmd_args<'v>(x: Value<'v>) -> ARef<'v, StarlarkCommandLineDataGen<Value<'v>>> {
    if let Some(x) = x.downcast_ref::<StarlarkCommandLine>() {
        ARef::new_ref(x.0.borrow())
    } else if let Some(x) = x.downcast_ref::<FrozenStarlarkCommandLine>() {
        ARef::new_ptr(coerce_ref(&x.0))
    } else {
        unreachable!("This parameter must always be a type of command args")
    }
}

#[starlark_module]
pub fn register_args_function(builder: &mut GlobalsBuilder) {
    #[starlark(type = "cmd_args")]
    fn cmd_args<'v>(
        args: Vec<Value<'v>>,
        format: Option<String>,
        joined @ false: bool,
        delimiter: Option<String>,
        quote: Option<String>,
        prepend: Option<String>,
    ) -> anyhow::Result<StarlarkCommandLine<'v>> {
        StarlarkCommandLine::try_from_values_with_options(
            &args,
            FormattingOptions::maybe_new(
                joined,
                delimiter,
                format,
                quote.try_map(|q| QuoteStyle::parse(q))?,
                prepend,
            ),
        )
    }
}

#[starlark_module]
fn command_line_builder_methods(builder: &mut MethodsBuilder) {
    fn add<'v>(
        this: Value<'v>,
        args: Vec<Value<'v>>,
        format: Option<String>,
        quote: Option<String>,
        prepend: Option<String>,
    ) -> anyhow::Result<Value<'v>> {
        if format.is_some() || quote.is_some() || prepend.is_some() {
            let mut inner_builder =
                StarlarkCommandLine::new_with_options(FormattingOptions::maybe_new(
                    false,
                    None,
                    format,
                    quote.try_map(|q| QuoteStyle::parse(q))?,
                    prepend,
                ));
            inner_builder.0.get_mut().add_values(&args)?;
            cmd_args_mut(this)?
                .0
                .borrow_mut()
                .add_value(heap.alloc(inner_builder))?;
        } else {
            cmd_args_mut(this)?.0.borrow_mut().add_values(&args)?;
        }
        Ok(this)
    }

    fn add_joined<'v>(
        this: Value<'v>,
        args: Vec<Value<'v>>,
        delimiter: Option<String>,
        format: Option<String>,
        quote: Option<String>,
        prepend: Option<String>,
    ) -> anyhow::Result<Value<'v>> {
        let mut inner_builder =
            StarlarkCommandLine::new_with_options(FormattingOptions::maybe_new(
                true,
                delimiter,
                format,
                quote.try_map(|q| QuoteStyle::parse(q))?,
                prepend,
            ));
        inner_builder.0.get_mut().add_values(&args)?;
        cmd_args_mut(this)?
            .0
            .borrow_mut()
            .add_value(heap.alloc(inner_builder))?;
        Ok(this)
    }

    fn hidden<'v>(this: Value<'v>, args: Vec<Value<'v>>) -> anyhow::Result<Value<'v>> {
        cmd_args_mut(this)?.0.borrow_mut().add_hidden(&args)?;
        Ok(this)
    }

    fn ignore_artifacts<'v>(this: Value<'v>) -> anyhow::Result<Value<'v>> {
        *cmd_args_mut(this)?.0.borrow_mut().ignore_artifacts_mut() = true;
        Ok(this)
    }

    fn relative_to<'v>(
        this: Value<'v>,
        directory: Value<'v>,
        parent @ 0i32: i32,
    ) -> anyhow::Result<Value<'v>> {
        if RelativeOrigin::from_value(directory).is_none() {
            return Err(ValueError::IncorrectParameterTypeNamed("directory".to_owned()).into());
        }
        if parent < 0 {
            return Err(ValueError::IncorrectParameterTypeNamed("parent".to_owned()).into());
        }
        *cmd_args_mut(this)?.0.borrow_mut().relative_to_mut() = Some((directory, parent as usize));
        Ok(this)
    }

    fn absolute_prefix<'v>(this: Value<'v>, prefix: String) -> anyhow::Result<Value<'v>> {
        *cmd_args_mut(this)?.0.borrow_mut().absolute_prefix_mut() = Some(prefix);
        Ok(this)
    }

    fn absolute_suffix<'v>(this: Value<'v>, suffix: String) -> anyhow::Result<Value<'v>> {
        *cmd_args_mut(this)?.0.borrow_mut().absolute_suffix_mut() = Some(suffix);
        Ok(this)
    }

    /// For all the artifacts listed in this `cmd_args`, use their parent directory.
    /// Typically used when the file name is passed one way, and the directory another,
    /// e.g. `cmd_args(artifact, format="-L{}").parent()`.
    fn parent<'v>(this: Value<'v>, ref count @ 1i32: i32) -> anyhow::Result<Value<'v>> {
        if count < 0 {
            return Err(ValueError::IncorrectParameterTypeNamed("count".to_owned()).into());
        }
        *cmd_args_mut(this)?.0.borrow_mut().parent_mut() += count as usize;
        Ok(this)
    }

    fn copy<'v>(this: Value<'v>) -> anyhow::Result<StarlarkCommandLine<'v>> {
        Ok(StarlarkCommandLineGen(RefCell::new(cmd_args(this).clone())))
    }

    /// Collect all the inputs (including hidden) referenced by this command line.
    #[starlark(attribute)]
    fn inputs<'v>(this: Value<'v>) -> anyhow::Result<StarlarkCommandLineInputs> {
        let mut visitor = SimpleCommandLineArtifactVisitor::new();
        cmd_args(this).visit_artifacts(&mut visitor)?;
        Ok(StarlarkCommandLineInputs {
            inputs: visitor.inputs,
        })
    }

    /// Collect all the outputs (including hidden) referenced by this command line.
    #[starlark(attribute)]
    fn outputs<'v>(this: Value<'v>) -> anyhow::Result<Vec<StarlarkOutputArtifact>> {
        let mut visitor = SimpleCommandLineArtifactVisitor::new();
        cmd_args(this).visit_artifacts(&mut visitor)?;
        Ok(visitor
            .outputs
            .into_iter()
            .map(StarlarkOutputArtifact::new)
            .collect())
    }
}

/// A wrapper for a [StarlarkCommandLine]'s inputs. This is an opaque type that only allows
/// debug-printing and querying the length to tell if any inputs exist.
#[derive(Debug, PartialEq, AnyLifetime, NoSerialize)]
pub struct StarlarkCommandLineInputs {
    inputs: IndexSet<ArtifactGroup>,
}

starlark_simple_value!(StarlarkCommandLineInputs);

impl Display for StarlarkCommandLineInputs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut comma = commas();
        write!(f, "command_line_inputs(")?;
        for v in self.inputs.iter() {
            comma(f)?;
            Display::fmt(v, f)?;
        }
        write!(f, ")")
    }
}

impl<'v> StarlarkValue<'v> for StarlarkCommandLineInputs {
    starlark_type!("command_line_inputs");

    fn length(&self) -> anyhow::Result<i32> {
        Ok(self.inputs.len().try_into()?)
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.inputs == other.inputs)
        } else {
            Ok(false)
        }
    }
}

// NOTE: This is an enum as opposed to a trait beause of the `C` parameter on (which is required
// because upcasting is not stable).
#[derive(Display)]
enum RelativeOrigin<'v> {
    Artifact(&'v dyn StarlarkArtifactLike),
    TestCwd(&'v TestCwd),
}

impl<'v> RelativeOrigin<'v> {
    fn from_value<V>(v: V) -> Option<Self>
    where
        V: ValueLike<'v>,
    {
        if let Some(v) = v.as_artifact() {
            return Some(RelativeOrigin::Artifact(v));
        }

        if let Some(v) = v.downcast_ref::<TestCwd>() {
            return Some(RelativeOrigin::TestCwd(v));
        }

        None
    }

    fn resolve<C>(&self, ctx: &C) -> anyhow::Result<RelativePathBuf>
    where
        C: CommandLineBuilderContext + ?Sized,
    {
        let loc = match self {
            Self::Artifact(artifact) => {
                // Shame we require the artifact to be bound here, we really just needs its
                // path even if it is unbound.
                let artifact = artifact.get_bound()?;
                ctx.resolve_artifact(&artifact)?
            }
            Self::TestCwd(test_cwd) => ctx.resolve_cell_path(test_cwd.cell_path())?,
        };

        Ok(loc.into_relative())
    }
}

#[cfg(test)]
pub mod tester {
    use std::convert::TryFrom;

    use buck2_core::fs::{
        paths::AbsPathBuf,
        project::{ProjectFilesystem, ProjectRelativePathBuf},
    };
    use starlark::{environment::GlobalsBuilder, values::Value};

    use crate::{
        actions::artifact::ArtifactFs,
        interpreter::{
            rule_defs::cmd_args::{
                builder::BaseCommandLineBuilder, SimpleCommandLineArtifactVisitor,
                StarlarkCommandLine, StarlarkCommandLineInputs, ValueAsCommandLineLike,
            },
            testing::cells,
        },
        path::{BuckOutPathResolver, BuckPathResolver},
    };

    fn artifact_fs() -> ArtifactFs {
        let cell_info = cells(None).unwrap();
        ArtifactFs::new(
            BuckPathResolver::new(cell_info.1),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
                "buck-out/v2".to_owned(),
            )),
            ProjectFilesystem::new(AbsPathBuf::try_from(std::env::current_dir().unwrap()).unwrap()),
        )
    }

    fn get_command_line(value: Value) -> anyhow::Result<Vec<String>> {
        let fs = artifact_fs();
        let mut builder = BaseCommandLineBuilder::new(&fs);

        match value.as_command_line() {
            Some(v) => v.add_to_command_line(&mut builder),
            None => value
                .as_command_line_err()?
                .add_to_command_line(&mut builder),
        }?;
        Ok(builder.build())
    }

    #[starlark_module]
    pub fn command_line_stringifier(builder: &mut GlobalsBuilder) {
        fn get_args<'v>(value: Value<'v>) -> anyhow::Result<Vec<String>> {
            get_command_line(value)
        }

        fn stringify_cli_arg<'v>(value: Value<'v>) -> anyhow::Result<String> {
            let fs = artifact_fs();
            let mut builder = BaseCommandLineBuilder::new(&fs);
            value
                .as_command_line_err()?
                .add_to_command_line(&mut builder)?;
            let cli = builder.build();
            assert_eq!(1, cli.len());
            Ok(cli.get(0).unwrap().clone())
        }

        fn short_name<'v>(value: Value<'v>) -> anyhow::Result<String> {
            Ok(StarlarkCommandLine::try_from_value(value)?.short_name())
        }
    }

    #[starlark_module]
    pub fn inputs_helper(builder: &mut GlobalsBuilder) {
        fn make_inputs<'v>(values: Vec<Value<'v>>) -> anyhow::Result<StarlarkCommandLineInputs> {
            let mut visitor = SimpleCommandLineArtifactVisitor::new();
            for v in values {
                let cli = v.as_command_line_err()?;
                cli.visit_artifacts(&mut visitor)?;
            }

            Ok(StarlarkCommandLineInputs {
                inputs: visitor.inputs,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::result::SharedResult;
    use indoc::indoc;

    use super::tester;
    use crate::interpreter::{
        rule_defs::{artifact::testing::artifactory, label::testing::label_creator},
        testing::{expect_error, import, Tester},
    };

    fn tester() -> anyhow::Result<Tester> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(|builder| {
            tester::command_line_stringifier(builder);
            tester::inputs_helper(builder);
            artifactory(builder);
            label_creator(builder);
        }));
        Ok(tester)
    }

    #[test]
    fn stringifies_correctly() -> SharedResult<()> {
        let mut tester = tester()?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            artifact1 = source_artifact("foo", "bar/baz.h")
            artifact2 = bound_artifact("//:dep1", "dir/baz.h")
            arg2 = "string1"
            arg3 = artifact1
            arg4 = artifact2
            arg5 = str(label("//foo:bar[baz]"))

            def test():
                artifact3 = source_artifact("foo", "bar/quz.h")
                artifact4 = bound_artifact("//:dep2", "dir/quz.h")
                arg7 = "string2"
                arg8 = artifact3
                arg9 = artifact4
                arg10 = str(label("//foo:bar[quz]"))

                assert_eq("string1", stringify_cli_arg(arg2))
                assert_eq("foo/bar/baz.h", stringify_cli_arg(arg3))
                assert_eq_ignore_hash("buck-out/v2/gen/root/<HASH>/__dep1__/dir/baz.h", stringify_cli_arg(arg4))
                assert_eq("root//foo:bar[baz] (<testing>)", stringify_cli_arg(arg5))
                assert_eq("string2", stringify_cli_arg(arg7))
                assert_eq("foo/bar/quz.h", stringify_cli_arg(arg8))
                assert_eq_ignore_hash("buck-out/v2/gen/root/<HASH>/__dep2__/dir/quz.h", stringify_cli_arg(arg9))
                assert_eq("root//foo:bar[quz] (<testing>)", stringify_cli_arg(arg10))
            "#
        ))?;

        let contents = indoc!(
            r#"
            def test():
                arg = stringify_cli_arg(["list of strings aren't valid"])
            "#
        );

        expect_error(
            tester.run_starlark_bzl_test(contents),
            contents,
            "expected command line item to be a string",
        );

        Ok(())
    }

    #[test]
    fn displays_correctly() -> SharedResult<()> {
        let mut tester = tester()?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            def test():
                cli = cmd_args()
                cli.add("foo")
                cli.hidden("bar")
                assert_eq('cmd_args("foo", hidden = ["bar"])', str(cli))
            "#
        ))?;

        Ok(())
    }

    #[test]
    fn command_line_builder() -> SharedResult<()> {
        let mut tester = tester()?;
        let content = indoc!(
            r#"
            b1 = cmd_args()
            b2 = cmd_args()
            b3 = cmd_args()
            b4 = cmd_args()
            b5 = cmd_args()

            b2.add("b2 s1").add(["b2 s2"])
            b3.add("b3 s1").add(["b3 s2"])

            a1 = source_artifact("foo", "bar1/baz.h")
            a2 = bound_artifact("//:dep1", "dir/baz.h")
            a3 = source_artifact("foo", "bar2/baz.h")
            a4 = bound_artifact("//:dep2", "dir/baz.h")
            l1 = str(label("//foo:bar[baz]"))

            b1.add("b1 s1", "b1 s2").add(b2).add(a1).add(a2)
            b1.add([b3, l1]).add([a3, a4])

            # Add something so we can verify it doesn't modify b1
            b2 = b2.copy()
            b2.add("not in b1")

            # Ensure that both lists, and *args formats work
            b4.add_joined(["--foo=", a1, ",bar=", b3, ",baz"])
            b4.add_joined("--foo=", a1, ",bar=", b3, ",baz")

            # Make sure delimiters work properly
            b5.add_joined("foo")
            b5.add_joined("foo", "bar", delimiter=",")
            b5.add_joined("foo", "bar", "baz", delimiter=",")

            mutable_b1_args = get_args(b1)
            mutable_b2_args = get_args(b2)
            mutable_b3_args = get_args(b3)
            mutable_b4_args = get_args(b4)
            mutable_b5_args = get_args(b5)

            def test():
                b1_args_expected = [
                    "b1 s1",
                    "b1 s2",
                    "b2 s1",
                    "b2 s2",
                    "foo/bar1/baz.h",
                    "buck-out/v2/gen/root/<HASH>/__dep1__/dir/baz.h",
                    "b3 s1",
                    "b3 s2",
                    "root//foo:bar[baz] (<testing>)",
                    "foo/bar2/baz.h",
                    "buck-out/v2/gen/root/<HASH>/__dep2__/dir/baz.h",
                ]
                b2_args_expected = [
                    "b2 s1",
                    "b2 s2",
                    "not in b1",
                ]
                b3_args_expected = [
                    "b3 s1",
                    "b3 s2",
                ]
                b4_args_expected = [
                    "--foo=foo/bar1/baz.h,bar=b3 s1b3 s2,baz",
                    "--foo=foo/bar1/baz.h,bar=b3 s1b3 s2,baz",
                ]
                b5_args_expected = [
                    "foo",
                    "foo,bar",
                    "foo,bar,baz",
                ]

                # Check values while builders were mutable
                assert_eq_ignore_hash(b1_args_expected, mutable_b1_args)

                assert_eq_ignore_hash(b2_args_expected, mutable_b2_args)

                assert_eq_ignore_hash(b3_args_expected, mutable_b3_args)

                assert_eq_ignore_hash(b4_args_expected, mutable_b4_args)

                assert_eq_ignore_hash(b5_args_expected, mutable_b5_args)

                # Check that frozen values still work
                frozen_b1_args = get_args(b1)
                frozen_b2_args = get_args(b2)
                frozen_b3_args = get_args(b3)
                frozen_b4_args = get_args(b4)
                frozen_b5_args = get_args(b5)

                assert_eq_ignore_hash(b1_args_expected, frozen_b1_args)

                assert_eq_ignore_hash(b2_args_expected, frozen_b2_args)

                assert_eq_ignore_hash(b3_args_expected, frozen_b3_args)

                assert_eq_ignore_hash(b4_args_expected, frozen_b4_args)

                assert_eq_ignore_hash(b5_args_expected, frozen_b5_args)

                # Make sure we can add frozen CLIs to unfrozen ones
                b6 = cmd_args()
                b6.add("b6").add(b2).add([b3])

                b6_args_expected = ["b6"] + b2_args_expected + b3_args_expected
                assert_eq(b6_args_expected, get_args(b6))
            "#
        );

        tester.run_starlark_bzl_test(content)?;

        let content_invalid_type_1 = r#"cmd_args().add({"not": "an arg"})"#;
        // TODO(ndmitchel): We claim you can add labels to a command line, but you can't
        let content_invalid_type_2 = r#"cmd_args().add(label("//:foo"))"#;
        let content_invalid_type_3 = r#"cmd_args().add([{"not": "an arg"}])"#;

        expect_error(
            tester.run_starlark_bzl_test(content_invalid_type_1),
            content_invalid_type_1,
            "expected command line item",
        );
        expect_error(
            tester.run_starlark_bzl_test(content_invalid_type_2),
            content_invalid_type_2,
            "expected command line item",
        );
        expect_error(
            tester.run_starlark_bzl_test(content_invalid_type_3),
            content_invalid_type_3,
            "expected command line item",
        );

        Ok(())
    }

    #[test]
    fn test_short_name() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                assert_eq("run foo", short_name(["foo", "bar"]))
                assert_eq("run command", short_name([]))
                c1 = cmd_args()
                c1.add("foo", "bar")
                c2 = cmd_args()
                c2.add(c1, "baz")
                assert_eq("run foo", short_name([c1, c2]))
            "#
        );

        tester.run_starlark_bzl_test(contents)?;
        Ok(())
    }

    #[test]
    fn test_relative_absolute() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add(source_artifact("foo","bar/baz/qux.h"))
                args.relative_to(source_artifact("foo", "bar/foo"))
                args.absolute_prefix("$ABSOLUTE/")
                assert_eq(get_args(args), ["$ABSOLUTE/../baz/qux.h"])

                args.absolute_suffix("!")
                assert_eq(get_args(args), ["$ABSOLUTE/../baz/qux.h!"])

                args = cmd_args()
                args.add(source_artifact("foo","bar/baz/qux.h"))
                args.relative_to(source_artifact("foo", "bar/baz"), parent=1)
                assert_eq(get_args(args), ["baz/qux.h"])
                "#
        );
        tester.run_starlark_bzl_test(contents)?;
        Ok(())
    }

    #[test]
    fn test_parent() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add(source_artifact("foo","bar/baz/qux.h"))
                args.parent().absolute_suffix("!")
                assert_eq(get_args(args), ["foo/bar/baz!"])
            "#
        );
        tester.run_starlark_bzl_test(contents)?;

        let too_many_parent_calls = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add(source_artifact("foo","qux.h"))
                args.parent().parent().parent()
                get_args(args)
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(too_many_parent_calls),
            too_many_parent_calls,
            "too many .parent() calls",
        );

        Ok(())
    }

    #[test]
    fn test_parent_n() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add(source_artifact("foo","bar/baz/qux.h"))
                args.parent(2).absolute_suffix("!")
                assert_eq(get_args(args), ["foo/bar!"])
            "#
        );
        tester.run_starlark_bzl_test(contents)?;

        let too_many_parent_calls = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add(source_artifact("foo","qux.h"))
                args.parent(3)
                get_args(args)
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(too_many_parent_calls),
            too_many_parent_calls,
            "too many .parent() calls",
        );

        let bad_count = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add(source_artifact("foo","qux.h"))
                args.parent(-12)
                get_args(args)
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(bad_count),
            bad_count,
            "Type of parameter `count` doesn't match",
        );

        Ok(())
    }

    #[test]
    fn test_format() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                args1 = cmd_args(joined=True, delimiter="", format="format-{}-{}-string")
                args2 = cmd_args(joined=False, format="format-{}-{}-string")
                args3 = cmd_args()

                args1.add("foo")
                args1.add(",bar")
                args2.add("foo")
                args2.add("bar")
                args3.add_joined(["foo", "bar"], format="format-{}-{}-string")
                args3.add_joined(["foo", "bar"], delimiter=",", format="format-{}-{}-string")
                args3.add("foo", format="format-{}-{}-string1")
                args3.add("bar", format="format-{}-{}-string2")

                assert_eq(["format-foo,bar-foo,bar-string"], get_args(args1))
                assert_eq(
                    [
                        "format-foo-foo-string",
                        "format-bar-bar-string",
                    ],
                    get_args(args2),
                )
                assert_eq(
                    [
                        "format-foobar-foobar-string",
                        "format-foo,bar-foo,bar-string",
                        "format-foo-foo-string1",
                        "format-bar-bar-string2",
                    ],
                    get_args(args3),
               )
            "#
        );
        tester.run_starlark_bzl_test(contents)?;
        Ok(())
    }

    #[test]
    fn test_joined_with_empty_args() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add_joined(["", "foo"], delimiter=",", format="format-{}-string")
                args.add_joined(["", "", "foo"], delimiter=",")
                assert_eq(
                    [
                        "format-,foo-string",
                        ",,foo",
                    ],
                    get_args(args),
               )
            "#
        );
        tester.run_starlark_bzl_test(contents)?;
        Ok(())
    }

    #[test]
    fn test_inputs_outputs() -> anyhow::Result<()> {
        let mut tester = tester()?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            artifact1 = source_artifact("foo", "bar/baz.h")
            artifact2 = bound_artifact("//:dep1", "dir/baz.h")

            def norm(xs):
                # Try and make everything consistent for the equality test
                return sorted([repr(x) for x in xs])

            def test():
                artifact3 = source_artifact("foo", "bar/quz.h")
                artifact4 = bound_artifact("//:dep2", "dir/quz.h")
                artifact5 = declared_artifact("declared")

                cli = cmd_args()
                cli.add(artifact3)
                cli.add("just a string")
                cli.add(artifact4)
                cli.hidden(artifact1)
                cli.add(artifact5.as_output())

                assert_eq(make_inputs([artifact3, artifact4, artifact1]), cli.inputs)
                assert_eq(3, len(cli.inputs))
                assert_eq(norm([artifact5.as_output()]), norm(cli.outputs))
            "#
        ))?;
        Ok(())
    }

    #[test]
    fn test_ignore_artifacts() -> anyhow::Result<()> {
        let mut tester = tester()?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            def norm(xs):
                # Try and make everything consistent for the equality test
                return sorted([repr(x) for x in xs])

            def test():
                artifact = bound_artifact("//:dep2", "dir/quz.h")

                cli = cmd_args()
                cli.add(artifact)
                cli.ignore_artifacts()

                assert_eq(make_inputs([]), cli.inputs)
                assert_eq([], cli.outputs)

                assert_eq_ignore_hash(["buck-out/v2/gen/root/<HASH>/__dep2__/dir/quz.h"], get_args(cli))
            "#
        ))?;
        Ok(())
    }

    #[test]
    fn test_frozen_inputs_outputs() -> anyhow::Result<()> {
        let mut tester = tester()?;

        tester.add_import(
            &import("root", "test", "def1.bzl"),
            indoc!(
                r#"
                def norm(xs):
                    # Try and make everything consistent for the equality test
                    return sorted([repr(x) for x in xs])

                input = source_artifact("foo", "bar/quz.h")
                output = declared_bound_artifact("//foo:bar", "declared")

                cli = cmd_args()
                cli.add("string")
                cli.add(input)
                cli.add(output.as_output())

                expected_outputs = norm([output.as_output()])
                expected_inputs = make_inputs([input])
                "#
            ),
        )?;

        tester.run_starlark_bzl_test(indoc!(
            r#"
            load("//test:def1.bzl", "cli", "expected_inputs", "expected_outputs", "norm")

            def test():
                assert_eq(expected_inputs, cli.inputs)
                assert_eq(expected_outputs, norm(cli.outputs))
            "#
        ))?;
        Ok(())
    }

    #[test]
    fn test_quote_style_shell() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add("$HELLO", quote = "shell")
                assert_eq(get_args(args), ["\"\\$HELLO\""])

                args = cmd_args()
                args.add(source_artifact("foo", "bar$qux.h"), quote = "shell")
                assert_eq(get_args(args), ["\"foo/bar\\$qux.h\""])
                "#
        );
        tester.run_starlark_bzl_test(contents)?;
        Ok(())
    }

    #[test]
    fn test_prepend() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add([["foo", "bar"], "baz"])
                assert_eq(["foo", "bar", "baz"], get_args(args))
                "#
        );
        tester.run_starlark_bzl_test(contents)?;
        Ok(())
    }

    #[test]
    fn test_list_list() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                args = cmd_args()
                args.add(["foo", "bar"], prepend = "-X")
                assert_eq(["-X", "foo", "-X", "bar"], get_args(args))
                "#
        );
        tester.run_starlark_bzl_test(contents)?;
        Ok(())
    }

    #[test]
    fn test_concat() -> anyhow::Result<()> {
        let mut tester = tester()?;
        let contents = indoc!(
            r#"
            def test():
                args = cmd_args("foo", "bar", delimiter = "-")
                assert_eq(["foo-bar"], get_args(args))
                "#
        );
        tester.run_starlark_bzl_test(contents)?;
        Ok(())
    }
}
