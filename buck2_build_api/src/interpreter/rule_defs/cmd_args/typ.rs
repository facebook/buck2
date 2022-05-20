/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    cell::RefCell,
    convert::TryInto,
    fmt::{self, Debug, Display},
};

use anyhow::anyhow;
use buck2_core::fs::{
    paths::{RelativePath, RelativePathBuf},
    project::ProjectRelativePathBuf,
};
use derive_more::Display;
use gazebo::{
    any::AnyLifetime,
    cell::ARef,
    coerce::{coerce_ref, Coerce},
    prelude::*,
};
use indexmap::IndexSet;
use serde::{Serialize, Serializer};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    starlark_type,
    values::{
        list::List, Freeze, Freezer, FrozenValue, NoSerialize, StarlarkValue, Trace, Value,
        ValueError, ValueLike,
    },
};
use static_assertions::assert_eq_size;

use crate::{
    actions::artifact::ArtifactFs,
    artifact_groups::ArtifactGroup,
    interpreter::rule_defs::{
        artifact::{StarlarkArtifactLike, StarlarkOutputArtifact, ValueAsArtifactLike},
        cmd_args::{
            traits::{
                CommandLineArgLike, CommandLineArtifactVisitor, CommandLineBuilder,
                CommandLineBuilderContext, CommandLineLocation, SimpleCommandLineArtifactVisitor,
                WriteToFileMacroVisitor,
            },
            ValueAsCommandLineLike,
        },
        test_cwd::TestCwd,
        util::commas,
    },
};

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

// These types show up a lot in the frozen heaps, so make sure they don't regress
assert_eq_size!(StarlarkCommandLineDataGen<FrozenValue>, [usize; 7]);
assert_eq_size!(CommandLineOptions<FrozenValue>, [usize; 20]);

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

impl<V> StarlarkCommandLineDataGen<V> {
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
#[serde(bound = "V : Serialize", transparent)]
pub struct StarlarkCommandLineGen<V>(V);

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

impl<'v, V: ValueLike<'v>> StarlarkCommandLineDataGen<V> {
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

    pub(crate) fn try_from_values_with_options(
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
        #[starlark(default = 0i32)] parent: i32,
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
    fn parent<'v>(
        this: Value<'v>,
        #[starlark(default = 1i32)] ref count: i32,
    ) -> anyhow::Result<Value<'v>> {
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
    pub(crate) inputs: IndexSet<ArtifactGroup>,
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
