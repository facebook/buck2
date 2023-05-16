/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_interpreter::types::cell_root::CellRoot;
use buck2_util::commas::commas;
use buck2_util::thin_box::ThinBoxSlice;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::*;
use regex::Regex;
use serde::Serialize;
use serde::Serializer;
use starlark::coerce::coerce;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValue;
use starlark::values::StringValue;
use starlark::values::StringValueLike;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use static_assertions::assert_eq_size;

use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;

/// Supported ways of quoting arguments.
#[derive(Debug, Clone, Dupe, Trace, Freeze, Serialize, Allocative)]
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

#[derive(Debug, thiserror::Error)]
enum CommandLineArgError {
    #[error("Unknown quoting style `{0}`")]
    UnknownQuotingStyle(String),
    #[error("too many .parent() calls")]
    TooManyParentCalls,
}

impl QuoteStyle {
    pub fn parse(s: &str) -> anyhow::Result<QuoteStyle> {
        match s {
            "shell" => Ok(QuoteStyle::Shell),
            _ => Err(anyhow::anyhow!(CommandLineArgError::UnknownQuotingStyle(
                s.to_owned()
            ))),
        }
    }
}

pub(crate) trait CommandLineOptionsTrait<'v>: Display {
    fn ignore_artifacts(&self) -> bool;
    fn delimiter(&self) -> Option<StringValue<'v>>;

    fn to_command_line_options<'a>(&'a self) -> CommandLineOptionsRef<'v, 'a>;
}

#[derive(Debug, Default_, Clone, Trace, Allocative)]
#[repr(C)]
pub(crate) struct CommandLineOptions<'v> {
    // These impact how artifacts are rendered
    /// The value of V must be convertible to a `RelativeOrigin`
    pub(crate) relative_to: Option<(Value<'v>, usize)>,
    pub(crate) absolute_prefix: Option<StringValue<'v>>,
    pub(crate) absolute_suffix: Option<StringValue<'v>>,
    pub(crate) parent: u32,
    pub(crate) ignore_artifacts: bool,

    // These impact the formatting of each string
    pub(crate) delimiter: Option<StringValue<'v>>,
    pub(crate) format: Option<StringValue<'v>>,
    pub(crate) prepend: Option<StringValue<'v>>,
    pub(crate) quote: Option<QuoteStyle>,
    #[allow(clippy::box_collection)]
    pub(crate) replacements: Option<Box<Vec<(StringValue<'v>, StringValue<'v>)>>>,
}

#[derive(Default, Serialize)]
pub(crate) struct CommandLineOptionsRef<'v, 'a> {
    #[serde(serialize_with = "serialize_opt_display")]
    pub(crate) relative_to: Option<(Value<'v>, usize)>,
    pub(crate) absolute_prefix: Option<StringValue<'v>>,
    pub(crate) absolute_suffix: Option<StringValue<'v>>,
    pub(crate) parent: u32,
    pub(crate) ignore_artifacts: bool,

    pub(crate) delimiter: Option<StringValue<'v>>,
    pub(crate) format: Option<StringValue<'v>>,
    pub(crate) prepend: Option<StringValue<'v>>,
    pub(crate) quote: Option<QuoteStyle>,
    pub(crate) replacements: &'a [(StringValue<'v>, StringValue<'v>)],
}

impl<'v, 'a> CommandLineOptionsRef<'v, 'a> {
    pub(crate) fn to_owned(&self) -> CommandLineOptions<'v> {
        CommandLineOptions {
            relative_to: self.relative_to,
            absolute_prefix: self.absolute_prefix,
            absolute_suffix: self.absolute_suffix,
            parent: self.parent,
            ignore_artifacts: self.ignore_artifacts,
            delimiter: self.delimiter,
            format: self.format,
            prepend: self.prepend,
            quote: self.quote.dupe(),
            replacements: if self.replacements.is_empty() {
                None
            } else {
                Some(Box::new(self.replacements.to_vec()))
            },
        }
    }
}

impl<'v> CommandLineOptionsTrait<'v> for CommandLineOptions<'v> {
    fn ignore_artifacts(&self) -> bool {
        self.ignore_artifacts
    }

    fn delimiter(&self) -> Option<StringValue<'v>> {
        self.delimiter.dupe()
    }

    fn to_command_line_options<'a>(&'a self) -> CommandLineOptionsRef<'v, 'a> {
        CommandLineOptionsRef {
            relative_to: self.relative_to,
            absolute_prefix: self.absolute_prefix,
            absolute_suffix: self.absolute_suffix,
            parent: self.parent,
            ignore_artifacts: self.ignore_artifacts,
            delimiter: self.delimiter,
            format: self.format,
            prepend: self.prepend,
            quote: self.quote.dupe(),
            replacements: match &self.replacements {
                None => &[],
                Some(v) => v.as_slice(),
            },
        }
    }
}

#[derive(Debug, Allocative)]
enum FrozenCommandLineOption {
    RelativeTo(FrozenValue, u32),
    AbsolutePrefix(FrozenStringValue),
    AbsoluteSuffix(FrozenStringValue),
    Parent(u32),
    IgnoreArtifacts,
    Delimiter(FrozenStringValue),
    Format(FrozenStringValue),
    Prepend(FrozenStringValue),
    Quote(QuoteStyle),
    #[allow(clippy::box_collection)]
    Replacements(ThinBoxSlice<(FrozenStringValue, FrozenStringValue)>),
}

assert_eq_size!(FrozenCommandLineOption, [usize; 2]);

#[derive(Debug, Default, Allocative)]
pub(crate) struct FrozenCommandLineOptions {
    options: ThinBoxSlice<FrozenCommandLineOption>,
}

impl FrozenCommandLineOptions {
    pub(crate) fn is_empty(&self) -> bool {
        self.options.is_empty()
    }
}

impl<'v> CommandLineOptionsTrait<'v> for FrozenCommandLineOptions {
    fn ignore_artifacts(&self) -> bool {
        for option in self.options.iter() {
            if let FrozenCommandLineOption::IgnoreArtifacts = option {
                return true;
            }
        }
        false
    }

    fn delimiter(&self) -> Option<StringValue<'v>> {
        for option in self.options.iter() {
            if let FrozenCommandLineOption::Delimiter(value) = option {
                return Some(value.to_string_value());
            }
        }
        None
    }

    fn to_command_line_options<'a>(&'a self) -> CommandLineOptionsRef<'v, 'a> {
        let mut options = CommandLineOptionsRef::default();
        for option in &*self.options {
            match option {
                FrozenCommandLineOption::RelativeTo(value, parent) => {
                    options.relative_to = Some((value.to_value(), *parent as usize));
                }
                FrozenCommandLineOption::AbsolutePrefix(value) => {
                    options.absolute_prefix = Some(value.to_string_value());
                }
                FrozenCommandLineOption::AbsoluteSuffix(value) => {
                    options.absolute_suffix = Some(value.to_string_value());
                }
                FrozenCommandLineOption::Parent(parent) => {
                    options.parent = *parent;
                }
                FrozenCommandLineOption::IgnoreArtifacts => {
                    options.ignore_artifacts = true;
                }
                FrozenCommandLineOption::Delimiter(value) => {
                    options.delimiter = Some(value.to_string_value());
                }
                FrozenCommandLineOption::Format(value) => {
                    options.format = Some(value.to_string_value());
                }
                FrozenCommandLineOption::Prepend(value) => {
                    options.prepend = Some(value.to_string_value());
                }
                FrozenCommandLineOption::Quote(value) => {
                    options.quote = Some(value.dupe());
                }
                FrozenCommandLineOption::Replacements(value) => {
                    options.replacements = coerce::<&[_], &[_]>(value);
                }
            }
        }
        options
    }
}

impl<'v> Serialize for CommandLineOptions<'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.to_command_line_options().serialize(serializer)
    }
}

impl Serialize for FrozenCommandLineOptions {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.to_command_line_options().serialize(serializer)
    }
}

impl<'v> Display for CommandLineOptions<'v> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.to_command_line_options(), f)
    }
}

impl Display for FrozenCommandLineOptions {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.to_command_line_options(), f)
    }
}

impl<'v> Freeze for CommandLineOptions<'v> {
    type Frozen = FrozenCommandLineOptions;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<FrozenCommandLineOptions> {
        let CommandLineOptions {
            relative_to,
            absolute_prefix,
            absolute_suffix,
            parent,
            ignore_artifacts,
            delimiter,
            format,
            prepend,
            quote,
            replacements,
        } = self;

        let mut options = Vec::new();
        if let Some(relative_to) = relative_to {
            let (relative, parent) = relative_to.freeze(freezer)?;
            let parent: u32 = parent.try_into()?;
            options.push(FrozenCommandLineOption::RelativeTo(relative, parent));
        }
        if let Some(absolute_prefix) = absolute_prefix {
            let absolute_prefix = absolute_prefix.freeze(freezer)?;
            options.push(FrozenCommandLineOption::AbsolutePrefix(absolute_prefix));
        }
        if let Some(absolute_suffix) = absolute_suffix {
            let absolute_suffix = absolute_suffix.freeze(freezer)?;
            options.push(FrozenCommandLineOption::AbsoluteSuffix(absolute_suffix));
        }
        if parent != 0 {
            options.push(FrozenCommandLineOption::Parent(parent));
        }
        if ignore_artifacts {
            options.push(FrozenCommandLineOption::IgnoreArtifacts);
        }
        if let Some(delimiter) = delimiter {
            let delimiter = delimiter.freeze(freezer)?;
            options.push(FrozenCommandLineOption::Delimiter(delimiter));
        }
        if let Some(format) = format {
            let format = format.freeze(freezer)?;
            options.push(FrozenCommandLineOption::Format(format));
        }
        if let Some(prepend) = prepend {
            let prepend = prepend.freeze(freezer)?;
            options.push(FrozenCommandLineOption::Prepend(prepend));
        }
        if let Some(quote) = quote {
            options.push(FrozenCommandLineOption::Quote(quote));
        }
        if let Some(replacements) = replacements {
            if !replacements.is_empty() {
                let replacements = ThinBoxSlice::from_iter((*replacements).freeze(freezer)?);
                options.push(FrozenCommandLineOption::Replacements(replacements));
            }
        }

        Ok(FrozenCommandLineOptions {
            options: ThinBoxSlice::from_iter(options),
        })
    }
}

fn serialize_opt_display<V: Display, S>(v: &Option<(V, usize)>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match v {
        Some((v, u)) => s.serialize_some(&(format!("{}", v), u)),
        None => s.serialize_none(),
    }
}

impl<'v, 'a> Display for CommandLineOptionsRef<'v, 'a> {
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
            write!(f, "absolute_suffix = {}", v)?;
        }
        if self.parent != 0 {
            comma(f)?;
            write!(f, "parent = {}", self.parent)?;
        }
        if self.ignore_artifacts {
            comma(f)?;
            write!(f, "ignore_artifacts = True")?;
        }
        if let Some(v) = &self.delimiter {
            comma(f)?;
            write!(f, "delimiter = {:?}", v)?;
        }
        if let Some(v) = &self.format {
            comma(f)?;
            write!(f, "format = {:?}", v)?;
        }
        if let Some(v) = &self.prepend {
            comma(f)?;
            write!(f, "prepend = {:?}", v)?;
        }
        if let Some(v) = &self.quote {
            comma(f)?;
            write!(f, "quote = \"{}\"", v)?;
        }
        if !self.replacements.is_empty() {
            comma(f)?;
            write!(f, "replacements = [")?;
            let mut vec_comma = commas();
            for p in self.replacements {
                vec_comma(f)?;
                write!(f, "({:?}, {:?})", p.0, p.1)?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

// NOTE: This is an enum as opposed to a trait because of the `C` parameter on (which is required
// because upcasting is not stable).
#[derive(Display)]
pub(crate) enum RelativeOrigin<'v> {
    Artifact(&'v dyn StarlarkArtifactLike),
    CellRoot(&'v CellRoot),
}

impl<'v> RelativeOrigin<'v> {
    pub(crate) fn from_value<V>(v: V) -> Option<Self>
    where
        V: ValueLike<'v>,
    {
        if let Some(v) = v.as_artifact() {
            return Some(RelativeOrigin::Artifact(v));
        }

        if let Some(v) = v.downcast_ref::<CellRoot>() {
            return Some(RelativeOrigin::CellRoot(v));
        }

        None
    }

    pub(crate) fn resolve<C>(&self, ctx: &C) -> anyhow::Result<RelativePathBuf>
    where
        C: CommandLineContext + ?Sized,
    {
        let loc = match self {
            Self::Artifact(artifact) => {
                // Shame we require the artifact to be bound here, we really just needs its
                // path even if it is unbound.
                let artifact = artifact.get_bound_artifact()?;
                ctx.resolve_artifact(&artifact)?
            }
            Self::CellRoot(cell_root) => ctx.resolve_cell_path(cell_root.cell_path())?,
        };

        Ok(loc.into_relative())
    }
}

impl<'v, 'x> CommandLineOptionsRef<'v, 'x> {
    fn changes_builder(&self) -> bool {
        match self {
            Self {
                relative_to: None,
                absolute_prefix: None,
                absolute_suffix: None,
                parent: 0,
                delimiter: None,
                format: None,
                prepend: None,
                quote: None,
                replacements: &[],
                ignore_artifacts: _, // Doesn't impact the builder
            } => false,
            _ => true,
        }
    }

    pub(crate) fn wrap_builder<'a, R>(
        &self,
        builder: &'a mut dyn CommandLineBuilder,
        ctx: &'a mut dyn CommandLineContext,
        f: impl for<'b> FnOnce(
            &'b mut dyn CommandLineBuilder,
            &'b mut dyn CommandLineContext,
        ) -> anyhow::Result<R>,
    ) -> anyhow::Result<R> {
        struct ExtrasBuilder<'a, 'v> {
            builder: &'a mut dyn CommandLineBuilder,
            opts: &'a CommandLineOptionsRef<'v, 'a>,
            // Auxiliary field to store concatenation result (when arguments are concatenated) and
            // a flag stating that the result is not yet started to be computated (i.e. the first
            // argument to be concatenated is not yet processed).
            concatenation_context: Option<(String, bool)>,
        }

        struct ExtrasContext<'a, 'v> {
            ctx: &'a mut dyn CommandLineContext,
            opts: &'a CommandLineOptionsRef<'v, 'a>,
            relative_to: Option<RelativePathBuf>,
        }

        impl<'a, 'v> CommandLineContext for ExtrasContext<'a, 'v> {
            fn resolve_project_path(
                &self,
                path: ProjectRelativePathBuf,
            ) -> anyhow::Result<CommandLineLocation> {
                let Self {
                    ctx,
                    relative_to,
                    opts,
                } = self;

                let resolved = ctx.resolve_project_path(path)?;

                if opts.parent == 0
                    && opts.absolute_prefix.is_none()
                    && opts.absolute_suffix.is_none()
                    && relative_to.is_none()
                {
                    return Ok(resolved);
                }

                let mut x = resolved.into_relative();
                if let Some(relative_to) = relative_to {
                    x = relative_to.relative(x);
                }
                let mut parent_ref = x.as_relative_path();
                for _ in 0..opts.parent {
                    parent_ref = parent_ref
                        .parent()
                        .ok_or(CommandLineArgError::TooManyParentCalls)?;
                }
                x = parent_ref.to_owned();
                if opts.absolute_prefix.is_some() || opts.absolute_suffix.is_some() {
                    x = RelativePath::new(&format!(
                        "{}{}{}",
                        opts.absolute_prefix.unwrap_or_default().as_str(),
                        x,
                        opts.absolute_suffix.unwrap_or_default().as_str(),
                    ))
                    .to_owned();
                }
                Ok(CommandLineLocation::from_relative_path(
                    x,
                    self.fs().path_separator(),
                ))
            }

            fn fs(&self) -> &ExecutorFs {
                self.ctx.fs()
            }

            fn next_macro_file_path(&mut self) -> anyhow::Result<RelativePathBuf> {
                let macro_path = self.ctx.next_macro_file_path()?;
                if let Some(relative_to_path) = &self.relative_to {
                    Ok(relative_to_path.relative(macro_path))
                } else {
                    Ok(macro_path)
                }
            }
        }

        impl<'a, 'v> ExtrasBuilder<'a, 'v> {
            /// If any items need to be concatted/formatted and added to the original CLI,
            /// do it here
            fn finalize_args(mut self) -> Self {
                if let Some((concatted_items, _)) = self.concatenation_context.take() {
                    self.builder.push_arg(concatted_items);
                }
                self
            }

            fn add_delimiter(&mut self) {
                if let Some((concatted_items, initital_state)) = self.concatenation_context.as_mut()
                {
                    if *initital_state {
                        *initital_state = false;
                    } else {
                        concatted_items.push_str(self.opts.delimiter.unwrap_or_default().as_str());
                    }
                }
            }

            fn add_arg(&mut self, arg: String) {
                if let Some((concatted_items, _)) = self.concatenation_context.as_mut() {
                    concatted_items.push_str(&arg)
                } else {
                    self.builder.push_arg(arg)
                }
            }

            fn format(&self, mut arg: String) -> String {
                for (pattern, replacement) in self.opts.replacements {
                    // We checked that regex is valid in replace_regex(), so unwrap is safe.
                    let re = Regex::new(pattern.as_str()).unwrap();
                    match re.replace_all(&arg, replacement.as_str()) {
                        Cow::Borrowed(_) => {}
                        Cow::Owned(new) => arg = new,
                    }
                }
                if let Some(format) = &self.opts.format {
                    arg = format.as_str().replace("{}", &arg);
                }
                match &self.opts.quote {
                    Some(QuoteStyle::Shell) => {
                        arg = shlex::quote(&arg).into_owned();
                    }
                    _ => {}
                }
                arg
            }
        }

        impl<'a, 'v> CommandLineBuilder for ExtrasBuilder<'a, 'v> {
            fn push_arg(&mut self, s: String) {
                // We apply options impacting formatting in the order:
                //   format, quote, (prepend + delimiter)
                self.add_delimiter();
                if let Some(i) = self.opts.prepend {
                    self.add_arg(i.as_str().to_owned());
                }
                self.add_arg(self.format(s))
            }
        }

        if !self.changes_builder() {
            f(builder, ctx)
        } else {
            let relative_to = self.relative_to_path(ctx)?;

            let mut extras_builder = ExtrasBuilder {
                builder,
                opts: self,
                concatenation_context: if self.delimiter.is_some() {
                    Some((String::new(), true))
                } else {
                    None
                },
            };

            let mut extras_ctx = ExtrasContext {
                ctx,
                opts: self,
                relative_to,
            };

            let res = f(&mut extras_builder, &mut extras_ctx)?;
            extras_builder.finalize_args();
            Ok(res)
        }
    }

    pub(crate) fn relative_to_path<C>(&self, ctx: &C) -> anyhow::Result<Option<RelativePathBuf>>
    where
        C: CommandLineContext + ?Sized,
    {
        let (value, parent) = match self.relative_to {
            Some(vp) => vp,
            None => return Ok(None),
        };

        let origin = RelativeOrigin::from_value(value)
            .expect("Must be a valid RelativeOrigin as this was checked in the setter");
        let mut relative_path = origin.resolve(ctx)?;
        for _ in 0..parent {
            if !relative_path.pop() {
                return Err(
                    anyhow::anyhow!(CommandLineArgError::TooManyParentCalls).context(format!(
                        "Error accessing {}-th parent of {}",
                        parent, origin
                    )),
                );
            }
        }

        Ok(Some(relative_path))
    }
}
