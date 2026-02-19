/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::content_hash::ContentBasedPathHash;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_fs::paths::RelativePath;
use buck2_fs::paths::RelativePathBuf;
use buck2_interpreter::types::cell_root::CellRoot;
use buck2_interpreter::types::project_root::StarlarkProjectRoot;
use buck2_interpreter::types::regex::StarlarkBuckRegex;
use buck2_util::thin_box::ThinBoxSlice;
use derive_more::Display;
use display_container::fmt_container;
use dupe::Dupe;
use either::Either;
use gazebo::prelude::*;
use regex::Regex;
use serde::Serialize;
use serde::Serializer;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValueOfUnchecked;
use starlark::values::StringValue;
use starlark::values::StringValueLike;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTypedComplex;
use starlark::values::string::StarlarkStr;
use starlark::values::type_repr::StarlarkTypeRepr;
use static_assertions::assert_eq_size;

use crate::interpreter::rule_defs::artifact::starlark_artifact_like::StarlarkInputArtifactLike;
use crate::interpreter::rule_defs::artifact::starlark_output_artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::cmd_args::ArtifactPathMapper;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;
use crate::interpreter::rule_defs::cmd_args::regex::CmdArgsRegex;
use crate::interpreter::rule_defs::cmd_args::regex::FrozenCmdArgsRegex;
use crate::interpreter::rule_defs::cmd_args::shlex_quote::shlex_quote;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineContext;

/// Supported ways of quoting arguments.
#[derive(Debug, Clone, Copy, Dupe, Trace, Freeze, Serialize, Allocative)]
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

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum CommandLineArgError {
    #[error("Unknown quoting style `{0}`")]
    UnknownQuotingStyle(String),
    #[error("too many .parent() calls")]
    TooManyParentCalls,
}

impl QuoteStyle {
    pub fn parse(s: &str) -> buck2_error::Result<QuoteStyle> {
        match s {
            "shell" => Ok(QuoteStyle::Shell),
            _ => Err(CommandLineArgError::UnknownQuotingStyle(s.to_owned()).into()),
        }
    }
}

pub(crate) trait CommandLineOptionsTrait<'v> {
    fn ignore_artifacts(&self) -> bool;
    fn delimiter(&self) -> Option<StringValue<'v>>;

    fn to_command_line_options<'a>(&'a self) -> CommandLineOptionsRef<'v, 'a>;
}

#[derive(Debug, Default_, Clone, Trace, Allocative)]
#[repr(C)]
pub(crate) struct CommandLineOptions<'v> {
    // These impact how artifacts are rendered
    pub(crate) relative_to: Option<(ValueOfUnchecked<'v, RelativeOrigin<'v>>, u32)>,
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
    pub(crate) replacements: Option<Box<Vec<(CmdArgsRegex<'v>, StringValue<'v>)>>>,
}

#[derive(Clone, Copy, Dupe)]
pub(crate) enum OptionsReplacementsRef<'v, 'a> {
    Unfrozen(&'a [(CmdArgsRegex<'v>, StringValue<'v>)]),
    Frozen(&'a [(FrozenCmdArgsRegex, FrozenStringValue)]),
}

impl<'v, 'a> Display for OptionsReplacementsRef<'v, 'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let iter = self.iter();
        fmt_container(
            f,
            "[",
            "]",
            iter.map(|(r, s)| {
                struct D<'v>(CmdArgsRegex<'v>, StringValue<'v>);
                impl<'v> Display for D<'v> {
                    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                        write!(f, "({}, {})", StarlarkStr::repr(self.0.as_str()), self.1)
                    }
                }
                D(r, s)
            }),
        )
    }
}

impl<'v, 'a> OptionsReplacementsRef<'v, 'a> {
    pub(crate) fn is_empty(&self) -> bool {
        match self {
            Self::Unfrozen(v) => v.is_empty(),
            Self::Frozen(v) => v.is_empty(),
        }
    }

    pub(crate) fn iter(
        &self,
    ) -> impl ExactSizeIterator<Item = (CmdArgsRegex<'v>, StringValue<'v>)> + use<'v, 'a> {
        match self {
            Self::Unfrozen(v) => Either::Left(v.iter().copied()),
            Self::Frozen(v) => Either::Right(v.iter().map(|(r, s)| {
                (
                    match r {
                        FrozenCmdArgsRegex::Str(s) => CmdArgsRegex::Str(s.to_string_value()),
                        FrozenCmdArgsRegex::Regex(s) => CmdArgsRegex::Regex(s.to_value_typed()),
                    },
                    s.to_string_value(),
                )
            })),
        }
    }
}

impl<'v, 'a> Default for OptionsReplacementsRef<'v, 'a> {
    fn default() -> Self {
        Self::Frozen(&[])
    }
}

impl<'v, 'a> Serialize for OptionsReplacementsRef<'v, 'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Unfrozen(v) => v.serialize(serializer),
            Self::Frozen(v) => v.serialize(serializer),
        }
    }
}

#[derive(Default, Serialize)]
pub(crate) struct CommandLineOptionsRef<'v, 'a> {
    #[serde(serialize_with = "serialize_opt_display")]
    pub(crate) relative_to: Option<(ValueOfUnchecked<'v, RelativeOrigin<'v>>, u32)>,
    pub(crate) absolute_prefix: Option<StringValue<'v>>,
    pub(crate) absolute_suffix: Option<StringValue<'v>>,
    pub(crate) parent: u32,
    pub(crate) ignore_artifacts: bool,

    pub(crate) delimiter: Option<StringValue<'v>>,
    pub(crate) format: Option<StringValue<'v>>,
    pub(crate) prepend: Option<StringValue<'v>>,
    pub(crate) quote: Option<QuoteStyle>,
    pub(crate) replacements: OptionsReplacementsRef<'v, 'a>,
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
                Some(Box::new(self.replacements.iter().collect()))
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
                None => OptionsReplacementsRef::default(),
                Some(v) => OptionsReplacementsRef::Unfrozen(v.as_slice()),
            },
        }
    }
}

#[derive(Debug, Allocative)]
enum FrozenCommandLineOption {
    RelativeTo(
        FrozenValueOfUnchecked<'static, RelativeOrigin<'static>>,
        u32,
    ),
    AbsolutePrefix(FrozenStringValue),
    AbsoluteSuffix(FrozenStringValue),
    Parent(u32),
    IgnoreArtifacts,
    Delimiter(FrozenStringValue),
    Format(FrozenStringValue),
    Prepend(FrozenStringValue),
    Quote(QuoteStyle),
    #[allow(clippy::box_collection)]
    Replacements(ThinBoxSlice<(FrozenCmdArgsRegex, FrozenStringValue)>),
}

assert_eq_size!(FrozenCommandLineOption, [usize; 2]);

#[derive(Debug, Default, Allocative)]
pub(crate) struct FrozenCommandLineOptions {
    options: ThinBoxSlice<FrozenCommandLineOption>,
}

impl FrozenCommandLineOptions {
    pub const fn empty() -> Self {
        FrozenCommandLineOptions {
            options: ThinBoxSlice::empty(),
        }
    }

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
                    let value = ValueOfUnchecked::new(value.get().to_value());
                    options.relative_to = Some((value, *parent));
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
                    options.replacements = OptionsReplacementsRef::Frozen(value);
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

impl<'v> Freeze for CommandLineOptions<'v> {
    type Frozen = FrozenCommandLineOptions;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<FrozenCommandLineOptions> {
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
        if let Some((relative_to, parent)) = relative_to {
            let relative_to = relative_to.get().freeze(freezer)?;
            options.push(FrozenCommandLineOption::RelativeTo(
                FrozenValueOfUnchecked::new(relative_to),
                parent,
            ));
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

fn serialize_opt_display<V: Display, S>(v: &Option<(V, u32)>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match v {
        Some((v, u)) => s.serialize_some(&(format!("{v}"), u)),
        None => s.serialize_none(),
    }
}

// NOTE: This is an enum as opposed to a trait because of the `C` parameter on (which is required
// because upcasting is not stable).
#[derive(Display, StarlarkTypeRepr, UnpackValue)]
pub(crate) enum RelativeOrigin<'v> {
    OutputArtifact(ValueTypedComplex<'v, StarlarkOutputArtifact<'v>>),
    Artifact(&'v dyn StarlarkInputArtifactLike<'v>),
    CellRoot(&'v CellRoot),
    /// Bit of a useless variant since this is simply the default, but we allow it for consistency.
    ProjectRoot(&'v StarlarkProjectRoot),
}

// If we have the actual path of the artifact (e.g. because it is an input to the action), then we
// can use that to resolve the relative path. Otherwise, we use a constant value to resolve the path,
// which still works, since the "form" of the path is the same, i.e. "<target_package>/<content hash>/<short_name>".
// E.g.
// - "a/b/hash1/c".relative_to("a/b/hash2/d") => "../../hash1/c"
// - "a/b/hash1/c".relative_to("a/b/placeholder/d") => "../../hash1/c"
pub struct RelativeOriginArtifactPathMapper<'a> {
    artifact_path_mapping: &'a dyn ArtifactPathMapper,
    relative_path_resolution: ContentBasedPathHash,
}

impl<'a> RelativeOriginArtifactPathMapper<'a> {
    pub(crate) fn new(
        artifact_path_mapping: &'a dyn ArtifactPathMapper,
    ) -> RelativeOriginArtifactPathMapper<'a> {
        RelativeOriginArtifactPathMapper {
            artifact_path_mapping,
            relative_path_resolution: ContentBasedPathHash::RelativePathResolution,
        }
    }
}

impl ArtifactPathMapper for RelativeOriginArtifactPathMapper<'_> {
    fn get(&self, artifact: &Artifact) -> Option<&ContentBasedPathHash> {
        if let Some(value) = self.artifact_path_mapping.get(artifact) {
            Some(value)
        } else {
            Some(&self.relative_path_resolution)
        }
    }
}

impl<'v> RelativeOrigin<'v> {
    pub(crate) fn resolve<C>(
        &self,
        ctx: &C,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<RelativePathBuf>
    where
        C: CommandLineContext + ?Sized,
    {
        let loc = match self {
            Self::OutputArtifact(artifact) => {
                let value = match artifact.unpack() {
                    Either::Right(value) => value,
                    // FIXME(JakobDegen): This is not the only place where we do it, but it's
                    // nonetheless an extremely non-local assertion
                    Either::Left(_) => {
                        return Err(buck2_error::internal_error!(
                            "Non-frozen output artifacts can't be added to CLIs"
                        ));
                    }
                };
                ctx.resolve_output_artifact(&value.inner().artifact)?
            }
            Self::Artifact(artifact) => {
                // Shame we require the artifact to be bound here, we really just needs its
                // path even if it is unbound.
                let artifact = artifact.get_bound_artifact()?;
                let artifact_path_mapping =
                    RelativeOriginArtifactPathMapper::new(artifact_path_mapping);
                ctx.resolve_artifact(&artifact, &artifact_path_mapping)?
            }
            Self::CellRoot(cell_root) => ctx.resolve_cell_path(cell_root.cell_path())?,
            Self::ProjectRoot(_) => {
                ctx.resolve_project_path(ProjectRelativePath::empty().to_owned())?
            }
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
                replacements,
                ignore_artifacts: _, // Doesn't impact the builder
            } if replacements.is_empty() => false,
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
        ) -> buck2_error::Result<R>,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<R> {
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
            ) -> buck2_error::Result<CommandLineLocation<'_>> {
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

            fn fs(&self) -> &ExecutorFs<'_> {
                self.ctx.fs()
            }

            fn next_macro_file_path(&mut self) -> buck2_error::Result<RelativePathBuf> {
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
                for (pattern, replacement) in self.opts.replacements.iter() {
                    let re;
                    let re = match &pattern {
                        CmdArgsRegex::Str(pattern) => {
                            // We checked that regex is valid in replace_regex(), so unwrap is safe.
                            re = StarlarkBuckRegex::Regular(Regex::new(pattern.as_str()).unwrap());
                            &re
                        }
                        CmdArgsRegex::Regex(regex) => regex,
                    };
                    match re.replace_all(&arg, replacement.as_str()) {
                        Cow::Borrowed(new) if new == arg => {}
                        cow => arg = cow.into_owned(),
                    }
                }
                if let Some(format) = &self.opts.format {
                    arg = format.as_str().replace("{}", &arg);
                }
                if let Some(QuoteStyle::Shell) = &self.opts.quote {
                    let quoted = shlex_quote(&arg);
                    arg = quoted.into_owned();
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
            let relative_to = self.relative_to_path(ctx, artifact_path_mapping)?;

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

    pub(crate) fn relative_to_path<C>(
        &self,
        ctx: &C,
        artifact_path_mapping: &dyn ArtifactPathMapper,
    ) -> buck2_error::Result<Option<RelativePathBuf>>
    where
        C: CommandLineContext + ?Sized,
    {
        let (value, parent) = match self.relative_to {
            Some(vp) => vp,
            None => return Ok(None),
        };

        let origin = value
            .unpack()
            .internal_error("Must be a valid RelativeOrigin as this was checked in the setter")?;
        let mut relative_path = origin
            .resolve(ctx, artifact_path_mapping)
            .internal_error("origin::resolve")?;
        for _ in 0..parent {
            if !relative_path.pop() {
                return Err(CommandLineArgError::TooManyParentCalls)
                    .buck_error_context(format!("Error accessing {parent}-th parent of {origin}"));
            }
        }

        Ok(Some(relative_path))
    }

    pub(crate) fn iter_fields_display(
        &self,
    ) -> impl Iterator<Item = (&'static str, CommandLineOptionsIterItem<'v, 'x>)> + use<'v, 'x>
    {
        let CommandLineOptionsRef {
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

        // This can be implemented without allocation,
        // but generic version with iterator chain chain chain...
        // or either either either... leads to compilation of the crate slowing down
        // from 15s to minutes, and a fight against the borrow checker.
        let mut iter = Vec::new();

        if let Some((value, index)) = relative_to {
            iter.push((
                "relative_to",
                CommandLineOptionsIterItem::Value(value.get()),
            ));
            if *index != 0 {
                iter.push((
                    "relative_to_parent",
                    CommandLineOptionsIterItem::U32(*index),
                ));
            }
        }

        if let Some(value) = absolute_prefix {
            iter.push((
                "absolute_prefix",
                CommandLineOptionsIterItem::StringValue(*value),
            ));
        }
        if let Some(value) = absolute_suffix {
            iter.push((
                "absolute_suffix",
                CommandLineOptionsIterItem::StringValue(*value),
            ));
        }
        if *parent != 0 {
            iter.push(("parent", CommandLineOptionsIterItem::U32(*parent)));
        }
        if *ignore_artifacts {
            iter.push(("ignore_artifacts", CommandLineOptionsIterItem::Str("True")));
        }
        if let Some(value) = delimiter {
            iter.push(("delimiter", CommandLineOptionsIterItem::StringValue(*value)));
        }
        if let Some(value) = format {
            iter.push(("format", CommandLineOptionsIterItem::StringValue(*value)));
        }
        if let Some(value) = prepend {
            iter.push(("prepend", CommandLineOptionsIterItem::StringValue(*value)));
        }
        if let Some(value) = quote {
            iter.push(("quote", CommandLineOptionsIterItem::QuoteStyle(*value)));
        }
        if !replacements.is_empty() {
            iter.push((
                "replacements",
                CommandLineOptionsIterItem::Replacements(*replacements),
            ));
        }

        iter.into_iter()
    }
}

#[derive(derive_more::Display)]
pub(crate) enum CommandLineOptionsIterItem<'v, 'a> {
    U32(u32),
    Value(Value<'v>),
    Str(&'static str),
    StringValue(StringValue<'v>),
    Replacements(OptionsReplacementsRef<'v, 'a>),
    #[display("\"{}\"", _0)]
    QuoteStyle(QuoteStyle),
}
