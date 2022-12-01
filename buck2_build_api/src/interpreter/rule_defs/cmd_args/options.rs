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
use std::marker::PhantomData;

use allocative::Allocative;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_interpreter::types::cell_root::CellRoot;
use derive_more::Display;
use gazebo::prelude::*;
use regex::Regex;
use serde::Serialize;
use serde::Serializer;
use starlark::values::Freeze;
use starlark::values::StringValueLike;
use starlark::values::Trace;
use starlark::values::ValueLike;

use crate::interpreter::rule_defs::artifact::StarlarkArtifactLike;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;
use crate::interpreter::rule_defs::cmd_args::traits::CommandLineContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::CommandLineLocation;
use crate::interpreter::rule_defs::util::commas;

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

#[derive(Debug, Default_, Clone, Trace, Serialize, Freeze, Allocative)]
#[repr(C)]
pub(crate) struct CommandLineOptions<'v, V: ValueLike<'v>> {
    #[serde(bound = "V: Display", serialize_with = "serialize_opt_display")]
    // These impact how artifacts are rendered
    /// The value of V must be convertible to a `RelativeOrigin`
    pub(crate) relative_to: Option<(V, usize)>,
    pub(crate) absolute_prefix: Option<V::String>,
    pub(crate) absolute_suffix: Option<V::String>,
    pub(crate) parent: usize,
    pub(crate) ignore_artifacts: bool,

    // These impact the formatting of each string
    pub(crate) delimiter: Option<V::String>,
    pub(crate) format: Option<V::String>,
    pub(crate) prepend: Option<V::String>,
    pub(crate) quote: Option<QuoteStyle>,
    pub(crate) replacements: Option<Box<Vec<(V::String, V::String)>>>,

    pub(crate) lifetime: PhantomData<&'v ()>,
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

impl<'v, V: ValueLike<'v>> Display for CommandLineOptions<'v, V> {
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
        if let Some(v) = &self.replacements {
            comma(f)?;
            write!(f, "replacements = [")?;
            let mut vec_comma = commas();
            for p in v.as_ref() {
                vec_comma(f)?;
                write!(f, "({:?}, {:?})", p.0, p.1)?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

// NOTE: This is an enum as opposed to a trait beause of the `C` parameter on (which is required
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

impl<'v, V: ValueLike<'v>> CommandLineOptions<'v, V> {
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
                replacements: None,
                ignore_artifacts: _, // Doesn't impact the builder
                lifetime: _,
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
        struct ExtrasBuilder<'a, 'v, V: ValueLike<'v>> {
            builder: &'a mut dyn CommandLineBuilder,
            opts: &'a CommandLineOptions<'v, V>,
            // Auxiliary field to store concatenation result (when arguments are concatenated) and
            // a flag stating that the result is not yet started to be computated (i.e. the first
            // argument to be concatenated is not yet processed).
            concatenation_context: Option<(String, bool)>,
        }

        struct ExtrasContext<'a, 'v, V: ValueLike<'v>> {
            ctx: &'a mut dyn CommandLineContext,
            opts: &'a CommandLineOptions<'v, V>,
            relative_to: Option<RelativePathBuf>,
        }

        impl<'a, 'v, V: ValueLike<'v>> CommandLineContext for ExtrasContext<'a, 'v, V> {
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

        impl<'a, 'v, V: ValueLike<'v>> ExtrasBuilder<'a, 'v, V> {
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
                if let Some(replacements) = &self.opts.replacements {
                    for (pattern, replacement) in replacements.iter() {
                        // We checked that regex is valid in replace_regex(), so unwrap is safe.
                        let re = Regex::new(pattern.as_str()).unwrap();
                        match re.replace_all(&arg, replacement.as_str()) {
                            Cow::Borrowed(_) => {}
                            Cow::Owned(new) => arg = new,
                        }
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

        impl<'a, 'v, V: ValueLike<'v>> CommandLineBuilder for ExtrasBuilder<'a, 'v, V> {
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
