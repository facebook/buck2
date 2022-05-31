/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    fmt::{self, Debug, Display},
    marker::PhantomData,
};

use anyhow::anyhow;
use buck2_core::fs::{
    paths::{RelativePath, RelativePathBuf},
    project::ProjectRelativePathBuf,
};
use derive_more::Display;
use gazebo::prelude::*;
use serde::{Serialize, Serializer};
use starlark::values::{Freeze, StringValueLike, Trace, ValueLike};

use crate::{
    actions::artifact::ArtifactFs,
    interpreter::rule_defs::{
        artifact::{StarlarkArtifactLike, ValueAsArtifactLike},
        cell_root::CellRoot,
        cmd_args::{traits::CommandLineBuilderContext, CommandLineBuilder, CommandLineLocation},
        util::commas,
    },
};

/// Supported ways of quoting arguments.
#[derive(Debug, Clone, Dupe, Trace, Freeze, Serialize)]
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
            _ => Err(anyhow!(CommandLineArgError::UnknownQuotingStyle(
                s.to_owned()
            ))),
        }
    }
}

/// Simple struct to help determine extra options for formatting
/// (whether to join items, how to join them, format strings, etc)
#[derive(Debug, Default_, Clone, Trace, Freeze, Serialize)]
pub(crate) struct FormattingOptions<S> {
    pub(crate) delimiter: Option<S>,
    pub(crate) format: Option<S>,
    pub(crate) prepend: Option<S>,
    pub(crate) quote: Option<QuoteStyle>,
}

impl<S: Debug> Display for FormattingOptions<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut comma = commas();
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
        Ok(())
    }
}

impl<S: Default> FormattingOptions<S> {
    pub(crate) fn is_empty(&self) -> bool {
        self.delimiter.is_none()
            && self.format.is_none()
            && self.prepend.is_none()
            && self.quote.is_none()
    }
}

#[derive(Debug, Default_, Clone, Trace, Serialize)]
#[repr(C)]
pub(crate) struct CommandLineOptions<'v, V: ValueLike<'v>> {
    #[serde(bound = "V: Display", serialize_with = "serialize_opt_display")]
    /// The value of V must be convertible to a `RelativeOrigin`
    pub(crate) relative_to: Option<(V, usize)>,
    pub(crate) absolute_prefix: Option<V::String>,
    pub(crate) absolute_suffix: Option<V::String>,
    pub(crate) parent: usize,
    pub(crate) ignore_artifacts: bool,
    pub(crate) formatting: FormattingOptions<V::String>,
    pub(crate) lifetime: PhantomData<&'v ()>,
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
        if !self.formatting.is_empty() {
            comma(f)?;
            Display::fmt(&self.formatting, f)?;
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
        C: CommandLineBuilderContext + ?Sized,
    {
        let loc = match self {
            Self::Artifact(artifact) => {
                // Shame we require the artifact to be bound here, we really just needs its
                // path even if it is unbound.
                let artifact = artifact.get_bound()?;
                ctx.resolve_artifact(&artifact)?
            }
            Self::CellRoot(cell_root) => ctx.resolve_cell_path(cell_root.cell_path())?,
        };

        Ok(loc.into_relative())
    }
}

impl<'v, V: ValueLike<'v>> CommandLineOptions<'v, V> {
    pub(crate) fn wrap_builder<'a, R>(
        &self,
        builder: &'a mut dyn CommandLineBuilder,
        f: impl for<'b> FnOnce(&'b mut dyn CommandLineBuilder) -> anyhow::Result<R>,
    ) -> anyhow::Result<R> {
        struct Extras<'a, 'v, S> {
            cli: &'a mut dyn CommandLineBuilder,
            relative_to: Option<RelativePathBuf>,
            absolute_prefix: Option<&'v str>,
            absolute_suffix: Option<&'v str>,
            parent: usize,
            // Auxiliary field to store concatenation result (when arguments are concatenated) and
            // a flag stating that the result is not yet started to be computated (i.e. the first
            // argument to be concatenated is not yet processed).
            concatenation_context: Option<(String, bool)>,
            formatting: FormattingOptions<S>,
        }

        impl<'a, 'v, S: StringValueLike<'v>> Extras<'a, 'v, S> {
            /// If any items need to be concatted/formatted and added to the original CLI,
            /// do it here
            fn finalize_args(mut self) -> Self {
                if let Some((concatted_items, _)) = self.concatenation_context.take() {
                    self.cli.add_arg_string(self.format(concatted_items));
                }
                self
            }

            fn format(&self, mut arg: String) -> String {
                if let Some(format) = &self.formatting.format {
                    arg = format.as_str().replace("{}", &arg);
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

        impl<'a, 'v, S> CommandLineBuilderContext for Extras<'a, 'v, S> {
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
                        absolute_prefix.unwrap_or(""),
                        x,
                        absolute_suffix.unwrap_or(""),
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

        impl<'a, 'v, S: StringValueLike<'v>> CommandLineBuilder for Extras<'a, 'v, S> {
            fn add_arg_string(&mut self, s: String) {
                if let Some((concatted_items, initital_state)) = self.concatenation_context.as_mut()
                {
                    if *initital_state {
                        *initital_state = false;
                    } else {
                        concatted_items.push_str(
                            self.formatting
                                .delimiter
                                .as_ref()
                                .map_or("", |x| x.as_str()),
                        );
                    }
                    concatted_items.push_str(&s)
                } else {
                    // NOTE: This doesn't go through formatting since to give users more
                    // flexibility. Since the prepended string is a static string, they _can_ format
                    // it ahead of time if they need to.
                    if let Some(i) = self.formatting.prepend.as_ref() {
                        self.cli.add_arg_string(i.as_str().to_owned());
                    }
                    self.cli.add_arg_string(self.format(s))
                }
            }
        }

        match (
            self.relative_to_path(builder).transpose(),
            self.absolute_prefix(),
            self.absolute_suffix(),
            self.parent(),
            self.formatting(),
        ) {
            (None, None, None, 0, None) => f(builder),
            (relative_to, absolute_prefix, absolute_suffix, parent, formatting) => {
                let concatenation_context = match formatting {
                    Some(opts) if opts.delimiter.is_some() => Some((String::new(), true)),
                    _ => None,
                };
                let formatting = match formatting {
                    Some(f) => (*f).clone(),
                    None => FormattingOptions::default(),
                };
                let mut cli_extras = Extras {
                    cli: builder,
                    relative_to: relative_to.transpose()?,
                    absolute_prefix,
                    absolute_suffix,
                    parent,
                    concatenation_context,
                    formatting,
                };
                let res = f(&mut cli_extras)?;
                cli_extras.finalize_args();
                Ok(res)
            }
        }
    }

    fn relative_to(&self) -> Option<&(V, usize)> {
        self.relative_to.as_ref()
    }
    fn absolute_prefix(&self) -> Option<&'v str> {
        self.absolute_prefix.map(|x| x.as_str())
    }

    fn absolute_suffix(&self) -> Option<&'v str> {
        self.absolute_suffix.map(|x| x.as_str())
    }

    fn parent(&self) -> usize {
        self.parent
    }

    fn formatting(&self) -> Option<&FormattingOptions<V::String>> {
        if self.formatting.is_empty() {
            None
        } else {
            Some(&self.formatting)
        }
    }

    pub(crate) fn relative_to_path<C>(&self, ctx: &C) -> anyhow::Result<Option<RelativePathBuf>>
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
