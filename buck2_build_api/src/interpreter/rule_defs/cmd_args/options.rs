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
use buck2_core::fs::paths::RelativePathBuf;
use derive_more::Display;
use gazebo::prelude::*;
use serde::{Serialize, Serializer};
use starlark::values::{Freeze, Trace, ValueLike};

use crate::interpreter::rule_defs::{
    artifact::{StarlarkArtifactLike, ValueAsArtifactLike},
    cell_root::CellRoot,
    cmd_args::traits::CommandLineBuilderContext,
    util::commas,
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
enum QuoteError {
    #[error("Unknown quoting style `{0}`")]
    UnknownQuotingStyle(String),
}

impl QuoteStyle {
    pub fn parse(s: &str) -> anyhow::Result<QuoteStyle> {
        match s {
            "shell" => Ok(QuoteStyle::Shell),
            _ => Err(anyhow!(QuoteError::UnknownQuotingStyle(s.to_owned()))),
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
