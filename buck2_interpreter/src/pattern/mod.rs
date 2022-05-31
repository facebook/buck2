/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implements target pattern resolution.
//!
#![doc = include_str ! ("../target_pattern.md")]

use std::{
    collections::HashSet,
    fmt::{self, Debug, Display},
};

use anyhow::{anyhow, Context};
use buck2_common::{
    dice::{cells::HasCellResolver, file_ops::HasFileOps},
    file_ops::FileOps,
    target_aliases::TargetAliasResolver,
};
use buck2_core::{
    cells::{
        paths::{CellPath, CellRelativePath},
        CellAliasResolver, CellResolver,
    },
    fs::paths::ForwardRelativePath,
    package::Package,
    provider::{ProviderName, ProvidersLabel, ProvidersName},
    target::{TargetLabel, TargetName},
};
use derive_more::From;
use dice::DiceComputations;
use futures::{channel::mpsc, stream::FuturesUnordered, Stream, StreamExt};
use gazebo::prelude::*;
use indexmap::IndexMap;
use more_futures::drop::DropTogether;
use once_cell::sync::Lazy;
use regex::Regex;
use thiserror::Error;
use tokio::sync::Semaphore;

use crate::{
    package_listing::find_buildfile::find_buildfile,
    pattern::ascii_pattern::{
        split1_opt_ascii, strip_suffix_ascii, trim_prefix_ascii, AsciiChar, AsciiStr, AsciiStr2,
    },
};

mod ascii_pattern;
pub mod parse_package;

#[derive(Error, Debug)]
enum TargetPatternParseError {
    #[error("Expected a `:`, a trailing `/...` or the literal `...`.")]
    UnexpectedFormat,
    #[error("Package is empty")]
    PackageIsEmpty,
    #[error("Must be absolute, with a `//` or no package just `:`.")]
    AbsoluteRequired,
    #[error(
        "Packages may not end with a trailing `/` (except when provided on the command line where it's tolerated)"
    )]
    PackageTrailingSlash,
    #[error("Required a target literal, but got a non-literal pattern `{0}`")]
    TargetLiteralRequired(String),
}

/// The pattern type to be parsed from the command line target patterns.
///
/// This is either 'TargetLabel', 'ConfiguredTargetLabel', or
/// 'ConfiguredProvidersLabel'
pub trait PatternType: Sized + Clone + Debug + PartialEq + Eq {
    type ExtraParts;

    /// Split the given str into the part that should become the TargetName, and the ExtraParts.
    fn split(s: &str) -> anyhow::Result<(&str, Self::ExtraParts)>;

    /// Construct this from a TargetName and the ExtraParts.
    fn from_parts(target: TargetName, extra: Self::ExtraParts) -> Self;

    /// Parse this from a str.
    fn parse_from(s: &str) -> anyhow::Result<Self> {
        let (target, extra) = Self::split(s)?;
        Ok(Self::from_parts(TargetName::new(target)?, extra))
    }
}

/// Pattern that matches an explicit target without any inner providers label.
/// This is useful for 'query's where we do not expect any provider specifiers.
///
/// Ex. `//some/package:target`
pub type TargetPattern = TargetName;

impl PatternType for TargetPattern {
    type ExtraParts = ();

    fn split(s: &str) -> anyhow::Result<(&str, Self::ExtraParts)> {
        Ok((s, ()))
    }

    fn from_parts(target: TargetName, _extra: Self::ExtraParts) -> Self {
        target
    }
}

/// Pattern that matches an inner providers label that refers to a specific
/// set of providers from a rule.
/// This is useful for builds, and provider or action queries where provider
/// specifiers makes sense
///
/// Ex. `//some/package:target[java-group]`
pub type ProvidersPattern = (TargetName, ProvidersName);

/// Buck1 uses flavors for a couple different purposes. Some of those flavors have ended
/// up being used by users. In v2, the functionality of most user-visible flavors ends up
/// being handled by either configurations or subtargets. One common use of flavors in
/// v1 was to have references to secondary or intermediate outputs, in v2 we can do that
/// via subtargets.
///
/// This mapping is a hardcoded map of flavors that we know can be handled simply as subtargets.
fn map_flavors(flavors: &str) -> anyhow::Result<ProvidersName> {
    let mut flavors_parts: Vec<&str> = flavors.split(',').collect();
    assert!(!flavors_parts.is_empty());

    match flavors_parts.iter().position(|x| x.starts_with("platform")) {
        Some(index) => {
            // remove platform flavor from the vector of flavors
            flavors_parts.remove(index);
        }
        None => {}
    }

    // sort a flavors list to have a deterministic order.
    flavors_parts.sort_unstable();
    Ok(ProvidersName::Named(vec![ProviderName::new_unchecked(
        match flavors_parts.len() {
            // If we only had one flavor that represents some specific platform then return a default provider name.
            0 => {
                // Some targets specifically ask for a given platform, we just ignore them
                return Ok(ProvidersName::Default);
            }

            1 => match flavors_parts[0] {
                // android_binary intermediate/secondary outputs. See https://fburl.com/diffusion/jd3cmnfw
                "package_string_assets" => "package_string_assets".to_owned(),
                "aapt2_link" => "aapt2_link".to_owned(),
                "unstripped_native_libraries" => "unstripped_native_libraries".to_owned(),
                "proguard_text_output" => "proguard_text_output".to_owned(),
                "generate_string_resources" => "generate_string_resources".to_owned(),
                "exo_symlink_tree" => "exo_symlink_tree".to_owned(),

                // Rules depend on `#headers` flavor of C++ libraries to use a
                // dep's headers without linking against it.
                "headers" => "headers".to_owned(),

                // This is used by Rust quite a bit
                "check" => "check".to_owned(),

                // FIXME(ndmitchell): Most users shouldn't be using strip-debug.
                // We currently can't handle strip-debug, and it's a dependency of Eden,
                // so just ignore it for now. D27984137 aims to add it back properly.
                "strip-debug" => return Ok(ProvidersName::Default),

                // Used in JEX builder script (https://fburl.com/code/2w2gjkey)
                "shared" => "shared".to_owned(),

                // This is for js_bundle. We strip it and let the configuration handle it instead.
                "android" => return Ok(ProvidersName::Default),

                _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
            },

            // For js_bundle rules. The platform and optimization ("release") flavors are stripped
            // and handled by the configuration. The other flavors are mapped to named outputs.
            2 => match (flavors_parts[0], flavors_parts[1]) {
                ("android", "dependencies") => "dependencies".to_owned(),
                ("android", "misc") => "misc".to_owned(),
                ("android", "source_map") => "source_map".to_owned(),
                ("android", "release") => return Ok(ProvidersName::Default),
                _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
            },

            3 => match (flavors_parts[0], flavors_parts[1], flavors_parts[2]) {
                ("android", "dependencies", "release") => "dependencies".to_owned(),
                ("android", "misc", "release") => "misc".to_owned(),
                ("android", "release", "source_map") => "source_map".to_owned(),
                _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
            },

            4 => match (
                flavors_parts[0],
                flavors_parts[1],
                flavors_parts[2],
                flavors_parts[3],
            ) {
                ("android", "misc", "rambundle-indexed", "release") => {
                    "rambundle-indexed-misc".to_owned()
                }
                _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
            },

            // This allows us to pass parsing for this thing.
            _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
        },
    )]))
}

impl PatternType for ProvidersPattern {
    type ExtraParts = ProvidersName;

    fn split(s: &str) -> anyhow::Result<(&str, Self::ExtraParts)> {
        if let Some((t, flavors)) = split1_opt_ascii(s, AsciiChar::new('#')) {
            let name = map_flavors(flavors)?;
            Ok((t, name))
        } else if let Some((t, p)) = split1_opt_ascii(s, AsciiChar::new('[')) {
            let mut names = Vec::new();

            let mut remaining = if let Some((p, r)) = split1_opt_ascii(p, AsciiChar::new(']')) {
                names.push(ProviderName::new(p.to_owned())?);
                r
            } else {
                return Err(anyhow!(
                    "target pattern with `[` must end with `]` to mark end of providers set label"
                ));
            };

            while !remaining.is_empty() {
                if let Some(("", r)) = split1_opt_ascii(remaining, AsciiChar::new('[')) {
                    if let Some((p, r)) = split1_opt_ascii(r, AsciiChar::new(']')) {
                        names.push(ProviderName::new(p.to_owned())?);
                        remaining = r;
                        continue;
                    }
                }
                return Err(anyhow!(
                    "target pattern with `[` must end with `]` to mark end of providers set label"
                ));
            }

            Ok((t, ProvidersName::Named(names)))
        } else {
            Ok((s, ProvidersName::Default))
        }
    }

    fn from_parts(target: TargetName, extra: Self::ExtraParts) -> Self {
        (target, extra)
    }
}

/// A parsed target pattern.
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum ParsedPattern<T> {
    /// A target pattern that matches a explicit target pattern type T. See
    /// `PatternType` for pattern
    Target(Package, T),
    /// A target pattern that matches an entire package. Ex. `//some/package:`
    Package(Package),
    /// A target pattern that matches all recursive packages. Ex.
    /// `//some/package/...`. The path component here is not required to be
    /// an actual package (i.e. a build file is not required at the path)
    /// and so we don't hold this as a [Package].
    Recursive(CellPath),
}

impl ParsedPattern<TargetPattern> {
    /// Extract [`TargetLabel`] from a [`ParsedPattern`].
    pub fn as_target_label(self, original: &str) -> anyhow::Result<TargetLabel> {
        let (package, target_name) = self.as_literal(original)?;
        Ok(TargetLabel::new(package, target_name))
    }

    /// Check if a [`ParsedPattern`] matches a [`TargetLabel`]
    pub fn matches(&self, target: &TargetLabel) -> bool {
        let target_pkg = target.pkg();
        match self {
            ParsedPattern::Target(pkg, t) => pkg == target_pkg && t == target.name(),
            ParsedPattern::Package(pkg) => target_pkg.as_cell_path() == pkg.as_cell_path(),
            ParsedPattern::Recursive(cell_path) => target_pkg.as_cell_path().starts_with(cell_path),
        }
    }
}

impl ParsedPattern<ProvidersPattern> {
    /// Extract [`ProvidersLabel`] from a [`ParsedPattern`].
    pub fn as_providers_label(self, original: &str) -> anyhow::Result<ProvidersLabel> {
        let (package, (target_name, providers_name)) = self.as_literal(original)?;

        Ok(ProvidersLabel::new(
            TargetLabel::new(package, target_name),
            providers_name,
        ))
    }
}

impl<T: PatternType> ParsedPattern<T> {
    /// Extract a literal from a [ParsedPattern], or `Err` if it is not a literal.
    pub fn as_literal(self, original: &str) -> anyhow::Result<(Package, T)> {
        // FIXME: Would be better if we had a Display on self, so we could produce a nice error message.
        //        For now, just require the original string to be passed in for good errors.
        match self {
            ParsedPattern::Target(package, val) => Ok((package, val)),
            _ => Err(TargetPatternParseError::TargetLiteralRequired(original.to_owned()).into()),
        }
    }

    /// Parse a TargetPattern, but where there there is no relative directory.
    /// Generally, not a good thing to do - should aim to remove most of these.
    pub fn parse_precise(cell_resolver: &CellAliasResolver, pattern: &str) -> anyhow::Result<Self> {
        parse_target_pattern(
            cell_resolver,
            None,
            TargetParsingOptions::precise(),
            pattern,
        )
        .with_context(|| format!("Invalid absolute target pattern `{}`", pattern))
    }

    pub fn parsed_opt_absolute(
        cell_resolver: &CellAliasResolver,
        relative_dir: Option<&Package>,
        pattern: &str,
    ) -> anyhow::Result<Self> {
        parse_target_pattern(
            cell_resolver,
            None,
            TargetParsingOptions {
                relative_dir,
                relative: false,
                infer_target: false,
                strip_package_trailing_slash: false,
            },
            pattern,
        )
        .with_context(|| format!("Invalid absolute target pattern `{}`", pattern))
    }

    /// Parse a TargetPattern out, resolving aliases via `cell_resolver`, and resolving relative
    /// targets via `enclosing_package`, if provided.
    /// Allows everything from `parse_absolute`, plus relative patterns.
    pub fn parse_relative(
        target_alias_resolver: &TargetAliasResolver,
        cell_resolver: &CellAliasResolver,
        relative_dir: &Package,
        pattern: &str,
    ) -> anyhow::Result<Self> {
        parse_target_pattern(
            cell_resolver,
            Some(target_alias_resolver),
            TargetParsingOptions {
                relative_dir: Some(relative_dir),
                relative: true,
                infer_target: false,
                strip_package_trailing_slash: false,
            },
            pattern,
        )
        .with_context(|| format!("Invalid relative target pattern `{}`", pattern))
    }

    /// Parse a TargetPattern out, resolving aliases via `cell_resolver`, resolving relative
    /// targets via `relative_dir`, inferring a target name if no target or recursive pattern
    /// is provided (e.g. `//foo/bar` is inferred to be equivalent to `//foo/bar:bar`), and
    /// stripping trailing `/` in package names instead of rejecting them.
    ///
    /// This should only be used with user-provided command line arguments, as precision is
    /// generally preferred elsewhere.
    pub fn parse_relaxed(
        target_alias_resolver: &TargetAliasResolver,
        cell_resolver: &CellAliasResolver,
        relative_dir: &Package,
        pattern: &str,
    ) -> anyhow::Result<Self> {
        parse_target_pattern(
            cell_resolver,
            Some(target_alias_resolver),
            TargetParsingOptions {
                relative_dir: Some(relative_dir),
                relative: true,
                infer_target: true,
                strip_package_trailing_slash: true,
            },
            pattern,
        )
        .with_context(|| format!("Invalid relative target pattern `{}`", pattern))
    }
}

impl Display for ParsedPattern<ProvidersPattern> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsedPattern::Target(package, (target, providers)) => {
                write!(f, "{}:{}{}", package.as_cell_path(), target, providers)
            }
            ParsedPattern::Package(package) => {
                write!(f, "{}:", package.as_cell_path())
            }
            ParsedPattern::Recursive(path) => {
                write!(f, "{}/...", path)
            }
        }
    }
}

#[derive(Debug)]
pub struct PatternParts<'a, T> {
    /// Is there a `foo//` or `//` part.
    cell_alias: Option<&'a str>,
    pub pattern: PatternDataOrAmbiguous<'a, T>,
}

#[derive(Debug, From)]
pub enum PatternDataOrAmbiguous<'a, T> {
    /// We successfully extracted PatternData.
    PatternData(PatternData<'a, T>),

    /// This pattern looks like `foo/bar`, `foo/bar/` or `foo`. It could be a package + target if
    /// we allow inference (i.e. expanding `foo/bar:bar`).
    Ambiguous {
        /// The pattern. If we allow inference this will become the package.
        pattern: &'a str,
        /// Whether we should strip trailing slashes out of this pattern before doing inference
        /// (rather than throwing an error).
        strip_package_trailing_slash: bool,
    },
}

impl<'a, T> PatternDataOrAmbiguous<'a, T>
where
    T: PatternType,
{
    /// If the pattern is ambiguous, try to infer a target. This would convert `foo/bar` into
    /// `foo/bar:bar`.
    pub fn infer_target(self) -> anyhow::Result<PatternData<'a, T>> {
        match self {
            Self::PatternData(d) => Ok(d),
            Self::Ambiguous {
                pattern,
                strip_package_trailing_slash,
            } => {
                // It would be a little cleaner for this to not allocate a TargetName but instead
                // just split for us.
                let (pattern, extra) = T::split(pattern)?;
                let package = normalize_package(pattern, strip_package_trailing_slash)?;

                let target = package
                    .file_name()
                    .context(TargetPatternParseError::PackageIsEmpty)?;

                let target = TargetName::new(target.as_ref())?;

                Ok(PatternData::TargetInPackage {
                    package,
                    target: T::from_parts(target, extra),
                })
            }
        }
    }

    /// If the pattern is ambiguous, error out.
    pub fn reject_ambiguity(self) -> anyhow::Result<PatternData<'a, T>> {
        match self {
            Self::PatternData(d) => Ok(d),
            Self::Ambiguous { .. } => Err(TargetPatternParseError::UnexpectedFormat.into()),
        }
    }
}

/// The pattern data we extracted.
#[derive(Debug)]
pub enum PatternData<'a, T> {
    /// A pattern like `foo/bar/...`.
    Recursive { package: &'a ForwardRelativePath },

    /// A pattern like `foo/bar:`, or `:`
    AllTargetsInPackage { package: &'a ForwardRelativePath },

    /// A pattern like `foo/bar:qux`, or `:qux`. The target will never be empty.
    TargetInPackage {
        package: &'a ForwardRelativePath,
        target: T,
    },
}

impl<'a, T> PatternData<'a, T> {
    pub fn package_path(&self) -> &'a ForwardRelativePath {
        match self {
            Self::Recursive { package } => package,
            Self::AllTargetsInPackage { package } => package,
            Self::TargetInPackage { package, .. } => package,
        }
    }

    pub fn target(&self) -> Option<&T> {
        match self {
            Self::Recursive { .. } => None,
            Self::AllTargetsInPackage { .. } => None,
            Self::TargetInPackage { target, .. } => Some(target),
        }
    }

    /// Whether this is a target that looks like `:target`.
    pub fn is_adjacent_target(&self) -> bool {
        self.package_path().is_empty() && self.target().is_some()
    }
}

// Lex the target pattern into the relevant pieces
pub fn lex_target_pattern<'a, T>(
    pattern: &'a str,
    strip_package_trailing_slash: bool,
) -> anyhow::Result<PatternParts<T>>
where
    T: PatternType,
{
    let (cell_alias, pattern) = match split1_opt_ascii(pattern, AsciiStr2::new("//")) {
        Some((a, p)) => (Some(trim_prefix_ascii(a, AsciiChar::new('@'))), p),
        None => (None, pattern),
    };

    let pattern = match split1_opt_ascii(pattern, AsciiChar::new(':')) {
        Some((package, "")) => PatternData::AllTargetsInPackage {
            package: normalize_package(package, strip_package_trailing_slash)?,
        }
        .into(),
        Some((package, target)) => PatternData::TargetInPackage {
            package: normalize_package(package, strip_package_trailing_slash)?,
            target: T::parse_from(target)?,
        }
        .into(),
        None => {
            if let Some(package) = strip_suffix_ascii(pattern, AsciiStr::new("/...")) {
                PatternData::Recursive {
                    package: ForwardRelativePath::new(package)?,
                }
                .into()
            } else if pattern == "..." {
                PatternData::Recursive {
                    package: ForwardRelativePath::new("")?,
                }
                .into()
            } else if !pattern.is_empty() {
                PatternDataOrAmbiguous::Ambiguous {
                    pattern,
                    strip_package_trailing_slash,
                }
            } else {
                return Err(TargetPatternParseError::UnexpectedFormat.into());
            }
        }
    };

    Ok(PatternParts {
        cell_alias,
        pattern,
    })
}

fn normalize_package<'a>(
    package: &'a str,
    strip_package_trailing_slash: bool,
) -> anyhow::Result<&'a ForwardRelativePath> {
    // Strip or reject trailing `/`, such as in `foo/:bar`.
    if let Some(stripped) = strip_suffix_ascii(package, AsciiChar::new('/')) {
        if strip_package_trailing_slash {
            return ForwardRelativePath::new(stripped);
        } else {
            return Err(anyhow::Error::from(
                TargetPatternParseError::PackageTrailingSlash,
            ));
        }
    }

    ForwardRelativePath::new(package)
}

#[derive(Copy, Clone, Dupe)]
struct TargetParsingOptions<'a> {
    /// The dir this pattern should be intepreted relative to.  This will be used to prepend to the
    /// package if `relative` is set, otherwise it'll only be used for targets such as `:foo`.
    relative_dir: Option<&'a Package>,
    /// Whether to interpret packages relatively.
    relative: bool,
    /// Whether to infer the target in a pattern such as `foo/bar` (to `foo/bar:bar`).
    infer_target: bool,
    /// Whether to strip trailing slashes in package names, in e.g. `foo/bar/` or `foo/bar/:qux`.
    /// If not set, trailing slashes are an error. Note that this happens before target inference
    /// (if enabled), so e.g. `foo/bar/` becomes `foo/bar:bar`.
    strip_package_trailing_slash: bool,
}

impl<'a> TargetParsingOptions<'a> {
    fn precise() -> Self {
        TargetParsingOptions {
            relative_dir: None,
            relative: false,
            infer_target: false,
            strip_package_trailing_slash: false,
        }
    }
}

/// Parse a TargetPattern out, resolving aliases via `cell_resolver`, and resolving relative
/// targets via `enclosing_package`, if provided.
fn parse_target_pattern<T>(
    cell_resolver: &CellAliasResolver,
    target_alias_resolver: Option<&TargetAliasResolver>,
    opts: TargetParsingOptions,
    pattern: &str,
) -> anyhow::Result<ParsedPattern<T>>
where
    T: PatternType,
{
    let TargetParsingOptions {
        relative_dir,
        relative,
        infer_target,
        strip_package_trailing_slash,
    } = opts;

    debug_assert!(if relative {
        relative_dir.is_some()
    } else {
        true
    });

    let lex = lex_target_pattern(pattern, strip_package_trailing_slash)?;

    if let Some(target_alias_resolver) = target_alias_resolver {
        if let Some(aliased) = resolve_target_alias(cell_resolver, target_alias_resolver, &lex)? {
            return Ok(aliased);
        }
    }

    let PatternParts {
        cell_alias,
        pattern,
    } = lex;

    let pattern = if infer_target {
        pattern.infer_target()?
    } else {
        pattern.reject_ambiguity()?
    };

    // This allows things of the form `//foo` (having a cell alias) or `:bar` (no cell, no package,
    // just relative target). This is a bit of a wonky  definition of "is_absolute" but we rely on
    // it.
    let is_absolute = cell_alias.is_some() || pattern.is_adjacent_target();
    if !relative && !is_absolute {
        return Err(TargetPatternParseError::AbsoluteRequired.into());
    }

    // We ask for the cell, but if the pattern is relative we might not use it
    let cell = cell_resolver.resolve(cell_alias.unwrap_or_default())?;

    let package_path = pattern.package_path();

    let path = match relative_dir {
        Some(rel) if cell_alias.is_none() && (relative || package_path.is_empty()) => {
            rel.join_unnormalized(package_path)
        }
        Some(rel)
            if rel.as_cell_path().cell() == cell
                && rel.as_cell_path().path().as_forward_relative_path() == package_path =>
        {
            // Reuse the `Package` object if parsed package is equal to the current package.
            // `Package` reconstruction is relatively cheap, but not free.
            rel.dupe()
        }
        _ => Package::new(cell, CellRelativePath::new(package_path)),
    };

    match pattern {
        PatternData::Recursive { .. } => Ok(ParsedPattern::Recursive(path.as_cell_path().clone())),
        PatternData::AllTargetsInPackage { .. } => Ok(ParsedPattern::Package(path)),
        PatternData::TargetInPackage { target, .. } => Ok(ParsedPattern::Target(path, target)),
    }
}

#[derive(Error, Debug)]
enum ResolveTargetAliasError {
    #[error("Error dereferencing alias `{}` -> `{}`", target, alias)]
    ErrorDereferencing { target: String, alias: String },

    #[error("Invalid alias: `{}`", alias)]
    InvalidAlias { alias: String },

    #[error("Alias for `{}` is not a target: `{}`", target, alias)]
    AliasIsNotATarget { target: String, alias: String },
}

fn resolve_target_alias<T>(
    cell_resolver: &CellAliasResolver,
    target_alias_resolver: &TargetAliasResolver,
    lex: &PatternParts<T>,
) -> anyhow::Result<Option<ParsedPattern<T>>>
where
    T: PatternType,
{
    // Imported from Buck1
    static ALIAS_REGEX: Lazy<Regex> =
        Lazy::new(|| Regex::new("^[a-zA-Z_-][a-zA-Z0-9_-]*$").unwrap());

    // If the input starts with a cell path, it can't be an alias.
    if lex.cell_alias.is_some() {
        return Ok(None);
    }

    // Unless the input is a standalone bit of ambiguous text then it cannot be an alias.
    let package = match lex.pattern {
        PatternDataOrAmbiguous::Ambiguous { pattern, .. } => pattern,
        _ => return Ok(None),
    };

    // Assuming this might be an alias, try to parse it as the thing we are trying to parse here.
    // This lets us resolve `foo` in `foo[bar]` as an alias, for example. If this is invalid we'll
    // just skip alias resolution and let the error propagate in `parse_target_pattern`.
    let (target, extra) = match T::split(package) {
        Ok(split) => split,
        Err(..) => return Ok(None),
    };

    // Check if this is an alias after all.
    let alias = match target_alias_resolver.get(target)? {
        Some(alias) => alias,
        None => return Ok(None),
    };

    // Now that we know it's an alias, check it matches the regex. We only do this once we know the
    // alias is valid so that we avoid throwing "alias is invalid" if the user didn't mean to use
    // an alias.
    if !ALIAS_REGEX.is_match(target) {
        return Err(ResolveTargetAliasError::InvalidAlias {
            alias: alias.to_owned(),
        }
        .into());
    }

    // We found a matching alias. Parse the alias as a target.
    let res = parse_target_pattern::<TargetPattern>(
        cell_resolver,
        None,
        TargetParsingOptions::precise(),
        alias,
    )
    .with_context(|| ResolveTargetAliasError::ErrorDereferencing {
        target: target.to_owned(),
        alias: alias.to_owned(),
    })?;

    // And finally, put the `T` we were looking for back together.
    let res = match res {
        ParsedPattern::Target(package, target) => {
            ParsedPattern::Target(package, T::from_parts(target, extra))
        }
        _ => {
            return Err(ResolveTargetAliasError::AliasIsNotATarget {
                target: target.to_owned(),
                alias: alias.to_owned(),
            }
            .into());
        }
    };

    Ok(Some(res))
}

#[derive(Debug, Eq, PartialEq)]
pub enum PackageSpec<T> {
    Targets(Vec<T>),
    All,
}

#[derive(Debug)]
pub struct ResolvedPattern<T> {
    pub specs: IndexMap<Package, PackageSpec<T>>,
}

impl<T> ResolvedPattern<T>
where
    T: PatternType,
{
    pub fn new() -> Self {
        Self {
            specs: IndexMap::new(),
        }
    }

    pub fn add_package(&mut self, package: &Package) {
        self.specs.insert(package.dupe(), PackageSpec::All);
    }

    pub fn add_target(&mut self, package: &Package, target: &T) {
        if let Some(s) = self.specs.get_mut(package) {
            match s {
                PackageSpec::Targets(ref mut t) => t.push(target.clone()),
                PackageSpec::All => {}
            }
        } else {
            self.specs
                .insert(package.dupe(), PackageSpec::Targets(vec![target.clone()]));
        }
    }
}

/// Resolves a list of [ParsedPattern] to a [ResolvedPattern].
pub async fn resolve_target_patterns<
    'a,
    P: 'a + PatternType,
    T: Iterator<Item = &'a ParsedPattern<P>>,
>(
    cell_resolver: &CellResolver,
    patterns: T,
    file_ops: &dyn FileOps,
) -> anyhow::Result<ResolvedPattern<P>> {
    let mut resolved = ResolvedPattern::new();
    for pattern in patterns {
        match pattern {
            ParsedPattern::Target(package, target) => {
                resolved.add_target(package, target);
            }
            ParsedPattern::Package(package) => {
                resolved.add_package(package);
            }
            ParsedPattern::Recursive(cell_path) => {
                let roots = find_package_roots(cell_path.clone(), file_ops, cell_resolver)
                    .await
                    .context("When resolving recursive target pattern.")?;
                for package in roots {
                    resolved.add_package(&package);
                }
            }
        }
    }
    Ok(resolved)
}

/// Resolves a list of CellPath to a stream of Package representing all the
/// packages recursively contained in the paths (used for resolving patterns
/// like `//module/...`). There's no guarantees about the order that results
/// are returned, if ordering is important the caller needs to handle it.
pub async fn find_package_roots_stream(
    ctx: &DiceComputations,
    paths: Vec<CellPath>,
) -> impl Stream<Item = anyhow::Result<Package>> {
    // Ideally we wouldn't take a ctx here, but if we pull things like the package_listing_resolver
    // out of the ctx, that resolver would have a lifetime bound to the ctx and then we couldn't
    // do a tokio::spawn (or even the ctx.temporary_spawn). So, we need to only pull those things
    // out within the spawned task. This will likely need to be changed to be another trait so we
    // can write some tests.

    // TODO(cjhopman): Convert this to a inline defined stream when support for those is good.
    let (packages_tx, packages_rx) = mpsc::unbounded();

    // We don't wait on the task finishing. The packages_rx we return will naturally end when the tx side is dropped.
    let spawned = ctx.temporary_spawn(|ctx| async move {
        let file_ops = ctx.file_ops();
        let cell_resolver = ctx.get_cell_resolver().await;
        let _ignored = find_package_roots_impl(&file_ops, &cell_resolver, paths, |res| {
            packages_tx.unbounded_send(res)
        })
        .await;
    });

    DropTogether::new(packages_rx, spawned)
}

async fn find_package_roots_impl<E>(
    file_ops: &dyn FileOps,
    cell_resolver: &CellResolver,
    paths: Vec<CellPath>,
    mut func: impl FnMut(anyhow::Result<Package>) -> Result<(), E>,
) -> Result<(), E> {
    // While we are discovering packages we may also be trying to load them. Both of these can
    // require reading dirs so we want to leave some capacity to serve the package loading.
    // We should make sure this is less than the semaphore used for limiting total read_dir.
    // TODO(cjhopman): We could probably figure out some form of tiered semaphore and tokio
    // num threads configuration to coordinate these.
    static SEMAPHORE: Lazy<Semaphore> = Lazy::new(|| Semaphore::new(300));
    let semaphore = &SEMAPHORE;

    let mut queue = FuturesUnordered::new();
    let mut seen = HashSet::new();

    let list_dir = |path| async move {
        let _permit = semaphore.acquire().await.unwrap();
        let listing = file_ops.read_dir(&path).await;
        (path, listing)
    };

    for path in paths {
        match file_ops.is_ignored(&path).await {
            Ok(true) => {
                // TODO(cjhopman): Ignoring this matches buck1 behavior, but we'd like this to be an error.
            }
            Ok(false) => {
                if seen.insert(path.clone()) {
                    queue.push(list_dir(path));
                }
            }
            Err(e) => func(Err(
                e.context(format!("when resolving recursive spec `{}/...`", path))
            ))?,
        }
    }

    while let Some((path, listing)) = queue.next().await {
        let (buildfile_candidates, listing) = match cell_resolver
            .get(path.cell())
            .and_then(|cell_instance| anyhow::Ok((cell_instance.buildfiles(), listing?)))
        {
            Ok(r) => r,
            Err(e) => {
                func(Err(e.context(format!(
                    "when resolving recursive spec `{}/...`",
                    path
                ))))?;
                continue;
            }
        };

        if find_buildfile(buildfile_candidates, &listing).is_some() {
            func(Ok(Package::from_cell_path(&path)))?;
        }

        // The rev() call isn't necessary, it ends up causing us to slightly prefer running
        // things in order and we've encountered a specific case that benefits a lot from it
        // (due to having some huge things in an `apps/` dir).
        for entry in listing.iter().rev() {
            if entry.file_type.is_dir() {
                let child =
                    path.join_unnormalized(ForwardRelativePath::unchecked_new(&entry.file_name));
                if seen.insert(child.clone()) {
                    queue.push(list_dir(child));
                }
            }
        }
    }

    Ok(())
}

/// From a given root, this will walk the file tree and find all recursive "package
/// roots" (i.e. directories that contain build files). It will return all the
/// found roots.
async fn find_package_roots(
    cell_path: CellPath,
    fs: &dyn FileOps,
    cells: &CellResolver,
) -> anyhow::Result<Vec<Package>> {
    let mut results = Vec::new();
    find_package_roots_impl(fs, cells, vec![cell_path], |res| {
        results.push(res);
        Result::<_, !>::Ok(())
    })
    .await?;
    let mut results: Vec<_> = results.into_try_map(|v| v)?;
    results.sort();
    Ok(results)
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{BTreeMap, BTreeSet, HashMap},
        convert::TryFrom,
        marker::PhantomData,
        sync::Arc,
    };

    use assert_matches::assert_matches;
    use buck2_common::{
        file_ops::testing::TestFileOps,
        legacy_configs::{testing::parse, LegacyBuckConfig},
        target_aliases::TargetAliasResolver,
    };
    use buck2_core::{
        cells::{paths::CellRelativePathBuf, CellAlias, CellName, CellsAggregator},
        fs::project::ProjectRelativePathBuf,
        package::testing::PackageExt,
        provider::ProvidersLabel,
        target::TargetLabel,
    };
    use test_case::test_case;

    use super::*;

    fn mk_package<P>(cell: &str, path: &str) -> ParsedPattern<P> {
        ParsedPattern::Package(Package::testing_new(cell, path))
    }

    fn mk_recursive<P>(cell: &str, path: &str) -> ParsedPattern<P> {
        ParsedPattern::Recursive(CellPath::new(
            CellName::unchecked_new(cell.to_owned()),
            CellRelativePathBuf::unchecked_new(path.to_owned()),
        ))
    }

    fn mk_target(cell: &str, path: &str, target: &str) -> ParsedPattern<TargetPattern> {
        ParsedPattern::Target(
            Package::testing_new(cell, path),
            TargetName::unchecked_new(target),
        )
    }

    fn mk_providers(
        cell: &str,
        path: &str,
        target: &str,
        providers: Option<&[&str]>,
    ) -> ParsedPattern<ProvidersPattern> {
        ParsedPattern::Target(
            Package::testing_new(cell, path),
            (
                TargetName::unchecked_new(target),
                providers.map_or(ProvidersName::Default, |n| {
                    ProvidersName::Named(n.map(|s| ProviderName::new((*s).to_owned()).unwrap()))
                }),
            ),
        )
    }

    fn fails<R>(x: anyhow::Result<R>, msgs: &[&str]) {
        match x {
            Err(e) => {
                let s = format!("{:#}", e);
                for msg in msgs {
                    if !s.contains(msg) {
                        panic!("Expected `{}` but missing from error `{:#}`", msg, e)
                    }
                }
            }
            Ok(_) => panic!("Expected failure but succeeded"),
        }
    }

    fn no_aliases() -> TargetAliasResolver {
        TargetAliasResolver::new(LegacyBuckConfig::empty())
    }

    fn resolver() -> CellAliasResolver {
        let m = hashmap![
            CellAlias::new("".to_owned()) => CellName::unchecked_new("root".to_owned()),
            CellAlias::new("cell1".to_owned()) => CellName::unchecked_new("cell1".to_owned()),
            CellAlias::new("alias2".to_owned()) => CellName::unchecked_new("cell2".to_owned()),
        ];
        CellAliasResolver::new(Arc::new(m)).expect("valid resolver")
    }

    #[test_case(PhantomData::< TargetPattern >; "parsing TargetPattern")]
    #[test_case(PhantomData::< ProvidersPattern >; "parsing ProvidersPattern")]
    fn parse_absolute_pattern<T: PatternType>(_: PhantomData<T>) {
        let package = Package::new(
            resolver().resolve_self(),
            CellRelativePath::unchecked_new("package/path"),
        );

        assert_eq!(
            mk_package::<T>("root", "package/path"),
            ParsedPattern::<T>::parse_precise(&resolver(), "//package/path:").unwrap()
        );
        assert_eq!(
            mk_package::<T>("root", ""),
            ParsedPattern::<T>::parse_precise(&resolver(), "//:").unwrap()
        );
        assert_eq!(
            mk_package::<T>("cell1", "package/path"),
            ParsedPattern::<T>::parse_precise(&resolver(), "cell1//package/path:").unwrap()
        );
        assert_matches!(
            ParsedPattern::<T>::parse_precise(&resolver(), "package/path:"),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<TargetPatternParseError>(),
                    Some(TargetPatternParseError::AbsoluteRequired)
                );
            }
        );
        assert_eq!(
            mk_package::<T>("cell2", "package/path"),
            ParsedPattern::<T>::parse_precise(&resolver(), "alias2//package/path:").unwrap()
        );
        assert_eq!(
            mk_package::<T>("cell2", "package/path"),
            ParsedPattern::<T>::parse_precise(&resolver(), "@alias2//package/path:").unwrap()
        );
        assert_eq!(
            mk_recursive::<T>("root", "package/path"),
            ParsedPattern::<T>::parse_precise(&resolver(), "//package/path/...").unwrap()
        );
        assert_eq!(
            mk_recursive::<T>("root", "package/path"),
            ParsedPattern::<T>::parse_relative(&no_aliases(), &resolver(), &package, "...")
                .unwrap()
        );
        assert_eq!(
            mk_recursive::<T>("root", "package/path/foo"),
            ParsedPattern::<T>::parse_relative(&no_aliases(), &resolver(), &package, "foo/...")
                .unwrap()
        );
    }

    #[test]
    fn parse_relative_pattern() -> anyhow::Result<()> {
        let package = Package::new(
            resolver().resolve_self(),
            CellRelativePath::unchecked_new("package/path"),
        );

        assert_eq!(
            mk_target("root", "package/path", "target"),
            ParsedPattern::parse_precise(&resolver(), "//package/path:target")?
        );
        assert_eq!(
            mk_target("root", "package/path/foo", "target"),
            ParsedPattern::parse_relative(&no_aliases(), &resolver(), &package, "foo:target")?
        );
        Ok(())
    }

    #[test]
    fn test_relaxed() -> anyhow::Result<()> {
        let package = Package::new(
            resolver().resolve_self(),
            CellRelativePath::unchecked_new("package"),
        );

        assert_matches!(
            ParsedPattern::<TargetPattern>::parse_relative(
                &no_aliases(),
                &resolver(),
                &package,
                "path"
            ),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<TargetPatternParseError>(),
                    Some(TargetPatternParseError::UnexpectedFormat)
                );
            }
        );

        assert_eq!(
            mk_target("root", "package/path", "path"),
            ParsedPattern::parse_relaxed(&no_aliases(), &resolver(), &package, "//package/path")?
        );
        assert_eq!(
            mk_target("root", "package/path", "path"),
            ParsedPattern::parse_relaxed(&no_aliases(), &resolver(), &package, "path")?
        );
        assert_eq!(
            mk_providers("root", "package/path", "path", Some(&["provider"])),
            ParsedPattern::parse_relaxed(&no_aliases(), &resolver(), &package, "path[provider]")?
        );
        assert_eq!(
            mk_providers(
                "root",
                "package/path/subpath",
                "subpath",
                Some(&["provider"])
            ),
            ParsedPattern::parse_relaxed(
                &no_aliases(),
                &resolver(),
                &package,
                "path/subpath[provider]"
            )?
        );
        assert_eq!(
            mk_target("root", "package/path/subpath", "subpath"),
            ParsedPattern::parse_relaxed(&no_aliases(), &resolver(), &package, "path/subpath")?
        );
        assert_eq!(
            mk_target("root", "package/path", "path"),
            ParsedPattern::parse_relaxed(&no_aliases(), &resolver(), &package, "//package/path/")?
        );
        assert_eq!(
            mk_target("root", "package/path", "target"),
            ParsedPattern::parse_relaxed(
                &no_aliases(),
                &resolver(),
                &package,
                "//package/path/:target"
            )?
        );

        // Awkward but technically valid?
        assert_eq!(
            mk_target("root", "package", "foo"),
            ParsedPattern::parse_relaxed(&no_aliases(), &resolver(), &package, "/:foo")?
        );

        // There's no target here so this is invalid.
        assert_matches!(
            ParsedPattern::<TargetPattern>::parse_relaxed(
                &no_aliases(),
                &resolver(),
                &package,
                "/"
            ),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<TargetPatternParseError>(),
                    Some(TargetPatternParseError::PackageIsEmpty)
                );
            }
        );

        Ok(())
    }

    #[test]
    fn test_parsed_opt_absolute() -> anyhow::Result<()> {
        let package = Package::new(
            resolver().resolve_self(),
            CellRelativePath::unchecked_new("package/path"),
        );

        assert_eq!(
            mk_target("root", "other", "target"),
            ParsedPattern::parsed_opt_absolute(&resolver(), Some(&package), "//other:target")?
        );
        assert_eq!(
            mk_target("root", "package/path", "target"),
            ParsedPattern::parsed_opt_absolute(&resolver(), Some(&package), ":target")?
        );

        assert_matches!(
            ParsedPattern::<TargetPattern>::parsed_opt_absolute(
                &resolver(),
                Some(&package),
                "foo/bar"
            ),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<TargetPatternParseError>(),
                    Some(TargetPatternParseError::UnexpectedFormat)
                );
            }
        );

        assert_matches!(
            ParsedPattern::<TargetPattern>::parsed_opt_absolute(
                &resolver(),
                Some(&package),
                "foo/bar:bar"
            ),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<TargetPatternParseError>(),
                    Some(TargetPatternParseError::AbsoluteRequired)
                );
            }
        );

        Ok(())
    }

    #[test]
    fn test_aliases() -> anyhow::Result<()> {
        let package = Package::new(
            resolver().resolve_self(),
            CellRelativePath::unchecked_new("package"),
        );

        let config = parse(
            &[(
                "/config",
                indoc::indoc!(
                    r#"
                    [alias]
                      foo = cell1//foo/bar:target
                      invalid/alias = cell1//foo/bar:target
                      badalias = cell1//foo/bar:
                    "#
                ),
            )],
            "/config",
        )?;

        assert_eq!(
            mk_target("cell1", "foo/bar", "target"),
            ParsedPattern::parse_relaxed(
                &config.target_alias_resolver(),
                &resolver(),
                &package,
                "foo"
            )?
        );

        assert_matches!(
            ParsedPattern::<TargetPattern>::parse_relaxed(
                &config.target_alias_resolver(),
                &resolver(),
                &package,
                "invalid/alias"
            ),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<ResolveTargetAliasError>(),
                    Some(ResolveTargetAliasError::InvalidAlias { .. })
                );
            }
        );

        assert_matches!(
            ParsedPattern::<TargetPattern>::parse_relaxed(
                &config.target_alias_resolver(),
                &resolver(),
                &package,
                "badalias"
            ),
            Err(e) => {
                assert_matches!(
                    e.downcast_ref::<ResolveTargetAliasError>(),
                    Some(ResolveTargetAliasError::AliasIsNotATarget { .. })
                );
            }
        );

        Ok(())
    }

    #[test]
    fn parse_providers_pattern() -> anyhow::Result<()> {
        assert_eq!(
            mk_providers("root", "package/path", "target", None),
            ParsedPattern::parse_precise(&resolver(), "//package/path:target")?
        );
        assert_eq!(
            mk_providers("root", "package/path", "target", Some(&["java-output"])),
            ParsedPattern::parse_precise(&resolver(), "//package/path:target[java-output]")?
        );
        assert_eq!(
            mk_providers(
                "root",
                "package/path",
                "target",
                Some(&["FDSIcon+FDSInternal.h"]),
            ),
            ParsedPattern::parse_precise(
                &resolver(),
                "//package/path:target[FDSIcon+FDSInternal.h]",
            )?
        );

        let (package, (target_name, providers_name)) =
            ParsedPattern::parse_precise(&resolver(), "//package/path:target#flavor")?
                .as_literal("")?;
        assert_eq!(
            "root//package/path:target#flavor",
            ProvidersLabel::new(TargetLabel::new(package, target_name), providers_name).to_string()
        );
        Ok(())
    }

    #[test]
    fn parse_providers_pattern_with_alias() -> anyhow::Result<()> {
        let package = Package::new(
            resolver().resolve_self(),
            CellRelativePath::unchecked_new("package"),
        );

        let config = parse(
            &[(
                "/config",
                indoc::indoc!(
                    r#"
                    [alias]
                      foo = cell1//foo/bar:target
                    "#
                ),
            )],
            "/config",
        )?;

        assert_eq!(
            mk_providers("cell1", "foo/bar", "target", Some(&["qux"])),
            ParsedPattern::parse_relaxed(
                &config.target_alias_resolver(),
                &resolver(),
                &package,
                "foo[qux]"
            )?
        );

        Ok(())
    }

    #[test_case(PhantomData::< TargetPattern >; "parsing TargetPattern")]
    #[test_case(PhantomData::< ProvidersPattern >; "parsing ProvidersPattern")]
    fn parse_pattern_failure<T: PatternType>(_: PhantomData<T>) {
        fails(ParsedPattern::<T>::parse_precise(&resolver(), ""), &[]);
        fails(
            ParsedPattern::<T>::parse_precise(&resolver(), "//package/path"),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(&resolver(), "//package..."),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(&resolver(), "package"),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(&resolver(), "bad_alias//package/path:"),
            &[
                "bad_alias//package/path:",
                "unknown cell alias: `bad_alias`.",
            ],
        );
        fails(
            ParsedPattern::<T>::parse_precise(&resolver(), "//package/path/:target"),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(&resolver(), "//package/path/"),
            &[],
        );
    }

    #[test]
    fn bad_providers_label() {
        fails(
            ParsedPattern::<ProvidersPattern>::parse_precise(
                &resolver(),
                "//package/path:target[unclosed",
            ),
            &[
                "//package/path:target[unclosed",
                "target pattern with `[` must end with `]` to mark end of providers set label",
            ],
        );
        fails(
            ParsedPattern::<ProvidersPattern>::parse_precise(
                &resolver(),
                "//package/path:target[out]wrong",
            ),
            &[
                "//package/path:target[out]wrong",
                "target pattern with `[` must end with `]` to mark end of providers set label",
            ],
        );
    }

    #[test]
    fn parsed_pattern_contains() -> anyhow::Result<()> {
        let cell_resolver = resolver();

        let pkg1 = Package::new(
            cell_resolver.resolve_self(),
            CellRelativePath::unchecked_new("package/path"),
        );
        let pkg2 = Package::new(
            cell_resolver.resolve_self(),
            CellRelativePath::unchecked_new("package"),
        );
        let pkg3 = Package::new(
            cell_resolver.resolve_self(),
            CellRelativePath::unchecked_new("package2"),
        );
        let pkg_in_different_cell = Package::new(
            cell_resolver.resolve("cell1")?,
            CellRelativePath::unchecked_new("package/path"),
        );

        let target_in_pkg1 = TargetLabel::new(pkg1.dupe(), TargetName::new("target")?);
        let another_target_in_pkg1 = TargetLabel::new(pkg1, TargetName::new("target2")?);
        let target_in_pkg2 = TargetLabel::new(pkg2, TargetName::new("target")?);
        let target_in_pkg3 = TargetLabel::new(pkg3, TargetName::new("target")?);
        let target_in_different_cell =
            TargetLabel::new(pkg_in_different_cell, TargetName::new("target")?);

        // Testing ParsedPattern::Target

        let pattern = ParsedPattern::parse_precise(&resolver(), "//package/path:target")?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(!pattern.matches(&another_target_in_pkg1));
        assert!(!pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        // Testing ParsedPattern::Package

        let pattern = ParsedPattern::parse_precise(&resolver(), "//package/path:")?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(pattern.matches(&another_target_in_pkg1));
        assert!(!pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        let pattern = ParsedPattern::parse_precise(&resolver(), "//package:")?;
        assert!(!pattern.matches(&target_in_pkg1));
        assert!(!pattern.matches(&another_target_in_pkg1));
        assert!(pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        // Testing ParsedPattern::Recursive

        let pattern = ParsedPattern::parse_precise(&resolver(), "//package/path/...")?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(pattern.matches(&another_target_in_pkg1));
        assert!(!pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        let pattern = ParsedPattern::parse_precise(&resolver(), "//package/...")?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(pattern.matches(&another_target_in_pkg1));
        assert!(pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        let pattern = ParsedPattern::parse_precise(&resolver(), "//...")?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(pattern.matches(&another_target_in_pkg1));
        assert!(pattern.matches(&target_in_pkg2));
        assert!(pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        let pattern = ParsedPattern::parse_precise(&resolver(), "cell1//...")?;
        assert!(!pattern.matches(&target_in_pkg1));
        assert!(!pattern.matches(&another_target_in_pkg1));
        assert!(!pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(pattern.matches(&target_in_different_cell));

        Ok(())
    }

    #[derive(Clone)]
    struct TestPatternResolver {
        resolver: CellResolver,
        file_ops: Arc<dyn FileOps>,
    }

    impl TestPatternResolver {
        fn new(cells: &[(&str, &str)], files: &[&str]) -> anyhow::Result<Self> {
            let resolver = {
                let mut agg = CellsAggregator::new();
                let mut cell_paths = HashMap::new();
                for (name, path) in cells {
                    cell_paths.insert(*name, *path);
                }

                for (_, path) in cells {
                    for (alias, alias_path) in &cell_paths {
                        agg.add_cell_alias_entry(
                            ProjectRelativePathBuf::try_from((*path).to_owned())?,
                            CellAlias::new((*alias).to_owned()),
                            ProjectRelativePathBuf::try_from((*alias_path).to_owned())?,
                        )?;
                    }
                }
                agg.make_cell_resolver()?
            };

            let resolved_files = files
                .iter()
                .map(|p| {
                    (
                        resolver
                            .get_cell_path(&ProjectRelativePathBuf::unchecked_new((*p).to_owned()))
                            .unwrap(),
                        "".to_owned(),
                    )
                })
                .collect();

            let file_ops = Arc::new(TestFileOps::new_with_files(resolved_files));
            Ok(TestPatternResolver { resolver, file_ops })
        }

        async fn resolve<T>(&self, patterns: &[&str]) -> anyhow::Result<ResolvedPattern<T>>
        where
            T: PatternType,
        {
            let aliases = self
                .resolver
                .get(&CellName::unchecked_new("root".to_owned()))
                .unwrap()
                .cell_alias_resolver();

            let patterns: Vec<_> =
                patterns.map(|p| ParsedPattern::<T>::parse_precise(aliases, p).unwrap());

            resolve_target_patterns(&self.resolver, patterns.iter(), &*self.file_ops).await
        }
    }

    trait ResolvedTargetPatternTestExt<T> {
        fn assert_eq(&self, expected: &[(Package, PackageSpec<T>)]);
    }

    impl<T> ResolvedTargetPatternTestExt<T> for ResolvedPattern<T>
    where
        T: PatternType,
    {
        fn assert_eq(&self, expected: &[(Package, PackageSpec<T>)]) {
            let expected: BTreeMap<_, _> = expected.iter().map(|(p, s)| (p.dupe(), s)).collect();

            let expected_keys: BTreeSet<_> = expected.keys().collect();
            let actual_keys: BTreeSet<_> = self.specs.keys().collect();

            let missing_keys: Vec<_> = expected_keys.difference(&actual_keys).collect();
            assert!(
                missing_keys.is_empty(),
                "Expected entries for keys {:?}. Had {:?}",
                missing_keys,
                self.specs
            );

            let extra_keys: Vec<_> = actual_keys.difference(&expected_keys).collect();
            assert!(
                extra_keys.is_empty(),
                "Got unexpected keys {:?}",
                extra_keys
            );

            for (k, v) in expected {
                assert_eq!(v, self.specs.get(&k).unwrap());
            }
        }
    }

    #[tokio::test]
    async fn test_simple_specs_targets() -> anyhow::Result<()> {
        let tester = TestPatternResolver::new(&[("root", ""), ("child", "child/cell")], &[])?;
        tester.resolve::<TargetPattern>(&[]).await?.assert_eq(&[]);
        tester
            .resolve::<TargetPattern>(&[
                "//some:target",
                "//some:other_target",
                "child//a/package:",
            ])
            .await?
            .assert_eq(&[
                (
                    Package::testing_new("root", "some"),
                    PackageSpec::Targets(vec![
                        TargetName::unchecked_new("target"),
                        TargetName::unchecked_new("other_target"),
                    ]),
                ),
                (Package::testing_new("child", "a/package"), PackageSpec::All),
            ]);
        Ok(())
    }

    #[tokio::test]
    async fn test_simple_specs_providers() -> anyhow::Result<()> {
        let tester = TestPatternResolver::new(&[("root", ""), ("child", "child/cell")], &[])?;
        tester
            .resolve::<ProvidersPattern>(&[])
            .await?
            .assert_eq(&[]);
        tester
            .resolve::<ProvidersPattern>(&[
                "//some:target",
                "//some:other_target[my-label]",
                "child//a/package:",
            ])
            .await?
            .assert_eq(&[
                (
                    Package::testing_new("root", "some"),
                    PackageSpec::Targets(vec![
                        (TargetName::unchecked_new("target"), ProvidersName::Default),
                        (
                            TargetName::unchecked_new("other_target"),
                            ProvidersName::Named(vec![
                                ProviderName::new("my-label".to_owned()).unwrap(),
                            ]),
                        ),
                    ]),
                ),
                (Package::testing_new("child", "a/package"), PackageSpec::All),
            ]);
        Ok(())
    }

    #[test_case(PhantomData::< TargetPattern >; "parsing TargetPattern")]
    #[test_case(PhantomData::< ProvidersPattern >; "parsing ProvidersPattern")]
    fn test_recursive_specs<T: PatternType>(_: PhantomData<T>) {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            let tester = TestPatternResolver::new(
                &[("root", ""), ("child", "child/cell")],
                &[
                    ("BUCK"),
                    ("other/BUCK"),
                    ("other/a/bit/deeper/BUCK"),
                    ("other/a/bit/deeper/and/deeper/BUCK"),
                    ("some/thing/dir/a/BUCK"),
                    ("some/thing/dir/a/b/BUCK"),
                    ("some/thing/extra/BUCK"),
                    ("child/cell/BUCK"),
                    ("child/cell/foo/BUCK"),
                ],
            )
            .unwrap();
            tester.resolve::<T>(&[]).await.unwrap().assert_eq(&[]);
            tester
                .resolve::<T>(&[
                    "//other/...",
                    "//other:target_that_doesnt_matter",
                    "//some/...",
                    "//some/thing/extra/...",
                    // root of cell.
                    "child//...",
                ])
                .await
                .unwrap()
                .assert_eq(&[
                    (Package::testing_new("root", "other"), PackageSpec::All),
                    (
                        Package::testing_new("root", "other/a/bit/deeper"),
                        PackageSpec::All,
                    ),
                    (
                        Package::testing_new("root", "other/a/bit/deeper/and/deeper"),
                        PackageSpec::All,
                    ),
                    (
                        Package::testing_new("root", "some/thing/dir/a"),
                        PackageSpec::All,
                    ),
                    (
                        Package::testing_new("root", "some/thing/dir/a/b"),
                        PackageSpec::All,
                    ),
                    (
                        Package::testing_new("root", "some/thing/extra"),
                        PackageSpec::All,
                    ),
                    (Package::testing_new("child", ""), PackageSpec::All),
                    (Package::testing_new("child", "foo"), PackageSpec::All),
                ]);
        })
    }
}
