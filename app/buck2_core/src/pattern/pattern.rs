/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use dupe::Dupe;
use once_cell::sync::Lazy;
use pagable::Pagable;
use regex::Regex;
use relative_path::RelativePath;
use serde::Serialize;

use crate::cells::CellAliasResolver;
use crate::cells::CellResolver;
use crate::cells::alias::CellAlias;
use crate::cells::cell_path::CellPath;
use crate::cells::cell_path::CellPathCow;
use crate::cells::cell_path::CellPathRef;
use crate::cells::cell_path_with_allowed_relative_dir::CellPathWithAllowedRelativeDir;
use crate::cells::cell_root_path::CellRootPathBuf;
use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePath;
use crate::configuration::bound_label::BoundConfigurationLabel;
use crate::configuration::builtin::BuiltinPlatform;
use crate::configuration::hash::ConfigurationHash;
use crate::package::PackageLabel;
use crate::pattern::ascii_pattern::AsciiChar;
use crate::pattern::ascii_pattern::AsciiStr;
use crate::pattern::ascii_pattern::AsciiStr2;
use crate::pattern::ascii_pattern::split1_opt_ascii;
use crate::pattern::ascii_pattern::strip_suffix_ascii;
use crate::pattern::ascii_pattern::trim_prefix_ascii;
use crate::pattern::package::PackagePattern;
use crate::pattern::pattern_type::ConfigurationPredicate;
use crate::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use crate::pattern::pattern_type::PatternType;
use crate::pattern::pattern_type::ProvidersPatternExtra;
use crate::pattern::pattern_type::TargetPatternExtra;
use crate::provider::flavors::map_flavors;
use crate::provider::label::NonDefaultProvidersName;
use crate::provider::label::ProviderName;
use crate::provider::label::ProvidersLabel;
use crate::provider::label::ProvidersName;
use crate::target::label::label::TargetLabel;
use crate::target::name::TargetName;
use crate::target::name::TargetNameRef;
use crate::target_aliases::TargetAliasResolver;

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum TargetPatternParseError {
    #[error("Expected pattern to contain `:`, trailing `/...` or literal `...`.")]
    UnexpectedFormat,
    #[error("Package is empty")]
    PackageIsEmpty,
    #[error(
        "Must be absolute. Starting with either `//` for a cell alias or `:` for a relative target."
    )]
    AbsoluteRequired,
    #[error(
        "Packages may not end with a trailing `/` (except when provided on the command line where it's tolerated)"
    )]
    PackageTrailingSlash,
    #[error("Required a target literal, but got a non-literal pattern `{0}`")]
    TargetLiteralRequired(String),
    #[error(
        "You may be trying to use a macro instead of a target pattern. Macro usage is invalid here"
    )]
    PossibleMacroUsage,
    #[error("Configuration part of the pattern must be enclosed in `()`")]
    ConfigurationPartMustBeEnclosedInParentheses,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub enum ModifiersError {
    #[error("Cannot use ?modifier syntax in target pattern expression with --target-universe flag")]
    PatternModifiersWithTargetUniverse,
    #[error(
        "Cannot specify modifiers with ?modifier syntax when global CLI modifiers are set with --modifier flag"
    )]
    PatternModifiersWithGlobalModifiers,
}

pub fn display_precise_pattern<'a, T: PatternType>(
    package: &'a PackageLabel,
    target_name: &'a TargetNameRef,
    extra: &'a T,
) -> impl Display + 'a {
    #[derive(derive_more::Display)]
    #[display("{}:{}{}", package, target_name, extra)]
    struct Impl<'a, T: PatternType> {
        package: &'a PackageLabel,
        target_name: &'a TargetNameRef,
        extra: &'a T,
    }
    Impl {
        package,
        target_name,
        extra,
    }
}

/// Extract provider name from a target pattern.
pub(crate) fn split_providers_name(s: &str) -> buck2_error::Result<(&str, ProvidersName)> {
    if let Some((t, flavors)) = split1_opt_ascii(s, AsciiChar::new('#')) {
        let name = map_flavors(flavors, s)?;
        Ok((t, name))
    } else if let Some((t, p)) = split1_opt_ascii(s, AsciiChar::new('[')) {
        let mut names = Vec::new();

        let mut remaining = if let Some((p, r)) = split1_opt_ascii(p, AsciiChar::new(']')) {
            names.push(ProviderName::new(p.to_owned())?);
            r
        } else {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
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
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "target pattern with `[` must end with `]` to mark end of providers set label"
            ));
        }

        Ok((
            t,
            ProvidersName::NonDefault(triomphe::Arc::new(NonDefaultProvidersName::Named(
                buck2_util::arc_str::ArcSlice::from_iter(names),
            ))),
        ))
    } else {
        Ok((s, ProvidersName::Default))
    }
}

#[derive(Dupe, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Modifiers(Option<Arc<[String]>>);

impl Modifiers {
    pub fn new(modifiers: Option<Vec<String>>) -> Self {
        Self(modifiers.map(|m| m.into()))
    }

    pub fn parse(string: &str) -> Self {
        Self::new(Some(string.split("+").map(String::from).collect()))
    }

    pub fn as_slice(&self) -> Option<&[String]> {
        self.0.as_deref()
    }
}

#[derive(Dupe, Clone, Eq, PartialEq, Hash)]
pub struct ProvidersLabelWithModifiers {
    pub providers_label: ProvidersLabel,
    pub modifiers: Modifiers,
}

#[derive(Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Dupe, Clone)]
pub struct TargetLabelWithModifiers {
    pub target_label: TargetLabel,
    pub modifiers: Modifiers,
}

impl Display for TargetLabelWithModifiers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(modifiers) = self.modifiers.as_slice() {
            write!(f, "{}?{}", self.target_label, modifiers.join("+"))
        } else {
            write!(f, "{}", self.target_label)
        }
    }
}

impl Serialize for TargetLabelWithModifiers {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

/// All possible labels.
/// - target label
/// - configured target label
/// - providers label
/// - configured providers label
pub struct TargetLabelWithExtra<T: PatternType> {
    pub target_label: TargetLabel,
    pub extra: T,
    pub modifiers: Modifiers,
}

impl TargetLabelWithExtra<TargetPatternExtra> {
    pub fn into_target_label(self) -> TargetLabel {
        self.target_label
    }
}

impl TargetLabelWithExtra<ProvidersPatternExtra> {
    pub fn into_providers_label(self) -> ProvidersLabel {
        ProvidersLabel::new(self.target_label, self.extra.providers)
    }

    pub fn into_providers_label_with_modifiers(self) -> ProvidersLabelWithModifiers {
        ProvidersLabelWithModifiers {
            providers_label: ProvidersLabel::new(self.target_label, self.extra.providers),
            modifiers: self.modifiers,
        }
    }
}

/// A parsed target pattern.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Allocative, Pagable)]
pub enum ParsedPattern<T: PatternType> {
    /// A target pattern that matches a explicit target pattern type T. See
    /// `PatternType` for pattern
    Target(PackageLabel, TargetName, T),
    /// A target pattern that matches an entire package. Ex. `//some/package:`
    Package(PackageLabel),
    /// A target pattern that matches all recursive packages. Ex.
    /// `//some/package/...`. The path component here is not required to be
    /// an actual package (i.e. a build file is not required at the path)
    /// and so we don't hold this as a [PackageLabel].
    Recursive(CellPath),
}

impl ParsedPattern<TargetPatternExtra> {
    /// Extract [`TargetLabel`] from a [`ParsedPattern`].
    pub fn as_target_label(self, original: &str) -> buck2_error::Result<TargetLabel> {
        let (target_label, TargetPatternExtra) = self.as_literal(original)?;
        Ok(target_label)
    }

    /// Check if a [`ParsedPattern`] matches a [`TargetLabel`]
    pub fn matches(&self, target: &TargetLabel) -> bool {
        let target_pkg = target.pkg();
        match self {
            ParsedPattern::Target(pkg, t, TargetPatternExtra) => {
                *pkg == target_pkg && t.as_ref() == target.name()
            }
            ParsedPattern::Package(pkg) => target_pkg.as_cell_path() == pkg.as_cell_path(),
            ParsedPattern::Recursive(cell_path) => {
                target_pkg.as_cell_path().starts_with(cell_path.as_ref())
            }
        }
    }
}

impl ParsedPattern<ProvidersPatternExtra> {
    /// Extract [`ProvidersLabel`] from a [`ParsedPattern`].
    pub fn as_providers_label(self, original: &str) -> buck2_error::Result<ProvidersLabel> {
        let (target_label, ProvidersPatternExtra { providers }) = self.as_literal(original)?;
        Ok(ProvidersLabel::new(target_label, providers))
    }
}

impl<T: PatternType> ParsedPattern<T> {
    pub fn try_map<U: PatternType>(
        self,
        f: impl FnOnce(T) -> buck2_error::Result<U>,
    ) -> buck2_error::Result<ParsedPattern<U>> {
        match self {
            ParsedPattern::Target(package, target_name, val) => {
                Ok(ParsedPattern::Target(package, target_name, f(val)?))
            }
            ParsedPattern::Package(package) => Ok(ParsedPattern::Package(package)),
            ParsedPattern::Recursive(cell_path) => Ok(ParsedPattern::Recursive(cell_path)),
        }
    }

    pub fn map<U: PatternType>(self, f: impl FnOnce(T) -> U) -> ParsedPattern<U> {
        match self {
            ParsedPattern::Target(package, target_name, val) => {
                ParsedPattern::Target(package, target_name, f(val))
            }
            ParsedPattern::Package(package) => ParsedPattern::Package(package),
            ParsedPattern::Recursive(cell_path) => ParsedPattern::Recursive(cell_path),
        }
    }

    pub fn into_package_pattern_ignore_target(self) -> PackagePattern {
        match self {
            ParsedPattern::Target(p, ..) | ParsedPattern::Package(p) => PackagePattern::Package(p),
            ParsedPattern::Recursive(p) => PackagePattern::Recursive(p),
        }
    }

    /// Extract a literal from a [ParsedPattern], or `Err` if it is not a literal.
    pub fn as_literal(self, original: &str) -> buck2_error::Result<(TargetLabel, T)> {
        // FIXME: Would be better if we had a Display on self, so we could produce a nice error message.
        //        For now, just require the original string to be passed in for good errors.
        match self {
            ParsedPattern::Target(package, target_name, val) => {
                Ok((TargetLabel::new(package, target_name.as_ref()), val))
            }
            _ => Err(TargetPatternParseError::TargetLiteralRequired(original.to_owned()).into()),
        }
    }

    /// `parse_not_relaxed` with `TargetParsingRel::RequiresAbsolute`
    pub fn parse_precise(
        pattern: &str,
        cell: CellName,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<Self> {
        let pattern_with_modifiers = parse_target_pattern(
            cell_resolver,
            cell_alias_resolver,
            TargetParsingOptions {
                relative: TargetParsingRel::RequireAbsolute(cell),
                infer_target: false,
                strip_package_trailing_slash: false,
            },
            pattern,
        )
        .with_buck_error_context(|| {
            format!("Invalid absolute target pattern `{pattern}` is not allowed")
        })?;

        Self::from_parsed_pattern_with_modifiers(pattern_with_modifiers)
    }

    pub fn parse_not_relaxed(
        pattern: &str,
        relative: TargetParsingRel<'_>,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<Self> {
        let pattern_with_modifiers = parse_target_pattern(
            cell_resolver,
            cell_alias_resolver,
            TargetParsingOptions {
                relative,
                infer_target: false,
                strip_package_trailing_slash: false,
            },
            pattern,
        )
        .with_buck_error_context(|| format!("Invalid target pattern `{pattern}` is not allowed"))?;

        Self::from_parsed_pattern_with_modifiers(pattern_with_modifiers)
    }

    /// Parse a TargetPattern out, resolving aliases via `cell_resolver`, resolving relative
    /// targets via `relative_dir`, inferring a target name if no target or recursive pattern
    /// is provided (e.g. `//foo/bar` is inferred to be equivalent to `//foo/bar:bar`), and
    /// stripping trailing `/` in package names instead of rejecting them.
    ///
    /// This should only be used with user-provided command line arguments, as precision is
    /// generally preferred elsewhere.
    pub fn parse_relaxed(
        target_alias_resolver: &dyn TargetAliasResolver,
        relative_dir: CellPathRef,
        pattern: &str,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<Self> {
        let pattern_with_modifiers = parse_target_pattern(
            cell_resolver,
            cell_alias_resolver,
            TargetParsingOptions {
                relative: TargetParsingRel::AllowRelative(
                    &CellPathWithAllowedRelativeDir::backwards_relative_not_supported(
                        relative_dir.to_owned(),
                    ),
                    Some(target_alias_resolver),
                ),
                infer_target: true,
                strip_package_trailing_slash: true,
            },
            pattern,
        )
        .with_buck_error_context(|| format!("Parsing target pattern `{pattern}`"))?;

        Self::from_parsed_pattern_with_modifiers(pattern_with_modifiers)
    }

    fn from_parsed_pattern_with_modifiers(
        pattern_with_modifiers: ParsedPatternWithModifiers<T>,
    ) -> buck2_error::Result<Self> {
        let ParsedPatternWithModifiers {
            parsed_pattern,
            modifiers,
        } = pattern_with_modifiers;

        match modifiers.as_slice() {
            None => Ok(parsed_pattern),
            Some(_) => Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "The ?modifier syntax is unsupported for this command"
            )),
        }
    }

    pub fn testing_parse(pattern: &str) -> Self {
        let cell_name = pattern.split_once("//").unwrap().0;
        let cell_name = CellName::testing_new(cell_name);
        let cell_resolver =
            CellResolver::testing_with_name_and_path(cell_name, CellRootPathBuf::testing_new(""));
        Self::parse_precise(
            pattern,
            cell_name,
            &cell_resolver,
            cell_resolver.root_cell_cell_alias_resolver(),
        )
        .unwrap()
    }
}

impl<T: PatternType> Display for ParsedPattern<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsedPattern::Target(package, target_name, pattern) => {
                write!(
                    f,
                    "{}",
                    display_precise_pattern(package, target_name.as_ref(), pattern)
                )
            }
            ParsedPattern::Package(package) => {
                write!(f, "{}:", package.as_cell_path())
            }
            ParsedPattern::Recursive(path) => {
                if path.path().is_empty() {
                    write!(f, "{path}...")
                } else {
                    write!(f, "{path}/...")
                }
            }
        }
    }
}

pub struct ParsedPatternWithModifiers<T: PatternType> {
    pub parsed_pattern: ParsedPattern<T>,
    pub modifiers: Modifiers,
}

impl<T: PatternType> ParsedPatternWithModifiers<T> {
    pub fn parse_precise(
        pattern: &str,
        cell: CellName,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<Self> {
        parse_target_pattern(
            cell_resolver,
            cell_alias_resolver,
            TargetParsingOptions {
                relative: TargetParsingRel::RequireAbsolute(cell),
                infer_target: false,
                strip_package_trailing_slash: false,
            },
            pattern,
        )
        .with_buck_error_context(|| {
            format!("Invalid absolute target pattern `{pattern}` is not allowed")
        })
    }

    pub fn parse_relaxed(
        target_alias_resolver: &dyn TargetAliasResolver,
        relative_dir: CellPathRef,
        pattern: &str,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<Self> {
        parse_target_pattern(
            cell_resolver,
            cell_alias_resolver,
            TargetParsingOptions {
                relative: TargetParsingRel::AllowRelative(
                    &CellPathWithAllowedRelativeDir::backwards_relative_not_supported(
                        relative_dir.to_owned(),
                    ),
                    Some(target_alias_resolver),
                ),
                infer_target: true,
                strip_package_trailing_slash: true,
            },
            pattern,
        )
        .with_buck_error_context(|| format!("Parsing target pattern `{pattern}`"))
    }

    pub fn parse_not_relaxed(
        pattern: &str,
        relative: TargetParsingRel<'_>,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<Self> {
        parse_target_pattern(
            cell_resolver,
            cell_alias_resolver,
            TargetParsingOptions {
                relative,
                infer_target: false,
                strip_package_trailing_slash: false,
            },
            pattern,
        )
        .with_buck_error_context(|| format!("Invalid target pattern `{pattern}` is not allowed"))
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Allocative)]
pub enum ParsedPatternPredicate<T: PatternType> {
    Any,
    AnyOf(Vec<ParsedPattern<T>>),
}

impl ParsedPatternPredicate<TargetPatternExtra> {
    pub fn matches(&self, target: &TargetLabel) -> bool {
        match self {
            ParsedPatternPredicate::Any => true,
            ParsedPatternPredicate::AnyOf(patterns) => {
                patterns.iter().any(|pattern| pattern.matches(target))
            }
        }
    }
}

#[derive(Debug)]
pub struct PatternParts<'a, T: PatternType> {
    /// Is there a `foo//` or `//` part.
    pub cell_alias: Option<&'a str>,
    pub pattern: PatternDataOrAmbiguous<'a, T>,
}

impl<'a, T: PatternType> PatternParts<'a, T> {
    fn try_map<U: PatternType, F: FnOnce(T) -> buck2_error::Result<U>>(
        self,
        f: F,
    ) -> buck2_error::Result<PatternParts<'a, U>> {
        let PatternParts {
            cell_alias,
            pattern,
        } = self;
        Ok(PatternParts {
            cell_alias,
            pattern: pattern.try_map(f)?,
        })
    }
}

#[derive(Debug, derive_more::From)]
pub enum PatternDataOrAmbiguous<'a, T: PatternType> {
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
        extra: T,
        modifiers: Modifiers,
    },
}

impl<'a, T: PatternType> PatternDataOrAmbiguous<'a, T> {
    fn try_map<U: PatternType>(
        self,
        f: impl FnOnce(T) -> buck2_error::Result<U>,
    ) -> buck2_error::Result<PatternDataOrAmbiguous<'a, U>> {
        match self {
            PatternDataOrAmbiguous::PatternData(d) => {
                Ok(PatternDataOrAmbiguous::PatternData(d.try_map(f)?))
            }
            PatternDataOrAmbiguous::Ambiguous {
                pattern,
                strip_package_trailing_slash,
                extra,
                modifiers,
            } => Ok(PatternDataOrAmbiguous::Ambiguous {
                pattern,
                strip_package_trailing_slash,
                extra: f(extra)?,
                modifiers,
            }),
        }
    }

    pub fn modifiers(&self) -> Modifiers {
        match self {
            PatternDataOrAmbiguous::PatternData(d) => d.modifiers(),
            PatternDataOrAmbiguous::Ambiguous { modifiers, .. } => modifiers.dupe(),
        }
    }
}

impl<'a, T> PatternDataOrAmbiguous<'a, T>
where
    T: PatternType,
{
    /// If the pattern is ambiguous, try to infer a target. This would convert `foo/bar` into
    /// `foo/bar:bar`.
    pub fn infer_target(self) -> buck2_error::Result<PatternData<'a, T>> {
        match self {
            Self::PatternData(d) => Ok(d),
            Self::Ambiguous {
                pattern,
                strip_package_trailing_slash,
                extra,
                modifiers,
            } => {
                let package = normalize_package(pattern, strip_package_trailing_slash)?;

                let target = package
                    .file_name()
                    .ok_or(TargetPatternParseError::PackageIsEmpty)?;

                let target_name = TargetName::new(target)?;

                Ok(PatternData::TargetInPackage {
                    package,
                    target_name,
                    extra,
                    modifiers,
                })
            }
        }
    }

    /// If the pattern is ambiguous, error out.
    pub fn reject_ambiguity(self) -> buck2_error::Result<PatternData<'a, T>> {
        match self {
            Self::PatternData(d) => Ok(d),
            Self::Ambiguous { pattern, .. } => {
                // Check if the user maybe tried to use a macro
                if pattern.contains('$')
                    && pattern.contains(' ')
                    && pattern.contains('(')
                    && pattern.contains(')')
                {
                    return Err(TargetPatternParseError::PossibleMacroUsage.into());
                }
                Err(TargetPatternParseError::UnexpectedFormat.into())
            }
        }
    }
}

/// The pattern data we extracted.
#[derive(Debug)]
pub enum PatternData<'a, T: PatternType> {
    /// A pattern like `foo/bar/...`.
    Recursive {
        package: &'a RelativePath,
        modifiers: Modifiers,
    },

    /// A pattern like `foo/bar:`, or `:`
    AllTargetsInPackage {
        package: &'a RelativePath,
        modifiers: Modifiers,
    },

    /// A pattern like `foo/bar:qux`, or `:qux`. The target will never be empty.
    TargetInPackage {
        package: &'a RelativePath,
        target_name: TargetName,
        extra: T,
        modifiers: Modifiers,
    },
}

impl<'a, T: PatternType> PatternData<'a, T> {
    fn try_map<U: PatternType>(
        self,
        f: impl FnOnce(T) -> buck2_error::Result<U>,
    ) -> buck2_error::Result<PatternData<'a, U>> {
        match self {
            PatternData::Recursive { package, modifiers } => {
                Ok(PatternData::Recursive { package, modifiers })
            }
            PatternData::AllTargetsInPackage { package, modifiers } => {
                Ok(PatternData::AllTargetsInPackage { package, modifiers })
            }
            PatternData::TargetInPackage {
                package,
                target_name,
                extra,
                modifiers,
            } => Ok(PatternData::TargetInPackage {
                package,
                target_name,
                extra: f(extra)?,
                modifiers,
            }),
        }
    }

    pub fn package_path(&self) -> &'a RelativePath {
        match self {
            Self::Recursive { package, .. } => package,
            Self::AllTargetsInPackage { package, .. } => package,
            Self::TargetInPackage { package, .. } => package,
        }
    }

    pub fn target(&self) -> Option<(&TargetName, &T)> {
        match self {
            Self::Recursive { .. } => None,
            Self::AllTargetsInPackage { .. } => None,
            Self::TargetInPackage {
                target_name, extra, ..
            } => Some((target_name, extra)),
        }
    }

    pub fn modifiers(&self) -> Modifiers {
        match self {
            Self::Recursive { modifiers, .. } => modifiers.dupe(),
            Self::AllTargetsInPackage { modifiers, .. } => modifiers.dupe(),
            Self::TargetInPackage { modifiers, .. } => modifiers.dupe(),
        }
    }

    /// Whether this is a target that looks like `:target`.
    pub fn is_adjacent_target(&self) -> bool {
        self.package_path().as_str().is_empty() && self.target().is_some()
    }
}

// Splits a pattern into cell alias and forward relative path if "//" is present, otherwise returns None,
pub fn maybe_split_cell_alias_and_relative_path(
    pattern: &str,
) -> buck2_error::Result<Option<(CellAlias, &ForwardRelativePath)>> {
    Ok(match split1_opt_ascii(pattern, AsciiStr2::new("//")) {
        Some((a, p)) => Some((
            CellAlias::new(trim_prefix_ascii(a, AsciiChar::new('@')).to_owned()),
            ForwardRelativePath::new(p)?,
        )),
        None => None,
    })
}

fn lex_provider_pattern(
    pattern: &str,
    strip_package_trailing_slash: bool,
) -> buck2_error::Result<PatternParts<'_, ProvidersPatternExtra>> {
    let (cell_alias, pattern) = match split1_opt_ascii(pattern, AsciiStr2::new("//")) {
        Some((a, p)) => (Some(trim_prefix_ascii(a, AsciiChar::new('@'))), p),
        None => (None, pattern),
    };

    if pattern.chars().filter(|&c| c == '?').count() > 1 {
        return Err(buck2_error!(
            buck2_error::ErrorTag::Input,
            "Expected at most one ? in pattern, question marks in file, target, modifier, or cell names are not supported",
        ));
    }

    let (pattern, modifiers) = match split1_opt_ascii(pattern, AsciiChar::new('?')) {
        Some((pattern, modifiers)) => (pattern, Modifiers::parse(modifiers)),
        None => (pattern, Modifiers::new(None)),
    };

    let pattern = match split1_opt_ascii(pattern, AsciiChar::new(':')) {
        Some((package, "")) => PatternData::AllTargetsInPackage {
            package: normalize_package(package, strip_package_trailing_slash)?,
            modifiers,
        }
        .into(),
        Some((package, target)) => {
            let (target, providers) = split_providers_name(target)?;
            let target_name = TargetName::new(target)?;
            let extra = ProvidersPatternExtra { providers };
            PatternData::TargetInPackage {
                package: normalize_package(package, strip_package_trailing_slash)?,
                target_name,
                extra,
                modifiers,
            }
            .into()
        }
        None => {
            if let Some(package) = strip_suffix_ascii(pattern, AsciiStr::new("/...")) {
                PatternData::Recursive {
                    package: RelativePath::new(package),
                    modifiers,
                }
                .into()
            } else if pattern == "..." {
                PatternData::Recursive {
                    package: RelativePath::new(""),
                    modifiers,
                }
                .into()
            } else if !pattern.is_empty() {
                let (pattern, providers) = split_providers_name(pattern)?;
                PatternDataOrAmbiguous::Ambiguous {
                    pattern,
                    strip_package_trailing_slash,
                    extra: ProvidersPatternExtra { providers },
                    modifiers,
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

fn lex_configuration_predicate(pattern: &str) -> buck2_error::Result<ConfigurationPredicate> {
    let pattern = pattern
        .strip_prefix('(')
        .ok_or(TargetPatternParseError::ConfigurationPartMustBeEnclosedInParentheses)?;
    let pattern = pattern
        .strip_suffix(')')
        .ok_or(TargetPatternParseError::ConfigurationPartMustBeEnclosedInParentheses)?;
    match pattern.split_once('#') {
        Some((cfg, hash)) => {
            let cfg = BoundConfigurationLabel::new(cfg.to_owned())?;
            let hash = ConfigurationHash::from_str(hash)?;
            Ok(ConfigurationPredicate::Bound(cfg, Some(hash)))
        }
        None => {
            if let Some(builtin) = BuiltinPlatform::from_label(pattern) {
                Ok(ConfigurationPredicate::Builtin(builtin))
            } else {
                Ok(ConfigurationPredicate::Bound(
                    BoundConfigurationLabel::new(pattern.to_owned())?,
                    None,
                ))
            }
        }
    }
}

/// Split target pattern and configuration preserving parentheses for better diagnostics.
fn split_cfg(s: &str) -> Option<(&str, &str)> {
    // Fast path.
    if !s.contains(' ') {
        return None;
    }

    let mut braces: u32 = 0;
    for (i, c) in s.char_indices() {
        match c {
            '(' => braces += 1,
            ')' => match braces.checked_sub(1) {
                Some(b) => braces = b,
                None => {
                    // Pattern is invalid, let parser fail elsewhere.
                    return None;
                }
            },
            ' ' if braces == 0 => return Some((&s[..i], &s[i + 1..])),
            _ => {}
        }
    }
    None
}

pub fn lex_configured_providers_pattern(
    pattern: &str,
    strip_package_trailing_slash: bool,
) -> buck2_error::Result<PatternParts<'_, ConfiguredProvidersPatternExtra>> {
    let (provider_pattern, cfg) = match split_cfg(pattern) {
        Some((providers, cfg)) => {
            let provider_pattern = lex_provider_pattern(providers, strip_package_trailing_slash)?;
            let cfg = lex_configuration_predicate(cfg)?;
            (provider_pattern, cfg)
        }
        None => (
            lex_provider_pattern(pattern, strip_package_trailing_slash)?,
            ConfigurationPredicate::Any,
        ),
    };

    let has_modifiers = match &provider_pattern.pattern {
        PatternDataOrAmbiguous::PatternData(d) => d.modifiers().as_slice().is_some(),
        PatternDataOrAmbiguous::Ambiguous { modifiers, .. } => modifiers.as_slice().is_some(),
    };

    if has_modifiers {
        match &cfg {
            ConfigurationPredicate::Bound(_, _) => {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "Modifiers incompatible with explicit configuration",
                ));
            }
            _ => {
                // Allow modifiers with Any or Builtin configuration predicates
            }
        }
    }

    provider_pattern.try_map(|ProvidersPatternExtra { providers }| {
        Ok(ConfiguredProvidersPatternExtra { providers, cfg })
    })
}

// Lex the target pattern into the relevant pieces.
pub fn lex_target_pattern<T: PatternType>(
    pattern: &str,
    strip_package_trailing_slash: bool,
) -> buck2_error::Result<PatternParts<'_, T>> {
    let provider_pattern = lex_configured_providers_pattern(pattern, strip_package_trailing_slash)?;
    provider_pattern
        .try_map(|extra| T::from_configured_providers(extra))
        .with_buck_error_context(|| {
            format!(
                "Expecting {} pattern, got: `{}`",
                // This can only fail when `PatternType = TargetName`, so the message is correct.
                T::NAME,
                pattern.to_owned(),
            )
        })
}

fn normalize_package(
    package: &str,
    strip_package_trailing_slash: bool,
) -> buck2_error::Result<&RelativePath> {
    // Strip or reject trailing `/`, such as in `foo/:bar`.
    if let Some(stripped) = strip_suffix_ascii(package, AsciiChar::new('/')) {
        if strip_package_trailing_slash {
            return Ok(RelativePath::new(stripped));
        } else {
            return Err(buck2_error::Error::from(
                TargetPatternParseError::PackageTrailingSlash,
            ));
        }
    }

    Ok(RelativePath::new(package))
}

#[derive(Clone, Dupe)]
pub enum TargetParsingRel<'a> {
    /// Parse the pattern relative to this package path
    AllowRelative(
        &'a CellPathWithAllowedRelativeDir,
        Option<&'a dyn TargetAliasResolver>,
    ),
    /// Allows relative patterns, but only if they're like `:foo`, not `bar:foo`
    AllowLimitedRelative(CellPathRef<'a>),
    /// Require the pattern to be absolute.
    ///
    /// Still allows reference to the self-cell (`//foo:bar`)
    RequireAbsolute(CellName),
}

impl<'a> TargetParsingRel<'a> {
    fn dir(&self) -> Option<CellPathRef<'a>> {
        match self {
            TargetParsingRel::AllowRelative(dir, _) => Some(dir.current_dir().as_ref()),
            TargetParsingRel::AllowLimitedRelative(dir) => Some(*dir),
            TargetParsingRel::RequireAbsolute(_) => None,
        }
    }

    fn allow_full_relative(&self) -> bool {
        match self {
            TargetParsingRel::AllowRelative(_, _) => true,
            TargetParsingRel::AllowLimitedRelative(_) => false,
            TargetParsingRel::RequireAbsolute(_) => false,
        }
    }

    fn target_alias_resolver(&self) -> Option<&dyn TargetAliasResolver> {
        match self {
            TargetParsingRel::AllowRelative(_, r) => *r,
            TargetParsingRel::AllowLimitedRelative(_) => None,
            TargetParsingRel::RequireAbsolute(_) => None,
        }
    }

    fn cell(&self) -> CellName {
        match self {
            TargetParsingRel::AllowRelative(p, _) => p.current_dir().cell(),
            TargetParsingRel::AllowLimitedRelative(p) => p.cell(),
            TargetParsingRel::RequireAbsolute(c) => *c,
        }
    }

    fn join_relative(&self, path: &RelativePath) -> buck2_error::Result<CellPath> {
        match self {
            TargetParsingRel::AllowRelative(dir, _) => Ok(dir.join_normalized(path)?),
            TargetParsingRel::AllowLimitedRelative(dir) => {
                Ok(dir.join(<&ForwardRelativePath>::try_from(path)?))
            }
            TargetParsingRel::RequireAbsolute(_) => Err(buck2_error::Error::from(
                TargetPatternParseError::AbsoluteRequired,
            )),
        }
    }
}

#[derive(Clone, Dupe)]
struct TargetParsingOptions<'a> {
    relative: TargetParsingRel<'a>,
    /// Whether to infer the target in a pattern such as `foo/bar` (to `foo/bar:bar`).
    infer_target: bool,
    /// Whether to strip trailing slashes in package names, in e.g. `foo/bar/` or `foo/bar/:qux`.
    /// If not set, trailing slashes are an error. Note that this happens before target inference
    /// (if enabled), so e.g. `foo/bar/` becomes `foo/bar:bar`.
    strip_package_trailing_slash: bool,
}

/// Parse a TargetPattern out, resolving aliases via `cell_resolver`, and resolving relative
/// targets via `enclosing_package`, if provided.
fn parse_target_pattern<T>(
    cell_resolver: &CellResolver,
    cell_alias_resolver: &CellAliasResolver,
    opts: TargetParsingOptions,
    pattern: &str,
) -> buck2_error::Result<ParsedPatternWithModifiers<T>>
where
    T: PatternType,
{
    parse_target_pattern_no_validate::<T>(cell_resolver, cell_alias_resolver, opts, pattern)
        .tag(buck2_error::ErrorTag::Input)
}

fn parse_target_pattern_no_validate<T>(
    cell_resolver: &CellResolver,
    cell_alias_resolver: &CellAliasResolver,
    opts: TargetParsingOptions,
    pattern: &str,
) -> buck2_error::Result<ParsedPatternWithModifiers<T>>
where
    T: PatternType,
{
    let TargetParsingOptions {
        relative,
        infer_target,
        strip_package_trailing_slash,
    } = opts;

    let lex = lex_target_pattern(pattern, strip_package_trailing_slash)?;

    if let Some(target_alias_resolver) = relative.target_alias_resolver() {
        if let Some(aliased) = resolve_target_alias(
            relative.cell(),
            cell_resolver,
            cell_alias_resolver,
            target_alias_resolver,
            &lex,
        )? {
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
    let is_absolute_or_adjacent = cell_alias.is_some() || pattern.is_adjacent_target();
    if !relative.allow_full_relative() && !is_absolute_or_adjacent {
        return Err(TargetPatternParseError::AbsoluteRequired.into());
    }

    // Prohibit parsing `:foo` as `root//:foo`.
    if relative.dir().is_none() && cell_alias.is_none() {
        return Err(TargetPatternParseError::AbsoluteRequired.into());
    }

    // We ask for the cell, but if the pattern is relative we might not use it
    let cell = cell_alias_resolver.resolve(cell_alias.unwrap_or_default())?;

    let package_path = pattern.package_path();

    let path = match relative.dir() {
        Some(_) if cell_alias.is_none() => {
            CellPathCow::Owned(relative.join_relative(package_path)?)
        }
        _ => CellPathCow::Borrowed(CellPathRef::new(
            cell,
            CellRelativePath::new(<&ForwardRelativePath>::try_from(package_path)?),
        )),
    };

    let modifiers = pattern.modifiers().clone();

    let parsed_pattern = match pattern {
        PatternData::Recursive { .. } => ParsedPattern::Recursive(path.into_owned()),
        PatternData::AllTargetsInPackage { .. } => {
            ParsedPattern::Package(PackageLabel::from_cell_path(path.as_ref())?)
        }
        PatternData::TargetInPackage {
            target_name, extra, ..
        } => ParsedPattern::Target(
            PackageLabel::from_cell_path(path.as_ref())?,
            target_name,
            extra,
        ),
    };

    Ok(ParsedPatternWithModifiers {
        parsed_pattern,
        modifiers,
    })
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tier0)]
enum ResolveTargetAliasError {
    #[error("Invalid alias: `{}`", alias)]
    InvalidAlias { alias: String },

    #[error("Alias for `{}` is not a target: `{}`", target, alias)]
    AliasIsNotATarget { target: String, alias: String },
}

fn resolve_target_alias<T>(
    cell_name: CellName,
    cell_resolver: &CellResolver,
    cell_alias_resolver: &CellAliasResolver,
    target_alias_resolver: &dyn TargetAliasResolver,
    lex: &PatternParts<T>,
) -> buck2_error::Result<Option<ParsedPatternWithModifiers<T>>>
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
    let (target, extra, modifiers) = match &lex.pattern {
        PatternDataOrAmbiguous::Ambiguous {
            pattern,
            extra,
            modifiers,
            ..
        } => (*pattern, extra, modifiers.clone()),
        _ => return Ok(None),
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
    let res = parse_target_pattern::<TargetPatternExtra>(
        cell_resolver,
        cell_alias_resolver,
        TargetParsingOptions {
            relative: TargetParsingRel::RequireAbsolute(cell_name),
            infer_target: false,
            strip_package_trailing_slash: false,
        },
        alias,
    )
    .with_buck_error_context(|| format!("Error dereferencing alias `{}` -> `{}`", target, alias))?;

    // And finally, put the `T` we were looking for back together.
    let parsed_pattern = match res.parsed_pattern {
        ParsedPattern::Target(package, target_name, TargetPatternExtra) => {
            ParsedPattern::Target(package, target_name, extra.clone())
        }
        _ => {
            return Err(ResolveTargetAliasError::AliasIsNotATarget {
                target: target.to_owned(),
                alias: alias.to_owned(),
            }
            .into());
        }
    };

    Ok(Some(ParsedPatternWithModifiers {
        parsed_pattern,
        modifiers,
    }))
}

#[derive(Debug, Eq, PartialEq)]
pub enum PackageSpec<T: PatternType> {
    /// Given targets in a package.
    Targets(Vec<(TargetName, T)>),
    /// All targets in a package, without subpackages.
    /// Syntax for this variant is `foo:`.
    All(),
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::marker::PhantomData;

    use assert_matches::assert_matches;
    use dupe::Dupe;
    use gazebo::prelude::*;
    use test_case::test_case;

    use crate::cells::CellAliasResolver;
    use crate::cells::CellResolver;
    use crate::cells::alias::NonEmptyCellAlias;
    use crate::cells::cell_path::CellPath;
    use crate::cells::cell_path_with_allowed_relative_dir::CellPathWithAllowedRelativeDir;
    use crate::cells::cell_root_path::CellRootPathBuf;
    use crate::cells::name::CellName;
    use crate::cells::paths::CellRelativePath;
    use crate::cells::paths::CellRelativePathBuf;
    use crate::configuration::bound_label::BoundConfigurationLabel;
    use crate::configuration::builtin::BuiltinPlatform;
    use crate::configuration::hash::ConfigurationHash;
    use crate::package::PackageLabel;
    use crate::pattern::pattern::Modifiers;
    use crate::pattern::pattern::ParsedPattern;
    use crate::pattern::pattern::ParsedPatternWithModifiers;
    use crate::pattern::pattern::TargetParsingRel;
    use crate::pattern::pattern::TargetPatternParseError;
    use crate::pattern::pattern_type::ConfigurationPredicate;
    use crate::pattern::pattern_type::ConfiguredProvidersPatternExtra;
    use crate::pattern::pattern_type::ConfiguredTargetPatternExtra;
    use crate::pattern::pattern_type::PatternType;
    use crate::pattern::pattern_type::ProvidersPatternExtra;
    use crate::pattern::pattern_type::TargetPatternExtra;
    use crate::provider::label::NonDefaultProvidersName;
    use crate::provider::label::ProviderName;
    use crate::provider::label::ProvidersName;
    use crate::target::label::label::TargetLabel;
    use crate::target::name::TargetName;
    use crate::target::name::TargetNameRef;
    use crate::target_aliases::TargetAliasResolver;

    fn mk_package<P: PatternType>(cell: &str, path: &str) -> ParsedPattern<P> {
        ParsedPattern::Package(PackageLabel::testing_new(cell, path))
    }

    fn mk_recursive<P: PatternType>(cell: &str, path: &str) -> ParsedPattern<P> {
        ParsedPattern::Recursive(CellPath::new(
            CellName::testing_new(cell),
            CellRelativePathBuf::unchecked_new(path.to_owned()),
        ))
    }

    fn mk_target(cell: &str, path: &str, target: &str) -> ParsedPattern<TargetPatternExtra> {
        ParsedPattern::Target(
            PackageLabel::testing_new(cell, path),
            TargetName::testing_new(target),
            TargetPatternExtra,
        )
    }

    fn mk_providers(
        cell: &str,
        path: &str,
        target: &str,
        providers: Option<&[&str]>,
    ) -> ParsedPattern<ProvidersPatternExtra> {
        ParsedPattern::Target(
            PackageLabel::testing_new(cell, path),
            TargetName::testing_new(target),
            ProvidersPatternExtra {
                providers: providers.map_or(ProvidersName::Default, |n| {
                    ProvidersName::NonDefault(triomphe::Arc::new(NonDefaultProvidersName::Named(
                        buck2_util::arc_str::ArcSlice::from_iter(
                            n.map(|s| ProviderName::new((*s).to_owned()).unwrap()),
                        ),
                    )))
                }),
            },
        )
    }

    fn mk_configured_providers(
        cell: &str,
        path: &str,
        target: &str,
        providers: Option<&[&str]>,
        cfg: ConfigurationPredicate,
    ) -> ParsedPattern<ConfiguredProvidersPatternExtra> {
        mk_providers(cell, path, target, providers)
            .try_map(|ProvidersPatternExtra { providers }| {
                Ok(ConfiguredProvidersPatternExtra { providers, cfg })
            })
            .unwrap()
    }

    fn fails<R>(x: buck2_error::Result<R>, msgs: &[&str]) {
        match x {
            Err(e) => {
                let s = format!("{e:#}");
                for msg in msgs {
                    if !s.contains(msg) {
                        panic!("Expected `{msg}` but missing from error `{e:#}`")
                    }
                }
            }
            Ok(_) => panic!("Expected failure but succeeded"),
        }
    }

    struct NoAliases;

    impl TargetAliasResolver for NoAliases {
        fn get<'a>(&'a self, _name: &str) -> buck2_error::Result<Option<&'a str>> {
            Ok(None)
        }
    }

    fn aliases(aliases: &[(&str, &str)]) -> impl TargetAliasResolver + use<> {
        struct Aliases(Vec<(String, String)>);

        impl TargetAliasResolver for Aliases {
            fn get<'a>(&'a self, name: &str) -> buck2_error::Result<Option<&'a str>> {
                Ok(self
                    .0
                    .iter()
                    .find(|(a, _)| *a == name)
                    .map(|(_, b)| b.as_str()))
            }
        }

        Aliases(
            aliases
                .iter()
                .map(|(a, b)| ((*a).to_owned(), (*b).to_owned()))
                .collect(),
        )
    }

    fn resolver() -> CellResolver {
        CellResolver::testing_with_names_and_paths_with_alias(
            &[
                (
                    CellName::testing_new("root"),
                    CellRootPathBuf::testing_new(""),
                ),
                (
                    CellName::testing_new("cell1"),
                    CellRootPathBuf::testing_new("cell1"),
                ),
                (
                    CellName::testing_new("cell2"),
                    CellRootPathBuf::testing_new("cell2"),
                ),
            ],
            HashMap::from_iter([
                (
                    NonEmptyCellAlias::testing_new("cell1"),
                    CellName::testing_new("cell1"),
                ),
                (
                    NonEmptyCellAlias::testing_new("alias2"),
                    CellName::testing_new("cell2"),
                ),
            ]),
        )
    }

    fn alias_resolver() -> CellAliasResolver {
        resolver().root_cell_cell_alias_resolver().clone()
    }

    #[test_case(PhantomData::< TargetPatternExtra >; "parsing TargetPattern")]
    #[test_case(PhantomData::< ProvidersPatternExtra >; "parsing ProvidersPattern")]
    #[test_case(PhantomData::< ConfiguredTargetPatternExtra >; "parsing ConfiguredTargetPatternExtra")]
    #[test_case(PhantomData::< ConfiguredProvidersPatternExtra >; "parsing ConfiguredProvidersPatternExtra")]
    fn parse_absolute_pattern<T: PatternType>(_: PhantomData<T>) {
        let package = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package/path").to_owned(),
        );

        assert_eq!(
            mk_package::<T>("root", "package/path"),
            ParsedPattern::<T>::parse_precise(
                "//package/path:",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )
            .unwrap()
        );
        assert_eq!(
            mk_package::<T>("root", ""),
            ParsedPattern::<T>::parse_precise(
                "//:",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )
            .unwrap()
        );
        assert_eq!(
            mk_package::<T>("cell1", "package/path"),
            ParsedPattern::<T>::parse_precise(
                "cell1//package/path:",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )
            .unwrap()
        );
        assert_matches!(
            ParsedPattern::<T>::parse_precise("package/path:", CellName::testing_new("root"), &resolver(),
            &alias_resolver(),),
            Err(e) => {
                assert!(
                    format!("{e:?}").contains(&format!("{}", TargetPatternParseError::AbsoluteRequired))
                );
            }
        );
        assert_eq!(
            mk_package::<T>("cell2", "package/path"),
            ParsedPattern::<T>::parse_precise(
                "alias2//package/path:",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )
            .unwrap()
        );
        assert_eq!(
            mk_package::<T>("cell2", "package/path"),
            ParsedPattern::<T>::parse_precise(
                "@alias2//package/path:",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )
            .unwrap()
        );
        assert_eq!(
            mk_recursive::<T>("root", "package/path"),
            ParsedPattern::<T>::parse_precise(
                "//package/path/...",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )
            .unwrap()
        );
        assert_eq!(
            mk_recursive::<T>("root", "package/path"),
            ParsedPattern::<T>::parse_not_relaxed(
                "...",
                TargetParsingRel::AllowRelative(
                    &CellPathWithAllowedRelativeDir::backwards_relative_not_supported(
                        package.clone()
                    ),
                    Some(&NoAliases),
                ),
                &resolver(),
                &alias_resolver(),
            )
            .unwrap()
        );
        assert_eq!(
            mk_recursive::<T>("root", "package/path/foo"),
            ParsedPattern::<T>::parse_not_relaxed(
                "foo/...",
                TargetParsingRel::AllowRelative(
                    &CellPathWithAllowedRelativeDir::backwards_relative_not_supported(package),
                    Some(&NoAliases),
                ),
                &resolver(),
                &alias_resolver(),
            )
            .unwrap()
        );
    }

    #[test]
    fn parse_relative_pattern() -> buck2_error::Result<()> {
        let package = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package/path").to_owned(),
        );

        assert_eq!(
            mk_target("root", "package/path", "target"),
            ParsedPattern::parse_precise(
                "//package/path:target",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_target("root", "package/path/foo", "target"),
            ParsedPattern::parse_not_relaxed(
                "foo:target",
                TargetParsingRel::AllowRelative(
                    &CellPathWithAllowedRelativeDir::backwards_relative_not_supported(package),
                    Some(&NoAliases),
                ),
                &resolver(),
                &alias_resolver(),
            )?
        );
        Ok(())
    }

    #[test]
    fn test_relaxed() -> buck2_error::Result<()> {
        let package = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package").to_owned(),
        );

        assert_matches!(
            ParsedPattern::<TargetPatternExtra>::parse_not_relaxed(
                "path",
                TargetParsingRel::AllowRelative(
                    &CellPathWithAllowedRelativeDir::backwards_relative_not_supported(package.clone()),
                    Some(&NoAliases),
                ),
                &resolver(),
                &alias_resolver(),
            ),
            Err(e) => {
                assert!(
                    format!("{e:?}").contains(&format!("{}", TargetPatternParseError::UnexpectedFormat))
                );
            }
        );

        assert_eq!(
            mk_target("root", "package/path", "path"),
            ParsedPattern::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "//package/path",
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_target("root", "package/path", "path"),
            ParsedPattern::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "path",
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_providers("root", "package/path", "path", Some(&["provider"])),
            ParsedPattern::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "path[provider]",
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_providers(
                "root",
                "package/path/subpath",
                "subpath",
                Some(&["provider"])
            ),
            ParsedPattern::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "path/subpath[provider]",
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_target("root", "package/path/subpath", "subpath"),
            ParsedPattern::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "path/subpath",
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_target("root", "package/path", "path"),
            ParsedPattern::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "//package/path/",
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_target("root", "package/path", "target"),
            ParsedPattern::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "//package/path/:target",
                &resolver(),
                &alias_resolver(),
            )?
        );

        // Awkward but technically valid?
        assert_eq!(
            mk_target("root", "package", "foo"),
            ParsedPattern::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "/:foo",
                &resolver(),
                &alias_resolver(),
            )?
        );

        // There's no target here so this is invalid.
        assert_matches!(
            ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "/",
                &resolver(),
                &alias_resolver(),
            ),
            Err(e) => {
                assert!(
                    format!("{e:?}").contains(&format!("{}", TargetPatternParseError::PackageIsEmpty))
                );
            }
        );

        Ok(())
    }

    #[test]
    fn test_parsed_opt_absolute() -> buck2_error::Result<()> {
        let package = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package/path").to_owned(),
        );

        assert_eq!(
            mk_target("root", "other", "target"),
            ParsedPattern::parse_not_relaxed(
                "//other:target",
                TargetParsingRel::AllowLimitedRelative(package.as_ref()),
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_target("root", "package/path", "target"),
            ParsedPattern::parse_not_relaxed(
                ":target",
                TargetParsingRel::AllowLimitedRelative(package.as_ref()),
                &resolver(),
                &alias_resolver(),
            )?
        );
        let err = ParsedPattern::<TargetPatternExtra>::parse_not_relaxed(
            ":target",
            TargetParsingRel::RequireAbsolute(CellName::testing_new("root")),
            &resolver(),
            &alias_resolver(),
        )
        .unwrap_err();
        assert!(
            err.to_string()
                .contains("Invalid target pattern `:target` is not allowed"),
            "{}",
            err
        );
        // But this should be fine.
        assert_eq!(
            mk_target("cell1", "", "target"),
            ParsedPattern::parse_not_relaxed(
                "cell1//:target",
                TargetParsingRel::RequireAbsolute(CellName::testing_new("root")),
                &resolver(),
                &alias_resolver(),
            )?
        );

        assert_matches!(
            ParsedPattern::<TargetPatternExtra>::parse_not_relaxed(
                "foo/bar",
                TargetParsingRel::AllowLimitedRelative(package.as_ref()),
                &resolver(),
                &alias_resolver(),
            ),
            Err(e) => {
                assert!(
                    format!("{e:?}").contains(&format!("{}", TargetPatternParseError::UnexpectedFormat))
                );
            }
        );

        assert_matches!(
            ParsedPattern::<TargetPatternExtra>::parse_not_relaxed(
                "foo/bar:bar",
                TargetParsingRel::AllowLimitedRelative(package.as_ref()),
                &resolver(),
                &alias_resolver(),
            ),
            Err(e) => {
                assert!(
                    format!("{e:?}").contains(&format!("{}", TargetPatternParseError::AbsoluteRequired))
                );
            }
        );
        assert_matches!(
            ParsedPattern::<TargetPatternExtra>::parse_not_relaxed(
                "foo/bar:bar",
                TargetParsingRel::RequireAbsolute(CellName::testing_new("root")),
                &resolver(),
                &alias_resolver(),
            ),
            Err(e) => {
                assert!(
                    format!("{e:?}").contains(&format!("{}", TargetPatternParseError::AbsoluteRequired))
                );
            }
        );

        Ok(())
    }

    #[test]
    fn test_aliases() -> buck2_error::Result<()> {
        let package = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package").to_owned(),
        );

        let config = aliases(&[
            ("foo", "cell1//foo/bar:target"),
            ("invalid/alias", "cell1//foo/bar:target"),
            ("badalias", "cell1//foo/bar:"),
        ]);

        assert_eq!(
            mk_target("cell1", "foo/bar", "target"),
            ParsedPattern::parse_relaxed(
                &config,
                package.as_ref(),
                "foo",
                &resolver(),
                &alias_resolver(),
            )?
        );

        assert_matches!(
            ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                &config,
                package.as_ref(),
                "invalid/alias",
                &resolver(),
                &alias_resolver(),
            ),
            Err(e) => {
                assert!(
                    format!("{e:?}").contains("Invalid alias")
                );
            }
        );

        assert_matches!(
            ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                &config,
                package.as_ref(),
                "badalias",
                &resolver(),
                &alias_resolver(),
            ),
            Err(e) => {
                assert!(
                    format!("{e:?}").contains("is not a target")
                );
            }
        );

        Ok(())
    }

    #[test]
    fn parse_providers_pattern() -> buck2_error::Result<()> {
        assert_eq!(
            mk_providers("root", "package/path", "target", None),
            ParsedPattern::parse_precise(
                "//package/path:target",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_providers("root", "package/path", "target", Some(&["java-output"])),
            ParsedPattern::parse_precise(
                "//package/path:target[java-output]",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_providers(
                "root",
                "package/path",
                "target",
                Some(&["FDSIcon+FDSInternal.h"]),
            ),
            ParsedPattern::parse_precise(
                "//package/path:target[FDSIcon+FDSInternal.h]",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )?
        );

        let (target_label, providers) = ParsedPattern::parse_precise(
            "//package/path:target#flavor",
            CellName::testing_new("root"),
            &resolver(),
            &alias_resolver(),
        )?
        .as_literal("")?;
        assert_eq!(
            "root//package/path:target#flavor",
            ProvidersPatternExtra::into_providers_label(
                providers,
                target_label.pkg(),
                target_label.name()
            )
            .to_string(),
        );
        Ok(())
    }

    #[test]
    fn parse_providers_pattern_with_alias() -> buck2_error::Result<()> {
        let package = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package").to_owned(),
        );

        let config = aliases(&[("foo", "cell1//foo/bar:target")]);

        assert_eq!(
            mk_providers("cell1", "foo/bar", "target", Some(&["qux"])),
            ParsedPattern::parse_relaxed(
                &config,
                package.as_ref(),
                "foo[qux]",
                &resolver(),
                &alias_resolver(),
            )?
        );

        Ok(())
    }

    #[test]
    fn test_parse_configured_providers_pattern() -> buck2_error::Result<()> {
        assert_eq!(
            mk_configured_providers(
                "root",
                "package/path",
                "target",
                None,
                ConfigurationPredicate::Any
            ),
            ParsedPattern::parse_precise(
                "//package/path:target",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_configured_providers(
                "root",
                "package/path",
                "target",
                None,
                ConfigurationPredicate::Builtin(BuiltinPlatform::Unspecified)
            ),
            ParsedPattern::parse_precise(
                "//package/path:target (<unspecified>)",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_configured_providers(
                "root",
                "package/path",
                "target",
                Some(&["P"]),
                ConfigurationPredicate::Bound(
                    BoundConfigurationLabel::new("<foo>".to_owned()).unwrap(),
                    None
                ),
            ),
            ParsedPattern::parse_precise(
                "//package/path:target[P] (<foo>)",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )?
        );
        assert_eq!(
            mk_configured_providers(
                "root",
                "package/path",
                "target",
                Some(&["P"]),
                ConfigurationPredicate::Bound(
                    BoundConfigurationLabel::new("<foo>".to_owned()).unwrap(),
                    Some(ConfigurationHash::from_str("0123456789abcdef").unwrap()),
                ),
            ),
            ParsedPattern::parse_precise(
                "//package/path:target[P] (<foo>#0123456789abcdef)",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            )?
        );
        Ok(())
    }

    #[test_case(PhantomData::< TargetPatternExtra >; "parsing TargetPattern")]
    #[test_case(PhantomData::< ProvidersPatternExtra >; "parsing ProvidersPattern")]
    #[test_case(PhantomData::< ConfiguredTargetPatternExtra >; "parsing ConfiguredTargetPatternExtra")]
    #[test_case(PhantomData::< ConfiguredProvidersPatternExtra >; "parsing ConfiguredProvidersPatternExtra")]
    fn parse_pattern_failure<T: PatternType>(_: PhantomData<T>) {
        fails(
            ParsedPattern::<T>::parse_precise(
                "",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(
                "//package/path",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(
                "//package...",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(
                "package",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(
                "bad_alias//package/path:",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[
                "bad_alias//package/path:",
                "unknown cell alias: `bad_alias`.",
            ],
        );
        fails(
            ParsedPattern::<T>::parse_precise(
                "//package/path/:target",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(
                "//package/path/",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[],
        );
        fails(
            ParsedPattern::<T>::parse_precise(
                "$(exe my macro)",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[
                "$(exe my macro)",
                "You may be trying to use a macro instead of a target pattern. Macro usage is invalid here",
            ],
        );
    }

    #[test]
    fn bad_providers_label() {
        fails(
            ParsedPattern::<ProvidersPatternExtra>::parse_precise(
                "//package/path:target[unclosed",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[
                "//package/path:target[unclosed",
                "target pattern with `[` must end with `]` to mark end of providers set label",
            ],
        );
        fails(
            ParsedPattern::<ProvidersPatternExtra>::parse_precise(
                "//package/path:target[out]wrong",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[
                "//package/path:target[out]wrong",
                "target pattern with `[` must end with `]` to mark end of providers set label",
            ],
        );
        fails(
            ParsedPattern::<ProvidersPatternExtra>::parse_precise(
                "$(exe my macro)",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &[
                "$(exe my macro)",
                "You may be trying to use a macro instead of a target pattern. Macro usage is invalid here",
            ],
        );
    }

    #[test]
    fn parsed_pattern_contains() -> buck2_error::Result<()> {
        let pkg1 = PackageLabel::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package/path"),
        )
        .unwrap();
        let pkg2 = PackageLabel::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package"),
        )
        .unwrap();
        let pkg3 = PackageLabel::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package2"),
        )
        .unwrap();
        let pkg_in_different_cell = PackageLabel::new(
            CellName::testing_new("cell1"),
            CellRelativePath::unchecked_new("package/path"),
        )
        .unwrap();

        let target_in_pkg1 = TargetLabel::new(pkg1.dupe(), TargetNameRef::new("target")?);
        let another_target_in_pkg1 = TargetLabel::new(pkg1, TargetNameRef::new("target2")?);
        let target_in_pkg2 = TargetLabel::new(pkg2, TargetNameRef::new("target")?);
        let target_in_pkg3 = TargetLabel::new(pkg3, TargetNameRef::new("target")?);
        let target_in_different_cell =
            TargetLabel::new(pkg_in_different_cell, TargetNameRef::new("target")?);

        // Testing ParsedPattern::Target

        let pattern = ParsedPattern::parse_precise(
            "//package/path:target",
            CellName::testing_new("root"),
            &resolver(),
            &alias_resolver(),
        )?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(!pattern.matches(&another_target_in_pkg1));
        assert!(!pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        // Testing ParsedPattern::Package

        let pattern = ParsedPattern::parse_precise(
            "//package/path:",
            CellName::testing_new("root"),
            &resolver(),
            &alias_resolver(),
        )?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(pattern.matches(&another_target_in_pkg1));
        assert!(!pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        let pattern = ParsedPattern::parse_precise(
            "//package:",
            CellName::testing_new("root"),
            &resolver(),
            &alias_resolver(),
        )?;
        assert!(!pattern.matches(&target_in_pkg1));
        assert!(!pattern.matches(&another_target_in_pkg1));
        assert!(pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        // Testing ParsedPattern::Recursive

        let pattern = ParsedPattern::parse_precise(
            "//package/path/...",
            CellName::testing_new("root"),
            &resolver(),
            &alias_resolver(),
        )?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(pattern.matches(&another_target_in_pkg1));
        assert!(!pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        let pattern = ParsedPattern::parse_precise(
            "//package/...",
            CellName::testing_new("root"),
            &resolver(),
            &alias_resolver(),
        )?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(pattern.matches(&another_target_in_pkg1));
        assert!(pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        let pattern = ParsedPattern::parse_precise(
            "//...",
            CellName::testing_new("root"),
            &resolver(),
            &alias_resolver(),
        )?;
        assert!(pattern.matches(&target_in_pkg1));
        assert!(pattern.matches(&another_target_in_pkg1));
        assert!(pattern.matches(&target_in_pkg2));
        assert!(pattern.matches(&target_in_pkg3));
        assert!(!pattern.matches(&target_in_different_cell));

        let pattern = ParsedPattern::parse_precise(
            "cell1//...",
            CellName::testing_new("root"),
            &resolver(),
            &alias_resolver(),
        )?;
        assert!(!pattern.matches(&target_in_pkg1));
        assert!(!pattern.matches(&another_target_in_pkg1));
        assert!(!pattern.matches(&target_in_pkg2));
        assert!(!pattern.matches(&target_in_pkg3));
        assert!(pattern.matches(&target_in_different_cell));

        Ok(())
    }

    #[test]
    fn test_parsed_pattern_display() {
        assert_eq!(
            "foo//bar:baz",
            ParsedPattern::<TargetPatternExtra>::testing_parse("foo//bar:baz").to_string()
        );
        assert_eq!(
            "foo//bar:",
            ParsedPattern::<TargetPatternExtra>::testing_parse("foo//bar:").to_string()
        );
        assert_eq!(
            "foo//bar/...",
            ParsedPattern::<TargetPatternExtra>::testing_parse("foo//bar/...").to_string()
        );
        assert_eq!(
            "foo//:",
            ParsedPattern::<TargetPatternExtra>::testing_parse("foo//:").to_string()
        );
        assert_eq!(
            "foo//...",
            ParsedPattern::<TargetPatternExtra>::testing_parse("foo//...").to_string()
        );
    }

    #[test]
    fn test_relative_pattern_with_parent() -> buck2_error::Result<()> {
        let package = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package/path").to_owned(),
        );

        assert_eq!(
            mk_target("root", "package/sibling", "target"),
            ParsedPattern::parse_not_relaxed(
                "../sibling:target",
                TargetParsingRel::AllowRelative(
                    &CellPathWithAllowedRelativeDir::new(
                        package.clone(),
                        Some(CellPath::testing_new("root//package"))
                    ),
                    Some(&NoAliases),
                ),
                &resolver(),
                &alias_resolver(),
            )?
        );

        fails(
            ParsedPattern::<TargetPatternExtra>::parse_not_relaxed(
                "../sibling:target",
                TargetParsingRel::AllowRelative(
                    &CellPathWithAllowedRelativeDir::backwards_relative_not_supported(
                        package.clone(),
                    ),
                    Some(&NoAliases),
                ),
                &resolver(),
                &alias_resolver(),
            ),
            &["Invalid target pattern `../sibling:target` is not allowed"],
        );

        fails(
            ParsedPattern::<TargetPatternExtra>::parse_not_relaxed(
                "../../not_allowed:target",
                TargetParsingRel::AllowRelative(
                    &CellPathWithAllowedRelativeDir::new(
                        package.clone(),
                        Some(CellPath::testing_new("root//package")),
                    ),
                    Some(&NoAliases),
                ),
                &resolver(),
                &alias_resolver(),
            ),
            &["Invalid target pattern `../../not_allowed:target` is not allowed"],
        );

        Ok(())
    }

    #[test]
    fn test_parsed_pattern_with_modifiers_relaxed() -> buck2_error::Result<()> {
        let resolver = resolver();
        let alias_resolver = alias_resolver();
        let package = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package").to_owned(),
        );

        fails(
            ParsedPatternWithModifiers::<TargetPatternExtra>::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "root//package?/target?modifier",
                &resolver,
                &alias_resolver,
            ),
            &[
                "Expected at most one ? in pattern, question marks in file, target, modifier, or cell names are not supported",
            ],
        );

        // Test target pattern with modifiers
        let pattern_parts = ParsedPatternWithModifiers::<TargetPatternExtra>::parse_relaxed(
            &NoAliases,
            package.as_ref(),
            "root//package/path:target?modifier1+modifier2",
            &resolver,
            &alias_resolver,
        )?;

        assert_eq!(
            pattern_parts.parsed_pattern,
            mk_target("root", "package/path", "target")
        );
        assert_eq!(
            pattern_parts.modifiers,
            Modifiers::new(Some(vec!["modifier1".to_owned(), "modifier2".to_owned()]))
        );

        // Test package pattern with modifiers
        let pattern_parts = ParsedPatternWithModifiers::<TargetPatternExtra>::parse_relaxed(
            &NoAliases,
            package.as_ref(),
            "root//package/path:?modifier",
            &resolver,
            &alias_resolver,
        )?;

        assert_eq!(
            pattern_parts.parsed_pattern,
            mk_package("root", "package/path")
        );
        assert_eq!(
            pattern_parts.modifiers,
            Modifiers::new(Some(vec!["modifier".to_owned()]))
        );

        // Test recursive pattern with modifiers
        let pattern_parts = ParsedPatternWithModifiers::<TargetPatternExtra>::parse_relaxed(
            &NoAliases,
            package.as_ref(),
            "root//package/path/...?modifier1+modifier2+modifier3",
            &resolver,
            &alias_resolver,
        )?;

        assert_eq!(
            pattern_parts.parsed_pattern,
            mk_recursive("root", "package/path")
        );
        assert_eq!(
            pattern_parts.modifiers,
            Modifiers::new(Some(vec![
                "modifier1".to_owned(),
                "modifier2".to_owned(),
                "modifier3".to_owned()
            ]))
        );

        // Test relative pattern with modifiers
        let pattern_parts = ParsedPatternWithModifiers::<TargetPatternExtra>::parse_relaxed(
            &NoAliases,
            package.as_ref(),
            "path:target?modifier",
            &resolver,
            &alias_resolver,
        )?;

        assert_eq!(
            pattern_parts.parsed_pattern,
            mk_target("root", "package/path", "target")
        );
        assert_eq!(
            pattern_parts.modifiers,
            Modifiers::new(Some(vec!["modifier".to_owned()]))
        );

        // Test ambiguous pattern with modifiers
        let pattern_parts = ParsedPatternWithModifiers::<TargetPatternExtra>::parse_relaxed(
            &NoAliases,
            package.as_ref(),
            "root//package/path?modifier",
            &resolver,
            &alias_resolver,
        )?;

        assert_eq!(
            pattern_parts.parsed_pattern,
            mk_target("root", "package/path", "path")
        );
        assert_eq!(
            pattern_parts.modifiers,
            Modifiers::new(Some(vec!["modifier".to_owned()]))
        );

        // Test pattern with trailing slash and modifiers
        let pattern_parts = ParsedPatternWithModifiers::<TargetPatternExtra>::parse_relaxed(
            &NoAliases,
            package.as_ref(),
            "root//package/path/?modifier",
            &resolver,
            &alias_resolver,
        )?;

        assert_eq!(
            pattern_parts.parsed_pattern,
            mk_target("root", "package/path", "path")
        );
        assert_eq!(
            pattern_parts.modifiers,
            Modifiers::new(Some(vec!["modifier".to_owned()]))
        );

        // Test alias
        let alias_config = aliases(&[("foo_target", "cell1//foo/bar:target")]);

        let pattern_parts = ParsedPatternWithModifiers::<TargetPatternExtra>::parse_relaxed(
            &alias_config,
            package.as_ref(),
            "foo_target?modifier1+modifier2",
            &resolver,
            &alias_resolver,
        )?;

        assert_eq!(
            pattern_parts.parsed_pattern,
            mk_target("cell1", "foo/bar", "target")
        );
        assert_eq!(
            pattern_parts.modifiers,
            Modifiers::new(Some(vec!["modifier1".to_owned(), "modifier2".to_owned()]))
        );

        Ok(())
    }

    #[test]
    fn test_parsed_pattern_fails_with_modifiers() {
        fails(
            ParsedPattern::<TargetPatternExtra>::parse_precise(
                "root//package/path:target?modifier",
                CellName::testing_new("root"),
                &resolver(),
                &alias_resolver(),
            ),
            &["The ?modifier syntax is unsupported for this command"],
        );

        fails(
            ParsedPattern::<TargetPatternExtra>::parse_not_relaxed(
                "root//package/path:target?modifier",
                TargetParsingRel::RequireAbsolute(CellName::testing_new("root")),
                &resolver(),
                &alias_resolver(),
            ),
            &["The ?modifier syntax is unsupported for this command"],
        );

        fails(
            ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                &NoAliases,
                CellPath::new(
                    CellName::testing_new("root"),
                    CellRelativePath::unchecked_new("package").to_owned(),
                )
                .as_ref(),
                "root//package/path:target?modifier",
                &resolver(),
                &alias_resolver(),
            ),
            &["The ?modifier syntax is unsupported for this command"],
        );
    }

    #[test]
    fn test_configured_parsed_pattern_with_modifiers() -> buck2_error::Result<()> {
        let resolver = resolver();
        let alias_resolver = alias_resolver();
        let package = CellPath::new(
            CellName::testing_new("root"),
            CellRelativePath::unchecked_new("package").to_owned(),
        );

        // Test that it fails when there are modifiers and configuration predicate is not Any or Builtin
        fails(
            ParsedPatternWithModifiers::<ConfiguredProvidersPatternExtra>::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "//package/path:target?modifier (<foo>)",
                &resolver,
                &alias_resolver,
            ),
            &["Modifiers incompatible with explicit configuration"],
        );

        // Test that it works with Any configuration predicate
        let pattern_parts =
            ParsedPatternWithModifiers::<ConfiguredProvidersPatternExtra>::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "//package/path:target?modifier",
                &resolver,
                &alias_resolver,
            )?;

        assert_eq!(
            pattern_parts.modifiers,
            Modifiers::new(Some(vec!["modifier".to_owned()]))
        );

        // Test that it works with Builtin configuration predicate
        let pattern_parts =
            ParsedPatternWithModifiers::<ConfiguredProvidersPatternExtra>::parse_relaxed(
                &NoAliases,
                package.as_ref(),
                "//package/path:target?modifier (<unbound>)",
                &resolver,
                &alias_resolver,
            )?;

        assert_eq!(
            pattern_parts.modifiers,
            Modifiers::new(Some(vec!["modifier".to_owned()]))
        );

        Ok(())
    }
}
