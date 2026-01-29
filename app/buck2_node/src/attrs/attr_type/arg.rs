/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub mod parser;

use allocative::Allocative;
use buck2_core::package::PackageLabel;
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use buck2_util::arc_str::ArcStr;
use derive_more::Display;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use pagable::Pagable;
use static_assertions::assert_eq_size;
use strong_hash::StrongHash;

use crate::attrs::attr_type::query::QueryMacroBase;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::traversal::CoercedAttrTraversal;

/// The kind of dependency for a macro like `$(location ...)` or `$(exe ...)`.
#[derive(
    Debug, Eq, PartialEq, Hash, Clone, Copy, Dupe, Allocative, Pagable, StrongHash
)]
pub enum MacroDepKind {
    /// Regular target dependency (configured with target configuration).
    Regular,
    /// Execution dependency (configured with exec configuration).
    Exec,
    /// Toolchain dependency (configured with toolchain configuration).
    Toolchain,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy, Dupe, Allocative, Pagable)]
pub struct ArgAttrType {
    pub anon_target_compatible: bool,
}

/// [StringWithMacros] is the core representation for an attrs.arg() (in all of it's coerced, configured, and resolved
/// forms). The parsed arg string is held as a sequence of parts (each part either a literal string or a macro). When
/// being added to a command line, these parts will be concattenated together and added as a single arg.
/// Each variant takes in a boolean which determines if the resolved form should be compatible with anon targets.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable, StrongHash)]
pub enum StringWithMacros<P: ProvidersLabelMaybeConfigured> {
    /// Semantically, StringWithMacros::StringPart(s) is equivalent to
    /// StringWithMacros::ManyParts(vec![StringWithMacrosPart::String(s)]). We special-case this
    /// for memory efficiency to avoid allocating unnecessary vectors, since lone string parts are
    /// very frequent.
    StringPart(ArcStr),
    // For resolution, we defer all work and simply alloc a ConfiguredStringWithMacros into the starlark
    // context. This allows us to defer resolution work to the point that it is being added to a command
    // line, but it requires that we can cheaply copy ConfiguredStringWithMacros.
    ManyParts(Box<[StringWithMacrosPart<P>]>),
}

// Avoid changing the size accidentally.
assert_eq_size!(StringWithMacros<ProvidersLabel>, [usize; 3]);

impl StringWithMacros<ConfiguredProvidersLabel> {
    pub fn concat(
        self,
        items: impl Iterator<Item = buck2_error::Result<Self>>,
    ) -> buck2_error::Result<Self> {
        let mut parts = Vec::new();
        for x in std::iter::once(Ok(self)).chain(items) {
            match x? {
                Self::StringPart(x) => {
                    parts.push(StringWithMacrosPart::String(x));
                }
                Self::ManyParts(xs) => {
                    parts.extend(xs.into_vec());
                }
            }
        }
        Ok(Self::ManyParts(parts.into_boxed_slice()))
    }
}

impl StringWithMacros<ConfiguredProvidersLabel> {
    pub fn traverse(
        &self,
        traversal: &mut dyn ConfiguredAttrTraversal,
        pkg: PackageLabel,
    ) -> buck2_error::Result<()> {
        match self {
            Self::StringPart(..) => {}
            Self::ManyParts(parts) => {
                for part in parts.iter() {
                    match part {
                        StringWithMacrosPart::String(_) => {}
                        StringWithMacrosPart::Macro(_, m) => {
                            m.traverse(traversal, pkg)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

impl StringWithMacros<ProvidersLabel> {
    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
        anon_target_compatible: bool,
    ) -> buck2_error::Result<ConfiguredStringWithMacros> {
        match self {
            Self::StringPart(part) => Ok(ConfiguredStringWithMacros {
                string_with_macros: StringWithMacros::StringPart(part.clone()),
                anon_target_compatible,
            }),
            Self::ManyParts(parts) => Ok(ConfiguredStringWithMacros {
                string_with_macros: StringWithMacros::ManyParts(
                    parts.try_map(|p| p.configure(ctx))?.into_boxed_slice(),
                ),
                anon_target_compatible,
            }),
        }
    }

    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
        pkg: PackageLabel,
    ) -> buck2_error::Result<()> {
        match self {
            Self::StringPart(..) => {}
            Self::ManyParts(parts) => {
                for part in parts.iter() {
                    match part {
                        StringWithMacrosPart::String(_) => {}
                        StringWithMacrosPart::Macro(_, m) => {
                            m.traverse(traversal, pkg)?;
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable, StrongHash)]
pub enum StringWithMacrosPart<P: ProvidersLabelMaybeConfigured> {
    String(ArcStr),
    Macro(/* write_to_file */ bool, MacroBase<P>),
}

assert_eq_size!(MacroBase<ProvidersLabel>, [usize; 3]);
assert_eq_size!(StringWithMacrosPart<ProvidersLabel>, [usize; 4]);

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable, StrongHash)]
pub struct UnrecognizedMacro {
    pub macro_type: Box<str>,
    pub args: Box<[String]>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable, StrongHash)]
pub enum MacroBase<P: ProvidersLabelMaybeConfigured> {
    Location {
        label: P,
        dep_kind: MacroDepKind,
    },
    /// Represents both $(exe) and $(exe_target) usages.
    Exe {
        label: P,
        exec_dep: bool,
    },
    /// A user-defined make variable (like `$(CXX)`). This will be resolved based on the propagated TemplateVariableInfos.
    UserUnkeyedPlaceholder(Box<str>),

    /// A user-defined macro (like `$(cxxppflags //some:target)`). This will be resolved based on the propagated TemplateVariableInfos.
    UserKeyedPlaceholder(Box<(Box<str>, P, Option<Box<str>>)>),

    Query(Box<QueryMacroBase<P>>),
    Source(CoercedPath),

    /// Right now, we defer error for unrecognized macros to the place where they are used. This just allows
    /// us to progress further into a build and detect more issues. Once we have all (or most) of the buckv1 macros
    /// recognized we'll remove this and make it an early error.
    UnrecognizedMacro(Box<UnrecognizedMacro>),
}

impl MacroBase<ConfiguredProvidersLabel> {
    pub fn traverse(
        &self,
        traversal: &mut dyn ConfiguredAttrTraversal,
        pkg: PackageLabel,
    ) -> buck2_error::Result<()> {
        // macros can't reference repo inputs (they only reference the outputs of other targets)
        match self {
            MacroBase::UserKeyedPlaceholder(box (_, l, _)) => traversal.dep(l),
            MacroBase::Location {
                label,
                dep_kind: MacroDepKind::Exec,
            } => traversal.exec_dep(label),
            MacroBase::Location {
                label,
                dep_kind: MacroDepKind::Toolchain,
            } => traversal.toolchain_dep(label),
            MacroBase::Location {
                label,
                dep_kind: MacroDepKind::Regular,
            } => traversal.dep(label),
            MacroBase::Exe {
                label,
                exec_dep: true,
            } => traversal.exec_dep(label),
            MacroBase::Exe {
                label,
                exec_dep: false,
            } => traversal.dep(label),
            MacroBase::Source(path) => {
                for x in path.inputs() {
                    traversal.input(SourcePathRef::new(pkg.dupe(), x))?;
                }
                Ok(())
            }
            MacroBase::Query(query_macro) => query_macro.traverse(traversal),
            MacroBase::UserUnkeyedPlaceholder(_) | MacroBase::UnrecognizedMacro(..) => Ok(()),
        }
    }
}

impl MacroBase<ProvidersLabel> {
    pub fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<ConfiguredMacro> {
        Ok(match self {
            UnconfiguredMacro::Location { label, dep_kind } => ConfiguredMacro::Location {
                label: match dep_kind {
                    MacroDepKind::Exec => ctx.configure_exec_target(label)?,
                    MacroDepKind::Toolchain => ctx.configure_toolchain_target(label),
                    MacroDepKind::Regular => ctx.configure_target(label),
                },
                dep_kind: *dep_kind,
            },
            UnconfiguredMacro::Exe { label, exec_dep } => ConfiguredMacro::Exe {
                label: if *exec_dep {
                    ctx.configure_exec_target(label)?
                } else {
                    ctx.configure_target(label)
                },
                exec_dep: *exec_dep,
            },
            UnconfiguredMacro::UserUnkeyedPlaceholder(var_name) => {
                ConfiguredMacro::UserUnkeyedPlaceholder(var_name.clone())
            }
            UnconfiguredMacro::UserKeyedPlaceholder(box (var_name, target, arg)) => {
                ConfiguredMacro::UserKeyedPlaceholder(Box::new((
                    var_name.clone(),
                    ctx.configure_target(target),
                    arg.clone(),
                )))
            }
            UnconfiguredMacro::Query(query) => {
                ConfiguredMacro::Query(Box::new(query.configure(ctx)?))
            }
            UnconfiguredMacro::Source(path) => ConfiguredMacro::Source(path.clone()),
            UnconfiguredMacro::UnrecognizedMacro(macr) => {
                ConfiguredMacro::UnrecognizedMacro(macr.clone())
            }
        })
    }

    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
        pkg: PackageLabel,
    ) -> buck2_error::Result<()> {
        match self {
            MacroBase::UserKeyedPlaceholder(box (_, l, _)) => traversal.dep(l),
            MacroBase::Location {
                label,
                dep_kind: MacroDepKind::Exec,
            } => traversal.exec_dep(label),
            MacroBase::Location {
                label,
                dep_kind: MacroDepKind::Toolchain,
            } => traversal.toolchain_dep(label),
            MacroBase::Location {
                label,
                dep_kind: MacroDepKind::Regular,
            } => traversal.dep(label),
            MacroBase::Exe {
                label,
                exec_dep: true,
            } => traversal.exec_dep(label),
            MacroBase::Exe {
                label,
                exec_dep: false,
            } => traversal.dep(label),
            MacroBase::Query(query) => query.traverse(traversal),
            MacroBase::Source(path) => {
                for x in path.inputs() {
                    traversal.input(SourcePathRef::new(pkg.dupe(), x))?;
                }
                Ok(())
            }
            MacroBase::UserUnkeyedPlaceholder(_) | MacroBase::UnrecognizedMacro(..) => Ok(()),
        }
    }
}

// These type aliases are just a little bit easier to use, the differentiating thing comes
// right at the beginning instead of at the end in a type param.
pub type UnconfiguredMacro = MacroBase<ProvidersLabel>;
pub type ConfiguredMacro = MacroBase<ConfiguredProvidersLabel>;

pub type UnconfiguredStringWithMacrosPart = StringWithMacrosPart<ProvidersLabel>;
pub type ConfiguredStringWithMacrosPart = StringWithMacrosPart<ConfiguredProvidersLabel>;

pub type UnconfiguredStringWithMacros = StringWithMacros<ProvidersLabel>;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Display, StrongHash)]
#[display("{}", string_with_macros)]
pub struct ConfiguredStringWithMacros {
    pub string_with_macros: StringWithMacros<ConfiguredProvidersLabel>,
    pub anon_target_compatible: bool,
}

/// Display attempts to approximately reproduce the string that created a macro.
impl<P: ProvidersLabelMaybeConfigured> Display for MacroBase<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: this should re-escape values in the args that need to be escaped to have returned that arg (it's not possible
        // to tell where there were unnecessary escapes and it's not worth tracking that).
        match self {
            MacroBase::Location { label, dep_kind } => {
                write!(
                    f,
                    "{} {}",
                    match dep_kind {
                        MacroDepKind::Exec => "location_exec",
                        MacroDepKind::Toolchain => "location_toolchain",
                        MacroDepKind::Regular => "location",
                    },
                    label
                )
            }
            MacroBase::Exe { label, exec_dep } => {
                write!(
                    f,
                    "{} {}",
                    if *exec_dep { "exe" } else { "exe_target" },
                    label
                )
            }
            MacroBase::Query(query) => Display::fmt(query, f),
            MacroBase::Source(path) => write!(f, "src {}", path.path()),
            MacroBase::UserUnkeyedPlaceholder(var) => write!(f, "{var}"),
            MacroBase::UserKeyedPlaceholder(box (macro_type, target, arg)) => write!(
                f,
                "{} {}{}",
                macro_type,
                target,
                if let Some(arg) = arg {
                    format!(" {arg}")
                } else {
                    "".to_owned()
                }
            ),
            MacroBase::UnrecognizedMacro(box UnrecognizedMacro { macro_type, args }) => {
                write!(f, "<unknown>({}) {}", macro_type, args.join(" "))
            }
        }
    }
}

impl<P: ProvidersLabelMaybeConfigured> Display for StringWithMacrosPart<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringWithMacrosPart::String(s) => write!(f, "{s}"),
            StringWithMacrosPart::Macro(write_to_file, m) => {
                write!(f, "$({}{})", if *write_to_file { "@" } else { "" }, m)
            }
        }
    }
}

impl StringWithMacrosPart<ProvidersLabel> {
    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<ConfiguredStringWithMacrosPart> {
        match self {
            StringWithMacrosPart::String(val) => {
                Ok(ConfiguredStringWithMacrosPart::String(val.clone()))
            }
            StringWithMacrosPart::Macro(write_to_file, unconfigured) => Ok(
                ConfiguredStringWithMacrosPart::Macro(*write_to_file, unconfigured.configure(ctx)?),
            ),
        }
    }
}

impl<P: ProvidersLabelMaybeConfigured> Display for StringWithMacros<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StringPart(part) => {
                write!(f, "{part}")?;
            }
            Self::ManyParts(parts) => {
                for part in parts.iter() {
                    write!(f, "{part}")?;
                }
            }
        }

        Ok(())
    }
}

/// Represents the type of a query placeholder (e.g. query_outputs, query_targets, query_targets_and_outputs).
#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable, StrongHash)]
pub enum QueryExpansion {
    Output,
    Target,
    /// Holds the separator used between a target and its output. If not provided, they will be space-separated.
    TargetAndOutput(Option<String>),
}

impl Display for QueryExpansion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QueryExpansion::Output => f.write_str("query_outputs ")?,
            QueryExpansion::Target => f.write_str("query_targets ")?,
            QueryExpansion::TargetAndOutput(Some(separator)) => {
                write!(f, "query_targets_and_outputs '{separator}' ")?;
            }
            QueryExpansion::TargetAndOutput(None) => f.write_str("query_targets_and_outputs ")?,
        };

        Ok(())
    }
}
