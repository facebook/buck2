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

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::package::PackageLabel;
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use buck2_error::internal_error;
use buck2_util::arc_str::ArcStr;
use buck2_util::size_assert;
use derive_more::Display;
use dupe::Dupe;
use pagable::Pagable;
use serde::Serializer;
use serde_json::to_value;
use smallvec::SmallVec;
use smallvec::smallvec;
use strong_hash::StrongHash;

use crate::attrs::arena::AttrToken;
use crate::attrs::arena::AttrWord;
use crate::attrs::arena::CoercedAttrArena;
use crate::attrs::arena::CoercedAttrArenaBuilder;
use crate::attrs::arena::RefItem;
use crate::attrs::arena::Refs;
use crate::attrs::arena::builder::content_hash;
use crate::attrs::arena::fields::ChildrenOf;
use crate::attrs::arena::fields::Cursor;
use crate::attrs::arena::fields::Extent;
use crate::attrs::arena::fields::IdHasher;
use crate::attrs::arena::fields::Text;
use crate::attrs::arena::schema::Kind;
use crate::attrs::arena::schema::SchemaKind;
use crate::attrs::arena::schema::Storage;
use crate::attrs::arena::schema::encode_kind;
use crate::attrs::arena::storage::RefRun;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::query::QueryAttrBase;
use crate::attrs::attr_type::query::QueryExprRef;
use crate::attrs::attr_type::query::QueryMacroBase;
use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::coerced::configure::Configure;
use crate::attrs::coerced::configure::TraverseConfigured;
use crate::attrs::coerced::kinds;
use crate::attrs::coerced::kinds::CoercedAttrKind;
use crate::attrs::coerced::render::Fmt;
use crate::attrs::coerced::render::JsonKey;
use crate::attrs::coerced::render::MatchesAny;
use crate::attrs::coerced::render::SerializeKind;
use crate::attrs::coerced::render::ToJson;
use crate::attrs::coerced::semantics::Semantics;
use crate::attrs::coerced::semantics::mismatch;
use crate::attrs::coerced::traverse::Traverse;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::strong_hash_without_config::StrongHashWithoutConfig;
use crate::attrs::traversal::CoercedAttrTraversal;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum StringMacroTraversalError {
    #[error("Expected a package when traversing string macro: `{0}`.")]
    NoPackageForSource(String),
}

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

#[derive(
    Debug, Eq, PartialEq, Hash, Clone, Copy, Dupe, Allocative, Pagable, StrongHash
)]
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
size_assert::words_of_type!(StringWithMacros<ProvidersLabel>, 3);

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

impl StrongHashWithoutConfig for StringWithMacros<ConfiguredProvidersLabel> {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::StringPart(part) => {
                "StringPart".strong_hash(state);
                part.strong_hash(state);
            }
            Self::ManyParts(parts) => {
                "ManyParts".strong_hash(state);
                (parts.len() as u64).strong_hash(state);
                parts
                    .iter()
                    .for_each(|part| part.strong_hash_without_config(state));
            }
        }
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

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable, StrongHash)]
pub enum StringWithMacrosPart<P: ProvidersLabelMaybeConfigured> {
    String(ArcStr),
    Macro(/* write_to_file */ bool, MacroBase<P>),
}

impl StrongHashWithoutConfig for StringWithMacrosPart<ConfiguredProvidersLabel> {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::String(value) => {
                "String".strong_hash(state);
                value.strong_hash(state);
            }
            Self::Macro(write_to_file, macro_) => {
                "Macro".strong_hash(state);
                write_to_file.strong_hash(state);
                macro_.strong_hash_without_config(state);
            }
        }
    }
}

size_assert::words_of_type!(MacroBase<ProvidersLabel>, 3);
size_assert::words_of_type!(StringWithMacrosPart<ProvidersLabel>, 4);

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
}

impl StrongHashWithoutConfig for MacroBase<ConfiguredProvidersLabel> {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Location { label, dep_kind } => {
                "Location".strong_hash(state);
                label.strong_hash_without_config(state);
                dep_kind.strong_hash(state);
            }
            Self::Exe { label, exec_dep } => {
                "Exe".strong_hash(state);
                label.strong_hash_without_config(state);
                exec_dep.strong_hash(state);
            }
            Self::UserUnkeyedPlaceholder(name) => {
                "UserUnkeyedPlaceholder".strong_hash(state);
                name.strong_hash(state);
            }
            Self::UserKeyedPlaceholder(value) => {
                "UserKeyedPlaceholder".strong_hash(state);
                let (name, label, arg) = value.as_ref();
                name.strong_hash(state);
                label.strong_hash_without_config(state);
                arg.strong_hash(state);
            }
            Self::Query(query) => {
                "Query".strong_hash(state);
                query.strong_hash_without_config(state);
            }
            Self::Source(path) => {
                "Source".strong_hash(state);
                path.strong_hash(state);
            }
        }
    }
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
            MacroBase::UserUnkeyedPlaceholder(_) => Ok(()),
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
        })
    }

    pub fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
        pkg: Option<PackageLabel>,
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
                let pkg = pkg.ok_or_else(|| {
                    StringMacroTraversalError::NoPackageForSource(path.path().to_string())
                })?;
                for x in path.inputs() {
                    traversal.input(SourcePathRef::new(pkg.dupe(), x))?;
                }
                Ok(())
            }
            MacroBase::UserUnkeyedPlaceholder(_) => Ok(()),
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

#[derive(
    Debug, Eq, PartialEq, Hash, Clone, Allocative, Display, StrongHash, Pagable
)]
#[display("{}", string_with_macros)]
pub struct ConfiguredStringWithMacros {
    pub string_with_macros: StringWithMacros<ConfiguredProvidersLabel>,
    pub anon_target_compatible: bool,
}

impl StrongHashWithoutConfig for ConfiguredStringWithMacros {
    fn strong_hash_without_config<H: Hasher>(&self, state: &mut H) {
        let Self {
            string_with_macros,
            anon_target_compatible,
        } = self;
        string_with_macros.strong_hash_without_config(state);
        anon_target_compatible.strong_hash(state);
    }
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
            QueryExpansion::Output => f.write_str("query_outputs")?,
            QueryExpansion::Target => f.write_str("query_targets")?,
            QueryExpansion::TargetAndOutput(Some(separator)) => {
                write!(f, "query_targets_and_outputs '{separator}'")?;
            }
            QueryExpansion::TargetAndOutput(None) => f.write_str("query_targets_and_outputs")?,
        };

        Ok(())
    }
}

impl<'a> ArgPartView<'a> {
    /// Mirror of `MacroBase::<ProvidersLabel>::configure` off the view.
    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> buck2_error::Result<ConfiguredStringWithMacrosPart> {
        Ok(match self {
            ArgPartView::String(s) => StringWithMacrosPart::String(ArcStr::from(*s)),
            ArgPartView::Macro { write_to_file, mac } => StringWithMacrosPart::Macro(
                *write_to_file,
                match mac {
                    ArgMacroView::Location { label, dep_kind } => ConfiguredMacro::Location {
                        label: match dep_kind {
                            MacroDepKind::Exec => ctx.configure_exec_target(label)?,
                            MacroDepKind::Toolchain => ctx.configure_toolchain_target(label),
                            MacroDepKind::Regular => ctx.configure_target(label),
                        },
                        dep_kind: *dep_kind,
                    },
                    ArgMacroView::Exe { label, exec_dep } => ConfiguredMacro::Exe {
                        label: if *exec_dep {
                            ctx.configure_exec_target(label)?
                        } else {
                            ctx.configure_target(label)
                        },
                        exec_dep: *exec_dep,
                    },
                    ArgMacroView::UserUnkeyedPlaceholder(name) => {
                        ConfiguredMacro::UserUnkeyedPlaceholder(Box::from(*name))
                    }
                    ArgMacroView::UserKeyedPlaceholder { var, label, arg } => {
                        ConfiguredMacro::UserKeyedPlaceholder(Box::new((
                            Box::from(*var),
                            ctx.configure_target(label),
                            arg.map(Box::from),
                        )))
                    }
                    ArgMacroView::Source(path) => ConfiguredMacro::Source((*path).clone()),
                    ArgMacroView::Query(q) => ConfiguredMacro::Query(Box::new(q.configure(ctx))),
                },
            ),
        })
    }
}

impl SchemaKind for kinds::ArgSimple {
    type Fields = Text;
}

impl CoercedAttrArenaBuilder {
    pub fn encode_arg_simple(&mut self, s: &str) -> buck2_error::Result<AttrToken> {
        encode_kind::<kinds::ArgSimple>(self, s)
    }
}

impl Semantics for kinds::ArgSimple {
    const CONCAT: Option<&'static str> = Some("arg");

    fn traverse<'a>(_: Self::View<'a>, _: Traverse<'_, 'a>) -> buck2_error::Result<()> {
        Ok(())
    }

    fn configure(
        s: Self::View<'_>,
        Configure { v, t, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match &t.non_optional().0.inner {
            AttrTypeInner::Arg(ty) => {
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Arg(ConfiguredStringWithMacros {
                    string_with_macros: StringWithMacros::StringPart(ArcStr::from(s)),
                    anon_target_compatible: ty.anon_target_compatible,
                }))
            }
            _ => mismatch(v, t).into(),
        }
    }

    fn traverse_configured(
        _: Self::View<'_>,
        _: TraverseConfigured<'_, '_>,
    ) -> ResultMaybeCompatible<()> {
        ResultMaybeCompatible::Compatible(())
    }

    fn to_json(s: Self::View<'_>, _: ToJson<'_>) -> buck2_error::Result<serde_json::Value> {
        Ok(to_value(s)?)
    }

    fn fmt(s: Self::View<'_>, Fmt { f, .. }: Fmt<'_, '_>) -> std::fmt::Result {
        write!(f, "\"{s}\"")
    }

    fn any_matches(
        s: Self::View<'_>,
        MatchesAny { filter }: MatchesAny<'_>,
    ) -> buck2_error::Result<bool> {
        filter(s)
    }

    fn json_key<'a>(s: Self::View<'a>, _: JsonKey<'_>) -> buck2_error::Result<Cow<'a, str>> {
        Ok(Cow::Borrowed(s))
    }

    fn serialize<S: Serializer>(
        s: Self::View<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, S>,
    ) -> Result<S::Ok, S::Error> {
        ser.serialize_str(s)
    }
}

impl SchemaKind for kinds::Arg {
    type Fields = ChildrenOf<ArgParts>;
}

impl CoercedAttrArenaBuilder {
    pub fn encode_arg_parts(&mut self, parts: &[AttrToken]) -> buck2_error::Result<AttrToken> {
        encode_kind::<kinds::Arg>(self, parts)
    }
}

impl Semantics for kinds::Arg {
    const CONCAT: Option<&'static str> = Some("arg");

    fn traverse<'a>(
        parts: Self::View<'a>,
        Traverse { v, t, pkg, tr }: Traverse<'_, 'a>,
    ) -> buck2_error::Result<()> {
        if !matches!(&t.non_optional().0.inner, AttrTypeInner::Arg(_)) {
            return Err(mismatch(v, t));
        }
        for part in parts.iter() {
            let ArgPartView::Macro { mac, .. } = part else {
                continue;
            };
            match mac {
                ArgMacroView::UserKeyedPlaceholder { label, .. } => tr.dep(label)?,
                ArgMacroView::Location {
                    label,
                    dep_kind: MacroDepKind::Exec,
                } => tr.exec_dep(label)?,
                ArgMacroView::Location {
                    label,
                    dep_kind: MacroDepKind::Toolchain,
                } => tr.toolchain_dep(label)?,
                ArgMacroView::Location {
                    label,
                    dep_kind: MacroDepKind::Regular,
                } => tr.dep(label)?,
                ArgMacroView::Exe {
                    label,
                    exec_dep: true,
                } => tr.exec_dep(label)?,
                ArgMacroView::Exe {
                    label,
                    exec_dep: false,
                } => tr.dep(label)?,
                ArgMacroView::Source(path) => {
                    let pkg = pkg.ok_or_else(|| {
                        internal_error!("Expected a package when traversing arg source macro")
                    })?;
                    for x in path.inputs() {
                        tr.input(SourcePathRef::new(pkg, x))?;
                    }
                }
                // Literal labels only, in expression order.
                ArgMacroView::Query(q) => {
                    let mut labels = Vec::new();
                    q.literal_labels(&mut labels);
                    for label in labels {
                        tr.dep(label)?;
                    }
                }
                ArgMacroView::UserUnkeyedPlaceholder(_) => {}
            }
        }
        Ok(())
    }

    fn configure(
        parts: Self::View<'_>,
        Configure { v, t, ctx, .. }: Configure<'_, '_>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        match &t.non_optional().0.inner {
            AttrTypeInner::Arg(ty) => {
                let configured = parts
                    .iter()
                    .map(|p| p.configure(ctx))
                    .collect::<buck2_error::Result<Box<[_]>>>()?;
                ResultMaybeCompatible::Compatible(ConfiguredAttr::Arg(ConfiguredStringWithMacros {
                    string_with_macros: StringWithMacros::ManyParts(configured),
                    anon_target_compatible: ty.anon_target_compatible,
                }))
            }
            _ => mismatch(v, t).into(),
        }
    }

    fn to_json(parts: Self::View<'_>, _: ToJson<'_>) -> buck2_error::Result<serde_json::Value> {
        Ok(to_value(parts.to_string())?)
    }

    fn fmt(parts: Self::View<'_>, Fmt { f, .. }: Fmt<'_, '_>) -> std::fmt::Result {
        write!(f, "\"{parts}\"")
    }

    fn any_matches(
        parts: Self::View<'_>,
        MatchesAny { filter }: MatchesAny<'_>,
    ) -> buck2_error::Result<bool> {
        filter(&parts.to_string())
    }

    fn json_key<'a>(parts: Self::View<'a>, _: JsonKey<'_>) -> buck2_error::Result<Cow<'a, str>> {
        Ok(Cow::Owned(parts.to_string()))
    }

    fn serialize<S: Serializer>(
        parts: Self::View<'_>,
        SerializeKind { ser, .. }: SerializeKind<'_, S>,
    ) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&parts.to_string())
    }
}

/// Arg macro record header (its first head field):
/// - bits 0-3: variant tag (`ARG_MACRO_TAG_*`)
/// - bits 4-11: variant flag byte — location: `MacroDepKind`; exe:
///   `exec_dep`; keyed placeholder: has-arg; query: expansion kind in
///   flag bits 0-1, has-separator in flag bit 2
/// - bit 12: `write_to_file`
const ARG_MACRO_TAG_MASK: u32 = 0xF;
const ARG_MACRO_FLAG_MASK: u32 = 0xFF;
const ARG_MACRO_TAG_LOCATION: u32 = 0;
const ARG_MACRO_TAG_EXE: u32 = 1;
const ARG_MACRO_TAG_UNKEYED: u32 = 2;
const ARG_MACRO_TAG_KEYED: u32 = 3;
const ARG_MACRO_TAG_SOURCE: u32 = 4;
/// Refs: `[expr, sep]`; `expr` is the parsed expression as a `QueryExpr`
/// record, `sep` a text record present when the has-separator flag bit is
/// set.
const ARG_MACRO_TAG_QUERY: u32 = 5;
const ARG_MACRO_FLAG_SHIFT: u32 = 4;
const ARG_MACRO_WTF_BIT: u32 = 1 << 12;

const ARG_QUERY_EXPANSION_MASK: u32 = 0x3;
const ARG_QUERY_EXPANSION_OUTPUT: u32 = 0;
const ARG_QUERY_EXPANSION_TARGET: u32 = 1;
const ARG_QUERY_EXPANSION_TARGET_AND_OUTPUT: u32 = 2;
const ARG_QUERY_HAS_SEP: u32 = 4;

fn dep_kind_bits(k: MacroDepKind) -> u32 {
    match k {
        MacroDepKind::Regular => 0,
        MacroDepKind::Exec => 1,
        MacroDepKind::Toolchain => 2,
    }
}

fn dep_kind_from_bits(bits: u32) -> MacroDepKind {
    match bits {
        0 => MacroDepKind::Regular,
        1 => MacroDepKind::Exec,
        2 => MacroDepKind::Toolchain,
        _ => unreachable!("should be valid: records are only written by the encoder"),
    }
}

/// Header, payload index, and refs of an `ArgMacro` record.
fn macro_parts(arena: &CoercedAttrArena, r: AttrWord) -> (u32, u32, RefRun<'_>) {
    let mut c = Cursor::new(arena, r);
    let header = c.raw();
    let payload = c.raw();
    let n = c.raw();
    (header, payload, c.refs(n))
}

impl<'a> ArgPartView<'a> {
    /// The part at `r`: an `ArgSimple` text record or an `ArgMacro` record.
    fn decode(arena: &'a CoercedAttrArena, r: AttrWord) -> ArgPartView<'a> {
        match arena.kind_of(r) {
            CoercedAttrKind::ArgSimple => ArgPartView::String(arena.str_at(r)),
            CoercedAttrKind::ArgMacro => {
                let (header, payload, refs) = macro_parts(arena, r);
                let flag = (header >> ARG_MACRO_FLAG_SHIFT) & ARG_MACRO_FLAG_MASK;
                let mac = match header & ARG_MACRO_TAG_MASK {
                    ARG_MACRO_TAG_LOCATION => ArgMacroView::Location {
                        label: &arena.providers_labels[payload as usize],
                        dep_kind: dep_kind_from_bits(flag),
                    },
                    ARG_MACRO_TAG_EXE => ArgMacroView::Exe {
                        label: &arena.providers_labels[payload as usize],
                        exec_dep: flag != 0,
                    },
                    ARG_MACRO_TAG_UNKEYED => {
                        ArgMacroView::UserUnkeyedPlaceholder(arena.str_at(refs.get(0)))
                    }
                    ARG_MACRO_TAG_KEYED => ArgMacroView::UserKeyedPlaceholder {
                        var: arena.str_at(refs.get(0)),
                        label: &arena.providers_labels[payload as usize],
                        arg: (flag != 0).then(|| arena.str_at(refs.get(1))),
                    },
                    ARG_MACRO_TAG_SOURCE => ArgMacroView::Source(&arena.paths[payload as usize]),
                    ARG_MACRO_TAG_QUERY => ArgMacroView::Query(ArgQueryView { arena, r }),
                    _ => unreachable!("should be valid: records are only written by the encoder"),
                };
                ArgPartView::Macro {
                    write_to_file: header & ARG_MACRO_WTF_BIT != 0,
                    mac,
                }
            }
            _ => unreachable!("should be valid: arg parts are ArgSimple or ArgMacro refs"),
        }
    }
}

/// One arg part (`ArgSimple` or `ArgMacro` record) per ref.
pub struct ArgParts;

impl RefItem for ArgParts {
    const STRIDE: u32 = 1;
    type Item<'a> = ArgPartView<'a>;
    type Spec = [AttrToken];
    fn item<'a>(refs: Refs<'a, Self>, i: u32) -> ArgPartView<'a> {
        ArgPartView::decode(refs.run.arena, refs.run.get(i))
    }
    fn tokens(spec: &[AttrToken]) -> impl Iterator<Item = AttrToken> + '_ {
        spec.iter().copied()
    }
}

/// An `ArgMacro` record decodes to its part view; the `String` case is an
/// `ArgSimple` record and never reaches here.
impl Kind for kinds::ArgMacro {
    const STORAGE: Storage = Storage::Record;
    const SCALAR: bool = false;
    type View<'a> = ArgPartView<'a>;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<Extent> {
        Some(Extent {
            head: 3,
            refs: Some(peek(2)?),
            text: None,
        })
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> ArgPartView<'a> {
        ArgPartView::decode(arena, r)
    }

    fn copy_into(
        parts: ArgPartView<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken> {
        b.encode_arg_part_view(parts)
    }

    fn identity(parts: ArgPartView<'_>, h: &mut IdHasher<'_>) {
        let h = &mut h.h;
        let ArgPartView::Macro { write_to_file, mac } = parts else {
            unreachable!("should be a macro: `ArgMacro` refs decode to macro parts")
        };
        write_to_file.strong_hash(h);
        match mac {
            ArgMacroView::Location { label, dep_kind } => {
                h.write_u8(ARG_MACRO_TAG_LOCATION as u8);
                h.write_u8(dep_kind_bits(dep_kind) as u8);
                label.strong_hash(h);
            }
            ArgMacroView::Exe { label, exec_dep } => {
                h.write_u8(ARG_MACRO_TAG_EXE as u8);
                exec_dep.strong_hash(h);
                label.strong_hash(h);
            }
            ArgMacroView::UserUnkeyedPlaceholder(var) => {
                h.write_u8(ARG_MACRO_TAG_UNKEYED as u8);
                var.strong_hash(h);
            }
            ArgMacroView::UserKeyedPlaceholder { var, label, arg } => {
                h.write_u8(ARG_MACRO_TAG_KEYED as u8);
                var.strong_hash(h);
                label.strong_hash(h);
                arg.strong_hash(h);
            }
            ArgMacroView::Source(path) => {
                h.write_u8(ARG_MACRO_TAG_SOURCE as u8);
                path.strong_hash(h);
            }
            ArgMacroView::Query(q) => {
                h.write_u8(ARG_MACRO_TAG_QUERY as u8);
                let (kind, sep) = q.expansion_parts();
                h.write_u8(kind as u8);
                sep.strong_hash(h);
                q.expr().strong_hash(h);
            }
        }
    }

    fn same(a: ArgPartView<'_>, b: ArgPartView<'_>) -> bool {
        a == b
    }
}

/// One decoded part of an `Arg` value containing macros.
#[derive(Copy, Clone, PartialEq)]
pub enum ArgPartView<'a> {
    String(&'a str),
    Macro {
        write_to_file: bool,
        mac: ArgMacroView<'a>,
    },
}

/// One decoded arg macro record.
#[derive(Copy, Clone, PartialEq)]
pub enum ArgMacroView<'a> {
    Location {
        label: &'a ProvidersLabel,
        dep_kind: MacroDepKind,
    },
    Exe {
        label: &'a ProvidersLabel,
        exec_dep: bool,
    },
    UserUnkeyedPlaceholder(&'a str),
    UserKeyedPlaceholder {
        var: &'a str,
        label: &'a ProvidersLabel,
        arg: Option<&'a str>,
    },
    Source(&'a CoercedPath),
    Query(ArgQueryView<'a>),
}

/// Mirrors `Display for MacroBase`: approximately the source text of the
/// macro, without the `$(...)` wrapper.
impl std::fmt::Display for ArgMacroView<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArgMacroView::Location { label, dep_kind } => {
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
            ArgMacroView::Exe { label, exec_dep } => {
                write!(
                    f,
                    "{} {}",
                    if *exec_dep { "exe" } else { "exe_target" },
                    label
                )
            }
            ArgMacroView::UserUnkeyedPlaceholder(var) => write!(f, "{var}"),
            ArgMacroView::UserKeyedPlaceholder { var, label, arg } => {
                write!(f, "{var} {label}")?;
                if let Some(arg) = arg {
                    write!(f, " {arg}")?;
                }
                Ok(())
            }
            ArgMacroView::Source(path) => write!(f, "src {}", path.path()),
            ArgMacroView::Query(q) => write!(f, "{} {}", q.expansion_type(), q.query().0),
        }
    }
}

/// Mirrors `Display for StringWithMacros`: the parts concatenated, string
/// parts raw and macro parts as `$(...)`, without surrounding quotes.
impl std::fmt::Display for Refs<'_, ArgParts> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for part in self.iter() {
            match part {
                ArgPartView::String(s) => write!(f, "{s}")?,
                ArgPartView::Macro { write_to_file, mac } => {
                    write!(f, "$({}{})", if write_to_file { "@" } else { "" }, mac)?
                }
            }
        }
        Ok(())
    }
}

/// Decoded view of a query macro record (`$(query_targets ...)` etc.).
#[derive(Copy, Clone)]
pub struct ArgQueryView<'a> {
    arena: &'a CoercedAttrArena,
    r: AttrWord,
}

impl<'a> ArgQueryView<'a> {
    fn flags(&self) -> u32 {
        let (header, _, _) = macro_parts(self.arena, self.r);
        (header >> ARG_MACRO_FLAG_SHIFT) & ARG_MACRO_FLAG_MASK
    }

    pub(crate) fn expr(&self) -> QueryExprRef<'a> {
        let (_, _, refs) = macro_parts(self.arena, self.r);
        QueryExprRef {
            arena: self.arena,
            r: refs.get(0),
        }
    }

    fn separator(&self) -> Option<&'a str> {
        let (_, _, refs) = macro_parts(self.arena, self.r);
        (self.flags() & ARG_QUERY_HAS_SEP != 0).then(|| self.arena.str_at(refs.get(1)))
    }

    /// Expansion kind bits and the borrowed separator, for readers that must
    /// not allocate; the owned [`QueryExpansion`] clones the separator.
    fn expansion_parts(&self) -> (u32, Option<&'a str>) {
        (self.flags() & ARG_QUERY_EXPANSION_MASK, self.separator())
    }

    fn expansion_type(&self) -> QueryExpansion {
        match self.flags() & ARG_QUERY_EXPANSION_MASK {
            ARG_QUERY_EXPANSION_OUTPUT => QueryExpansion::Output,
            ARG_QUERY_EXPANSION_TARGET => QueryExpansion::Target,
            ARG_QUERY_EXPANSION_TARGET_AND_OUTPUT => {
                QueryExpansion::TargetAndOutput(self.separator().map(str::to_owned))
            }
            _ => unreachable!("should be valid: records are only written by the encoder"),
        }
    }

    /// The canonical reprint of the stored expression plus its literal map,
    /// with offsets into the reprint. One walk produces both, so the map's
    /// spans always agree with the returned text.
    pub fn query(&self) -> (String, ResolvedQueryLiterals<ProvidersLabel>) {
        self.expr().query()
    }

    /// The stored expression's literal labels, in expression order (the
    /// same order as the offset-sorted map of the original text).
    pub fn literal_labels(&self, out: &mut Vec<&'a ProvidersLabel>) {
        self.expr().labels(out);
    }

    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> QueryMacroBase<ConfiguredProvidersLabel> {
        let mut query = String::new();
        let mut lits = BTreeMap::new();
        self.expr().reprint(&mut query, &mut |span, l| {
            lits.insert(span, ctx.configure_target(l));
        });
        QueryMacroBase {
            expansion_type: self.expansion_type(),
            query: QueryAttrBase {
                query,
                resolved_literals: ResolvedQueryLiterals(lits),
            },
        }
    }
}

impl PartialEq for ArgQueryView<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.flags() == other.flags()
            && self.separator() == other.separator()
            && self.query() == other.query()
    }
}

/// A macro's content as its record stores it: labels and paths by
/// reference, nested text as text, the query expression already encoded.
/// Both the parsed and the decoded form of a macro convert to this, so the
/// record layout and its content hash are defined once.
enum MacroParts<'a> {
    Location {
        label: &'a ProvidersLabel,
        dep_kind: MacroDepKind,
    },
    Exe {
        label: &'a ProvidersLabel,
        exec_dep: bool,
    },
    UserUnkeyedPlaceholder(&'a str),
    UserKeyedPlaceholder {
        var: &'a str,
        label: &'a ProvidersLabel,
        arg: Option<&'a str>,
    },
    Source(&'a CoercedPath),
    Query {
        expr: AttrToken,
        expansion: u32,
        sep: Option<&'a str>,
    },
}

impl CoercedAttrArenaBuilder {
    /// Encode an `arg()` value: simple text and macros as records.
    pub fn encode_arg(
        &mut self,
        arg: &StringWithMacros<ProvidersLabel>,
    ) -> buck2_error::Result<AttrToken> {
        match arg {
            StringWithMacros::StringPart(part) => self.encode_arg_simple(part),
            StringWithMacros::ManyParts(parts) => {
                let parts = parts
                    .iter()
                    .map(|p| self.encode_arg_part(p))
                    .collect::<buck2_error::Result<SmallVec<[AttrToken; 8]>>>()?;
                self.encode_arg_parts(&parts)
            }
        }
    }

    /// Encode one arg part: string parts are `ArgSimple` text records;
    /// macro parts dedup as `ArgMacro` records with labels in the label
    /// table.
    fn encode_arg_part(
        &mut self,
        part: &StringWithMacrosPart<ProvidersLabel>,
    ) -> buck2_error::Result<AttrToken> {
        let (write_to_file, m) = match part {
            StringWithMacrosPart::String(s) => {
                return self.encode_str(CoercedAttrKind::ArgSimple, s);
            }
            StringWithMacrosPart::Macro(write_to_file, m) => (*write_to_file, m),
        };
        let parts = match m {
            MacroBase::Location { label, dep_kind } => MacroParts::Location {
                label,
                dep_kind: *dep_kind,
            },
            MacroBase::Exe { label, exec_dep } => MacroParts::Exe {
                label,
                exec_dep: *exec_dep,
            },
            MacroBase::UserUnkeyedPlaceholder(name) => MacroParts::UserUnkeyedPlaceholder(name),
            MacroBase::UserKeyedPlaceholder(box (var, label, arg)) => {
                MacroParts::UserKeyedPlaceholder {
                    var,
                    label,
                    arg: arg.as_deref(),
                }
            }
            MacroBase::Source(path) => MacroParts::Source(path),
            MacroBase::Query(q) => {
                let parsed = buck2_query_parser::parse_expr(&q.query.query)?;
                let expr =
                    self.encode_query_expr(&parsed, &q.query.query, &q.query.resolved_literals)?;
                let (expansion, sep) = match &q.expansion_type {
                    QueryExpansion::Output => (ARG_QUERY_EXPANSION_OUTPUT, None),
                    QueryExpansion::Target => (ARG_QUERY_EXPANSION_TARGET, None),
                    QueryExpansion::TargetAndOutput(sep) => {
                        (ARG_QUERY_EXPANSION_TARGET_AND_OUTPUT, sep.as_deref())
                    }
                };
                MacroParts::Query {
                    expr,
                    expansion,
                    sep,
                }
            }
        };
        self.encode_macro(write_to_file, parts)
    }

    /// [`Self::encode_arg_part`] from a decoded part: the same records and
    /// the same hashes, so copies dedup against parsed originals.
    fn encode_arg_part_view(&mut self, part: ArgPartView<'_>) -> buck2_error::Result<AttrToken> {
        let (write_to_file, mac) = match part {
            ArgPartView::String(s) => return self.encode_str(CoercedAttrKind::ArgSimple, s),
            ArgPartView::Macro { write_to_file, mac } => (write_to_file, mac),
        };
        let parts = match mac {
            ArgMacroView::Location { label, dep_kind } => MacroParts::Location { label, dep_kind },
            ArgMacroView::Exe { label, exec_dep } => MacroParts::Exe { label, exec_dep },
            ArgMacroView::UserUnkeyedPlaceholder(name) => MacroParts::UserUnkeyedPlaceholder(name),
            ArgMacroView::UserKeyedPlaceholder { var, label, arg } => {
                MacroParts::UserKeyedPlaceholder { var, label, arg }
            }
            ArgMacroView::Source(path) => MacroParts::Source(path),
            ArgMacroView::Query(q) => {
                let expr = self.encode_query_expr_view(q.expr())?;
                let (expansion, sep) = q.expansion_parts();
                MacroParts::Query {
                    expr,
                    expansion,
                    sep,
                }
            }
        };
        self.encode_macro(write_to_file, parts)
    }

    /// One `ArgMacro` record: head `[header, payload, n]` then `n` refs; the
    /// content hash folds the same components in the same order.
    fn encode_macro(
        &mut self,
        write_to_file: bool,
        mac: MacroParts<'_>,
    ) -> buck2_error::Result<AttrToken> {
        let (tag, flag, payload, refs, hash_parts): (
            u32,
            u32,
            u32,
            SmallVec<[AttrWord; 2]>,
            SmallVec<[u64; 3]>,
        ) = match mac {
            MacroParts::Location { label, dep_kind } => {
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                (
                    ARG_MACRO_TAG_LOCATION,
                    dep_kind_bits(dep_kind),
                    idx,
                    smallvec![],
                    smallvec![lh],
                )
            }
            MacroParts::Exe { label, exec_dep } => {
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                (
                    ARG_MACRO_TAG_EXE,
                    exec_dep as u32,
                    idx,
                    smallvec![],
                    smallvec![lh],
                )
            }
            MacroParts::UserUnkeyedPlaceholder(name) => {
                let AttrToken(sr, sh) = self.encode_str(CoercedAttrKind::String, name)?;
                (ARG_MACRO_TAG_UNKEYED, 0, 0, smallvec![sr], smallvec![sh.0])
            }
            MacroParts::UserKeyedPlaceholder { var, label, arg } => {
                let AttrToken(vr, vh) = self.encode_str(CoercedAttrKind::String, var)?;
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                let mut refs: SmallVec<[AttrWord; 2]> = smallvec![vr];
                let mut parts: SmallVec<[u64; 3]> = smallvec![vh.0, lh];
                let mut flag = 0;
                if let Some(a) = arg {
                    let AttrToken(ar, ah) = self.encode_str(CoercedAttrKind::String, a)?;
                    refs.push(ar);
                    parts.push(ah.0);
                    flag = 1;
                }
                (ARG_MACRO_TAG_KEYED, flag, idx, refs, parts)
            }
            MacroParts::Source(path) => {
                let (idx, path_hash) = self.source_path_idx_and_hash(path);
                (
                    ARG_MACRO_TAG_SOURCE,
                    0,
                    idx,
                    smallvec![],
                    smallvec![path_hash],
                )
            }
            MacroParts::Query {
                expr,
                expansion,
                sep,
            } => {
                let mut refs: SmallVec<[AttrWord; 2]> = smallvec![expr.0];
                let mut parts: SmallVec<[u64; 3]> = smallvec![expr.1.0];
                let mut flags = expansion;
                if let Some(sp) = sep {
                    let AttrToken(sr, sh) = self.encode_str(CoercedAttrKind::String, sp)?;
                    refs.push(sr);
                    parts.push(sh.0);
                    flags |= ARG_QUERY_HAS_SEP;
                }
                (ARG_MACRO_TAG_QUERY, flags, 0, refs, parts)
            }
        };
        let hash = content_hash(CoercedAttrKind::ArgMacro, |h| {
            (tag, flag, write_to_file, &hash_parts).hash(h)
        });
        let wtf_bit = if write_to_file { ARG_MACRO_WTF_BIT } else { 0 };
        let head = [
            tag | (flag << ARG_MACRO_FLAG_SHIFT) | wtf_bit,
            payload,
            refs.len() as u32,
        ];
        let r = self.encode_record(CoercedAttrKind::ArgMacro, hash, &head, &refs)?;
        Ok(AttrToken(r, hash))
    }
}
