/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use crate::attrs::attr_type::attr_config::AttrConfig;
use crate::attrs::attr_type::query::QueryMacroBase;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct ArgAttrType;

/// [StringWithMacros] is the core representation for an attr.arg() (in all of it's coerced, configured, and resolved
/// forms). The parsed arg string is held as a sequence of parts (each part either a literal string or a macro). When
/// being added to a command line, these parts will be concattenated together and added as a single arg.
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum StringWithMacros<C: AttrConfig> {
    /// Semantically, StringWithMacros::StringPart(s) is equivalent to
    /// StringWithMacros::ManyParts(vec![StringWithMacrosPart::String(s)]). We special-case this
    /// for memory efficiency to avoid allocating unnecessary vectors, since lone string parts are
    /// very frequent.
    StringPart(Box<str>),
    // For resolution, we defer all work and simply alloc a ConfiguredStringWithMacros into the starlark
    // context. This allows us to defer resolution work to the point that it is being added to a command
    // line, but it requires that we can cheaply copy ConfiguredStringWithMacros.
    ManyParts(Box<[StringWithMacrosPart<C>]>),
}

impl<C: AttrConfig> StringWithMacros<C> {
    pub fn concat(self, items: impl Iterator<Item = anyhow::Result<Self>>) -> anyhow::Result<Self> {
        let mut parts = Vec::new();
        for x in std::iter::once(Ok(self)).chain(items) {
            match x? {
                Self::StringPart(x) => parts.push(StringWithMacrosPart::String(x.into_string())),
                Self::ManyParts(xs) => parts.extend(xs.into_vec().into_iter()),
            }
        }
        Ok(Self::ManyParts(parts.into_boxed_slice()))
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum StringWithMacrosPart<C: AttrConfig> {
    String(String),
    Macro(/* write_to_file */ bool, MacroBase<C>),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum MacroBase<C: AttrConfig> {
    Location(C::ProvidersType),
    /// Represents both $(exe) and $(exe_target) usages.
    Exe {
        label: C::ProvidersType,
        exec_dep: bool,
    },
    /// A user-defined make variable (like `$(CXX)`). This will be resolved based on the propagated TemplateVariableInfos.
    UserUnkeyedPlaceholder(String),

    /// A user-defined macro (like `$(cxxppflags //some:target)`). This will be resolved based on the propagated TemplateVariableInfos.
    UserKeyedPlaceholder(String, C::ProvidersType, Option<String>),

    Query(Box<QueryMacroBase<C>>),

    /// Right now, we defer error for unrecognized macros to the place where they are used. This just allows
    /// us to progress further into a build and detect more issues. Once we have all (or most) of the buckv1 macros
    /// recognized we'll remove this and make it an early error.
    UnrecognizedMacro(String, Vec<String>),
}

/// Display attempts to approximately reproduce the string that created a macro.
impl<C: AttrConfig> Display for MacroBase<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: this should re-escape values in the args that need to be escaped to have returned that arg (it's not possible
        // to tell where there were unnecessary escapes and it's not worth tracking that).
        match self {
            MacroBase::Location(l) => write!(f, "location {}", l),
            MacroBase::Exe { label, exec_dep } => {
                write!(
                    f,
                    "{} {}",
                    if *exec_dep { "exe" } else { "exe_target" },
                    label
                )
            }
            MacroBase::Query(query) => Display::fmt(query, f),
            MacroBase::UserUnkeyedPlaceholder(var) => write!(f, "{}", var),
            MacroBase::UserKeyedPlaceholder(macro_type, target, arg) => write!(
                f,
                "{} {}{}",
                macro_type,
                target,
                if let Some(arg) = arg {
                    format!(" {}", arg)
                } else {
                    "".to_owned()
                }
            ),
            MacroBase::UnrecognizedMacro(macro_type, args) => {
                write!(f, "<unknown>({}) {}", macro_type, args.join(" "))
            }
        }
    }
}

impl<C: AttrConfig> Display for StringWithMacrosPart<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringWithMacrosPart::String(s) => write!(f, "{}", s),
            StringWithMacrosPart::Macro(write_to_file, m) => {
                write!(f, "$({}{})", if *write_to_file { "@" } else { "" }, m)
            }
        }
    }
}

impl<C: AttrConfig> Display for StringWithMacros<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StringPart(part) => {
                write!(f, "{}", part)?;
            }
            Self::ManyParts(ref parts) => {
                for part in parts.iter() {
                    write!(f, "{}", part)?;
                }
            }
        }

        Ok(())
    }
}

/// Represents the type of a query placeholder (e.g. query_outputs, query_targets, query_targets_and_outputs).
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
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
                write!(f, "query_targets_and_outputs '{}' ", separator)?;
            }
            QueryExpansion::TargetAndOutput(None) => f.write_str("query_targets_and_outputs ")?,
        };

        Ok(())
    }
}
