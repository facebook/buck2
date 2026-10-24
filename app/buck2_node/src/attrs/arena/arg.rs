/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! `attrs.arg()` values: the macro record layout, its decoded views and
//! its encoders.

use std::collections::BTreeMap;
use std::hash::Hash;
use std::hash::Hasher;

use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use smallvec::SmallVec;
use smallvec::smallvec;
use strong_hash::StrongHash;

use crate::attrs::arena::builder::CoercedAttrArenaBuilder;
use crate::attrs::arena::builder::content_hash;
use crate::attrs::arena::builder::token;
use crate::attrs::arena::fields::IdHasher;
use crate::attrs::arena::query_expr::QueryExprRef;
use crate::attrs::arena::schema::AttrKind;
use crate::attrs::arena::schema::Kind;
use crate::attrs::arena::schema::kinds;
use crate::attrs::arena::storage::AttrToken;
use crate::attrs::arena::storage::AttrWord;
use crate::attrs::arena::storage::CoercedAttrArena;
use crate::attrs::arena::storage::CoercedAttrHash;
use crate::attrs::arena::storage::RefRun;
use crate::attrs::arena::storage::TailLen;
use crate::attrs::arena::view::ArgParts;
use crate::attrs::arena::view::CoercedAttrRef;
use crate::attrs::arena::view::CoercedAttrRefKind;
use crate::attrs::arena::view::Refs;
use crate::attrs::attr_type::arg::ConfiguredMacro;
use crate::attrs::attr_type::arg::ConfiguredStringWithMacrosPart;
use crate::attrs::attr_type::arg::MacroBase;
use crate::attrs::attr_type::arg::MacroDepKind;
use crate::attrs::attr_type::arg::QueryExpansion;
use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::arg::StringWithMacrosPart;
use crate::attrs::attr_type::query::QueryAttrBase;
use crate::attrs::attr_type::query::QueryMacroBase;
use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::configuration_context::AttrConfigurationContext;

/// Arg macro record header (its first head field):
/// - bits 0-3: variant tag (`ARG_MACRO_TAG_*`)
/// - bits 4-11: variant flag byte — location: `MacroDepKind`; exe:
///   `exec_dep`; keyed placeholder: has-arg; query: expansion kind in
///   flag bits 0-1, has-separator in flag bit 2
/// - bit 12: `write_to_file`
pub(super) const ARG_MACRO_TAG_MASK: u32 = 0xF;
pub(super) const ARG_MACRO_FLAG_MASK: u32 = 0xFF;
pub(super) const ARG_MACRO_TAG_LOCATION: u32 = 0;
pub(super) const ARG_MACRO_TAG_EXE: u32 = 1;
pub(super) const ARG_MACRO_TAG_UNKEYED: u32 = 2;
pub(super) const ARG_MACRO_TAG_KEYED: u32 = 3;
pub(super) const ARG_MACRO_TAG_SOURCE: u32 = 4;
/// Refs: `[expr, sep]`; `expr` is the parsed expression as a `QueryExpr`
/// record, `sep` a text record present when the has-separator flag bit is
/// set.
pub(super) const ARG_MACRO_TAG_QUERY: u32 = 5;
pub(super) const ARG_MACRO_FLAG_SHIFT: u32 = 4;
pub(super) const ARG_MACRO_WTF_BIT: u32 = 1 << 12;

pub(super) const ARG_QUERY_EXPANSION_MASK: u32 = 0x3;
pub(super) const ARG_QUERY_EXPANSION_OUTPUT: u32 = 0;
pub(super) const ARG_QUERY_EXPANSION_TARGET: u32 = 1;
pub(super) const ARG_QUERY_EXPANSION_TARGET_AND_OUTPUT: u32 = 2;
pub(super) const ARG_QUERY_HAS_SEP: u32 = 4;

pub(super) fn dep_kind_bits(k: MacroDepKind) -> u32 {
    match k {
        MacroDepKind::Regular => 0,
        MacroDepKind::Exec => 1,
        MacroDepKind::Toolchain => 2,
    }
}

pub(super) fn dep_kind_from_bits(bits: u32) -> MacroDepKind {
    match bits {
        0 => MacroDepKind::Regular,
        1 => MacroDepKind::Exec,
        2 => MacroDepKind::Toolchain,
        _ => unreachable!("should be valid: records are only written by the encoder"),
    }
}

impl CoercedAttrArena {
    /// Header, payload index, and refs of an `ArgMacro` record.
    pub(super) fn arg_macro_parts(&self, r: AttrWord) -> (u32, u32, RefRun<'_>) {
        let rec = self.open(r);
        (rec.head.raw(0), rec.head.raw(1), rec.refs())
    }

    pub(super) fn arg_part_at(&self, r: AttrWord) -> ArgPartView<'_> {
        match self.kind_of(r) {
            AttrKind::ArgSimple => ArgPartView::String(self.str_at(r)),
            AttrKind::ArgMacro => {
                let (header, payload, refs) = self.arg_macro_parts(r);
                let flag = (header >> ARG_MACRO_FLAG_SHIFT) & ARG_MACRO_FLAG_MASK;
                let mac = match header & ARG_MACRO_TAG_MASK {
                    ARG_MACRO_TAG_LOCATION => ArgMacroView::Location {
                        label: &self.providers_labels[payload as usize],
                        dep_kind: dep_kind_from_bits(flag),
                    },
                    ARG_MACRO_TAG_EXE => ArgMacroView::Exe {
                        label: &self.providers_labels[payload as usize],
                        exec_dep: flag != 0,
                    },
                    ARG_MACRO_TAG_UNKEYED => {
                        ArgMacroView::UserUnkeyedPlaceholder(self.str_at(refs.get(0)))
                    }
                    ARG_MACRO_TAG_KEYED => ArgMacroView::UserKeyedPlaceholder {
                        var: self.str_at(refs.get(0)),
                        label: &self.providers_labels[payload as usize],
                        arg: (flag != 0).then(|| self.str_at(refs.get(1))),
                    },
                    ARG_MACRO_TAG_SOURCE => ArgMacroView::Source(&self.paths[payload as usize]),
                    ARG_MACRO_TAG_QUERY => ArgMacroView::Query(ArgQueryView { arena: self, r }),
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

/// An `ArgMacro` record decodes to its part view; the `String` case is an
/// `ArgSimple` record and never reaches here.
impl Kind for kinds::ArgMacro {
    const KIND: AttrKind = AttrKind::ArgMacro;
    const SCALAR: bool = false;
    type Parts<'a> = ArgPartView<'a>;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        Some((3, TailLen::Refs(peek(2)?)))
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> ArgPartView<'a> {
        arena.arg_part_at(r)
    }

    fn wrap<'a>(_v: CoercedAttrRef<'a>, _parts: ArgPartView<'a>) -> CoercedAttrRefKind<'a>
    where
        Self: 'a,
    {
        unreachable!("arg macro records are not attribute values")
    }

    fn copy_into(
        parts: ArgPartView<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken> {
        b.encode_arg_part_view(parts).map(token)
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
    pub(super) arena: &'a CoercedAttrArena,
    pub(super) r: AttrWord,
}

impl<'a> ArgQueryView<'a> {
    pub(super) fn flags(&self) -> u32 {
        let (header, _, _) = self.arena.arg_macro_parts(self.r);
        (header >> ARG_MACRO_FLAG_SHIFT) & ARG_MACRO_FLAG_MASK
    }

    pub(super) fn expr(&self) -> QueryExprRef<'a> {
        let (_, _, refs) = self.arena.arg_macro_parts(self.r);
        QueryExprRef {
            arena: self.arena,
            r: refs.get(0),
        }
    }

    pub(super) fn separator(&self) -> Option<&'a str> {
        let (_, _, refs) = self.arena.arg_macro_parts(self.r);
        (self.flags() & ARG_QUERY_HAS_SEP != 0).then(|| self.arena.str_at(refs.get(1)))
    }

    /// Expansion kind bits and the borrowed separator, for readers that must
    /// not allocate; the owned [`QueryExpansion`] clones the separator.
    pub(super) fn expansion_parts(&self) -> (u32, Option<&'a str>) {
        (self.flags() & ARG_QUERY_EXPANSION_MASK, self.separator())
    }

    pub(super) fn expansion_type(&self) -> QueryExpansion {
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
        let mut out = String::new();
        let mut lits = BTreeMap::new();
        self.expr().reprint(&mut out, &mut |span, l| {
            lits.insert(span, l.dupe());
        });
        (out, ResolvedQueryLiterals(lits))
    }

    /// The stored expression's literal labels, in expression order (the
    /// same order as the offset-sorted map of the original text).
    pub fn literal_labels(&self, out: &mut Vec<&'a ProvidersLabel>) {
        self.expr().labels(out);
    }

    pub(super) fn configure(
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
                    .map(|p| self.encode_arg_part(p).map(token))
                    .collect::<buck2_error::Result<SmallVec<[AttrToken; 8]>>>()?;
                self.encode_arg_parts(&parts)
            }
        }
    }

    /// [`Self::encode_arg_part`] from a decoded part: the same records and
    /// the same hashes, so copies dedup against parsed originals.
    pub(super) fn encode_arg_part_view(
        &mut self,
        part: ArgPartView<'_>,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        let (write_to_file, mac) = match part {
            ArgPartView::String(s) => return self.encode_str(AttrKind::ArgSimple, s),
            ArgPartView::Macro { write_to_file, mac } => (write_to_file, mac),
        };
        let wtf_bit = if write_to_file { ARG_MACRO_WTF_BIT } else { 0 };
        let (tag, flag, payload, refs, hash_parts): (
            u32,
            u32,
            u32,
            SmallVec<[AttrWord; 2]>,
            SmallVec<[u64; 3]>,
        ) = match mac {
            ArgMacroView::Location { label, dep_kind } => {
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                (
                    ARG_MACRO_TAG_LOCATION,
                    dep_kind_bits(dep_kind),
                    idx,
                    smallvec![],
                    smallvec![lh],
                )
            }
            ArgMacroView::Exe { label, exec_dep } => {
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                (
                    ARG_MACRO_TAG_EXE,
                    exec_dep as u32,
                    idx,
                    smallvec![],
                    smallvec![lh],
                )
            }
            ArgMacroView::UserUnkeyedPlaceholder(name) => {
                let (sr, sh) = self.encode_str(AttrKind::String, name)?;
                (ARG_MACRO_TAG_UNKEYED, 0, 0, smallvec![sr], smallvec![sh.0])
            }
            ArgMacroView::UserKeyedPlaceholder { var, label, arg } => {
                let (vr, vh) = self.encode_str(AttrKind::String, var)?;
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                let mut refs: SmallVec<[AttrWord; 2]> = smallvec![vr];
                let mut parts: SmallVec<[u64; 3]> = smallvec![vh.0, lh];
                let mut flag = 0;
                if let Some(a) = arg {
                    let (ar, ah) = self.encode_str(AttrKind::String, a)?;
                    refs.push(ar);
                    parts.push(ah.0);
                    flag = 1;
                }
                (ARG_MACRO_TAG_KEYED, flag, idx, refs, parts)
            }
            ArgMacroView::Source(path) => {
                let (idx, path_hash) = self.source_path_idx_and_hash(path);
                (
                    ARG_MACRO_TAG_SOURCE,
                    0,
                    idx,
                    smallvec![],
                    smallvec![path_hash],
                )
            }
            ArgMacroView::Query(q) => {
                let (er, eh) = self.encode_query_expr_view(q.expr())?;
                let (expansion, sep) = q.expansion_parts();
                let mut refs: SmallVec<[AttrWord; 2]> = smallvec![er];
                let (sep_hash, sep_flag) = match sep {
                    Some(sp) => {
                        let (sr, sh) = self.encode_str(AttrKind::String, sp)?;
                        refs.push(sr);
                        (Some(sh), ARG_QUERY_HAS_SEP)
                    }
                    None => (None, 0),
                };
                let flags = expansion | sep_flag;
                let hash = content_hash(AttrKind::ArgMacro, |h| {
                    (ARG_MACRO_TAG_QUERY, flags, write_to_file, eh, sep_hash).hash(h)
                });
                let head = self.arg_macro_head(ARG_MACRO_TAG_QUERY, flags, wtf_bit, 0, refs.len());
                let r = self.encode_record(AttrKind::ArgMacro, hash, &head, &refs)?;
                return Ok((r, hash));
            }
        };
        let hash = content_hash(AttrKind::ArgMacro, |h| {
            (tag, flag, write_to_file, &hash_parts).hash(h)
        });
        let head = self.arg_macro_head(tag, flag, wtf_bit, payload, refs.len());
        let r = self.encode_record(AttrKind::ArgMacro, hash, &head, &refs)?;
        Ok((r, hash))
    }

    /// Encode one arg part: string parts are `ArgSimple` text records;
    /// macro parts dedup as `ArgMacro` records with labels in the label
    /// table.
    pub(super) fn encode_arg_part(
        &mut self,
        part: &StringWithMacrosPart<ProvidersLabel>,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        let (write_to_file, m) = match part {
            StringWithMacrosPart::String(s) => return self.encode_str(AttrKind::ArgSimple, s),
            StringWithMacrosPart::Macro(write_to_file, m) => (*write_to_file, m),
        };
        let wtf_bit = if write_to_file { ARG_MACRO_WTF_BIT } else { 0 };
        // Every record hashes the same shape — (tag, flag, write_to_file,
        // components) — and lays out `[header, payload, n, refs...]`; the
        // arms only extract their components.
        let (tag, flag, payload, refs, hash_parts): (
            u32,
            u32,
            u32,
            SmallVec<[AttrWord; 2]>,
            SmallVec<[u64; 3]>,
        ) = match m {
            MacroBase::Location { label, dep_kind } => {
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                (
                    ARG_MACRO_TAG_LOCATION,
                    dep_kind_bits(*dep_kind),
                    idx,
                    smallvec![],
                    smallvec![lh],
                )
            }
            MacroBase::Exe { label, exec_dep } => {
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                (
                    ARG_MACRO_TAG_EXE,
                    *exec_dep as u32,
                    idx,
                    smallvec![],
                    smallvec![lh],
                )
            }
            MacroBase::UserUnkeyedPlaceholder(name) => {
                let (sr, sh) = self.encode_str(AttrKind::String, name)?;
                (ARG_MACRO_TAG_UNKEYED, 0, 0, smallvec![sr], smallvec![sh.0])
            }
            MacroBase::UserKeyedPlaceholder(box (var, label, arg)) => {
                let (vr, vh) = self.encode_str(AttrKind::String, var)?;
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                let mut refs: SmallVec<[AttrWord; 2]> = smallvec![vr];
                let mut parts: SmallVec<[u64; 3]> = smallvec![vh.0, lh];
                let mut flag = 0;
                if let Some(a) = arg {
                    let (ar, ah) = self.encode_str(AttrKind::String, a)?;
                    refs.push(ar);
                    parts.push(ah.0);
                    flag = 1;
                }
                (ARG_MACRO_TAG_KEYED, flag, idx, refs, parts)
            }
            MacroBase::Source(path) => {
                let (idx, path_hash) = self.source_path_idx_and_hash(path);
                (
                    ARG_MACRO_TAG_SOURCE,
                    0,
                    idx,
                    smallvec![],
                    smallvec![path_hash],
                )
            }
            MacroBase::Query(q) => {
                let parsed = buck2_query_parser::parse_expr(&q.query.query)?;
                let (er, eh) =
                    self.encode_query_expr(&parsed, &q.query.query, &q.query.resolved_literals)?;
                let (expansion, sep) = match &q.expansion_type {
                    QueryExpansion::Output => (ARG_QUERY_EXPANSION_OUTPUT, None),
                    QueryExpansion::Target => (ARG_QUERY_EXPANSION_TARGET, None),
                    QueryExpansion::TargetAndOutput(sep) => {
                        (ARG_QUERY_EXPANSION_TARGET_AND_OUTPUT, sep.as_deref())
                    }
                };
                let mut refs: SmallVec<[AttrWord; 2]> = smallvec![er];
                let (sep_hash, sep_flag) = match sep {
                    Some(sp) => {
                        let (sr, sh) = self.encode_str(AttrKind::String, sp)?;
                        refs.push(sr);
                        (Some(sh), ARG_QUERY_HAS_SEP)
                    }
                    None => (None, 0),
                };
                let flags = expansion | sep_flag;
                let hash = content_hash(AttrKind::ArgMacro, |h| {
                    (ARG_MACRO_TAG_QUERY, flags, write_to_file, eh, sep_hash).hash(h)
                });
                let head = self.arg_macro_head(ARG_MACRO_TAG_QUERY, flags, wtf_bit, 0, refs.len());
                let r = self.encode_record(AttrKind::ArgMacro, hash, &head, &refs)?;
                return Ok((r, hash));
            }
        };
        let hash = content_hash(AttrKind::ArgMacro, |h| {
            (tag, flag, write_to_file, &hash_parts).hash(h)
        });
        let head = self.arg_macro_head(tag, flag, wtf_bit, payload, refs.len());
        let r = self.encode_record(AttrKind::ArgMacro, hash, &head, &refs)?;
        Ok((r, hash))
    }

    pub(super) fn arg_macro_head(
        &self,
        tag: u32,
        flag: u32,
        wtf_bit: u32,
        payload: u32,
        nrefs: usize,
    ) -> [u32; 3] {
        [
            tag | (flag << ARG_MACRO_FLAG_SHIFT) | wtf_bit,
            payload,
            nrefs as u32,
        ]
    }
}
