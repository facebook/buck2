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
use smallvec::SmallVec;
use smallvec::smallvec;
use strong_hash::StrongHash;

use crate::attrs::arena::builder::CoercedAttrArenaBuilder;
use crate::attrs::arena::builder::content_hash;
use crate::attrs::arena::fields::IdHasher;
use crate::attrs::arena::schema::Kind;
use crate::attrs::arena::schema::Storage;
use crate::attrs::arena::storage::AttrToken;
use crate::attrs::arena::storage::AttrWord;
use crate::attrs::arena::storage::CoercedAttrArena;
use crate::attrs::arena::storage::RefRun;
use crate::attrs::arena::storage::TailLen;
use crate::attrs::arena::view::ArgParts;
use crate::attrs::arena::view::Refs;
use crate::attrs::attr_type::arg::MacroBase;
use crate::attrs::attr_type::arg::MacroDepKind;
use crate::attrs::attr_type::arg::QueryExpansion;
use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::arg::StringWithMacrosPart;
use crate::attrs::attr_type::query::QueryAttrBase;
use crate::attrs::attr_type::query::QueryExprRef;
use crate::attrs::attr_type::query::QueryMacroBase;
use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::coerced::kinds;
use crate::attrs::coerced::kinds::CoercedAttrKind;
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
            CoercedAttrKind::ArgSimple => ArgPartView::String(self.str_at(r)),
            CoercedAttrKind::ArgMacro => {
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
    const STORAGE: Storage = Storage::Record;
    const SCALAR: bool = false;
    type View<'a> = ArgPartView<'a>;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        Some((3, TailLen::Refs(peek(2)?)))
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> ArgPartView<'a> {
        arena.arg_part_at(r)
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
    pub(super) fn encode_arg_part(
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
    pub(super) fn encode_arg_part_view(
        &mut self,
        part: ArgPartView<'_>,
    ) -> buck2_error::Result<AttrToken> {
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
