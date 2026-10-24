/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Query expressions stored in parsed form, for `attrs.query()` values and
//! query macros.

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::hash::Hash;
use std::hash::Hasher;

use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_error::internal_error;
use buck2_hash::BuckMutMap;
use buck2_query_parser::BinaryOp;
use buck2_query_parser::Expr;
use dupe::Dupe;
use smallvec::SmallVec;
use strong_hash::StrongHash;

use crate::attrs::arena::builder::CoercedAttrArenaBuilder;
use crate::attrs::arena::builder::EncodeScratchFields;
use crate::attrs::arena::builder::EncodeScratchRefs;
use crate::attrs::arena::builder::content_hash;
use crate::attrs::arena::builder::token;
use crate::attrs::arena::fields::IdHasher;
use crate::attrs::arena::schema::AttrKind;
use crate::attrs::arena::schema::Kind;
use crate::attrs::arena::schema::kinds;
use crate::attrs::arena::storage::AttrToken;
use crate::attrs::arena::storage::AttrWord;
use crate::attrs::arena::storage::CoercedAttrArena;
use crate::attrs::arena::storage::CoercedAttrHash;
use crate::attrs::arena::storage::RefRun;
use crate::attrs::arena::storage::TailLen;
use crate::attrs::arena::view::CoercedAttrRef;
use crate::attrs::arena::view::CoercedAttrRefKind;
use crate::attrs::attr_type::query::QueryAttr;
use crate::attrs::attr_type::query::QueryAttrBase;
use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::provider_id_set::ProviderIdSet;

// Query-expression node subtags (first head field of a `QueryExpr` record).
pub(super) const Q_EXPR_NONE: u32 = 0;
pub(super) const Q_EXPR_WORD: u32 = 1;
pub(super) const Q_EXPR_STRING: u32 = 2;
pub(super) const Q_EXPR_INT: u32 = 3;
pub(super) const Q_EXPR_LABEL: u32 = 4;
pub(super) const Q_EXPR_FUNCTION: u32 = 5;
pub(super) const Q_EXPR_BINOP: u32 = 6;
pub(super) const Q_EXPR_SET: u32 = 7;
pub(super) const Q_EXPR_FILESET: u32 = 8;

pub(super) fn binary_op_bits(op: BinaryOp) -> u32 {
    match op {
        BinaryOp::Intersect => 0,
        BinaryOp::Except => 1,
        BinaryOp::Union => 2,
    }
}

pub(super) fn binary_op_from_bits(bits: u32) -> BinaryOp {
    match bits {
        0 => BinaryOp::Intersect,
        1 => BinaryOp::Except,
        2 => BinaryOp::Union,
        _ => unreachable!("should be valid: records are only written by the encoder"),
    }
}

impl Kind for kinds::QueryExpr {
    const KIND: AttrKind = AttrKind::QueryExpr;
    const SCALAR: bool = false;
    type Parts<'a> = QueryExprRef<'a>;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        let nfields = 3u32.checked_add(peek(1)?)?;
        Some((nfields, TailLen::Refs(peek(nfields - 1)?)))
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> QueryExprRef<'a> {
        QueryExprRef { arena, r }
    }

    fn wrap<'a>(_v: CoercedAttrRef<'a>, _parts: QueryExprRef<'a>) -> CoercedAttrRefKind<'a>
    where
        Self: 'a,
    {
        unreachable!("query expression nodes are not attribute values")
    }

    fn copy_into(
        parts: QueryExprRef<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken> {
        b.encode_query_expr_view(parts).map(token)
    }

    fn identity(parts: QueryExprRef<'_>, h: &mut IdHasher<'_>) {
        parts.strong_hash(&mut h.h);
    }

    fn same(a: QueryExprRef<'_>, b: QueryExprRef<'_>) -> bool {
        a.same_expr(&b)
    }
}

/// Decoded view of an `attrs.query()` value record.
#[derive(Copy, Clone)]
pub struct QueryView<'a> {
    pub(super) arena: &'a CoercedAttrArena,
    pub(super) r: AttrWord,
}

impl<'a> QueryView<'a> {
    pub(super) fn expr(&self) -> QueryExprRef<'a> {
        QueryExprRef {
            arena: self.arena,
            r: self.arena.open(self.r).refs().get(0),
        }
    }

    /// The canonical reprint of the stored expression plus its literal map,
    /// with offsets into the reprint.
    pub fn query(&self) -> (String, ResolvedQueryLiterals<ProvidersLabel>) {
        let mut out = String::new();
        let mut lits = BTreeMap::new();
        self.expr().reprint(&mut out, &mut |span, l| {
            lits.insert(span, l.dupe());
        });
        (out, ResolvedQueryLiterals(lits))
    }

    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> QueryAttr<ConfiguredProvidersLabel> {
        let mut query = String::new();
        let mut lits = BTreeMap::new();
        self.expr().reprint(&mut query, &mut |span, l| {
            lits.insert(span, ctx.configure_target(l));
        });
        QueryAttr {
            providers: ProviderIdSet::EMPTY,
            query: QueryAttrBase {
                query,
                resolved_literals: ResolvedQueryLiterals(lits),
            },
        }
    }
}

impl PartialEq for QueryView<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.query() == other.query()
    }
}

/// Handle to one stored query-expression node ([`AttrKind::QueryExpr`]);
/// `view` decodes it shallowly.
#[derive(Copy, Clone)]
pub(crate) struct QueryExprRef<'a> {
    pub(super) arena: &'a CoercedAttrArena,
    pub(super) r: AttrWord,
}

/// Shallow decode of a [`QueryExprRef`]. Children decode lazily via further
/// `view` calls.
pub(super) enum QueryExprView<'a> {
    None,
    /// Bare word (set members, unresolved tokens).
    Word(&'a str),
    /// Quoted string.
    String(&'a str),
    Int(u64),
    /// A target literal, resolved at coercion.
    Label(&'a ProvidersLabel),
    Function {
        name: &'a str,
        args: QueryExprSeq<'a>,
    },
    BinOp {
        first: QueryExprRef<'a>,
        rest: QueryExprOpSeq<'a>,
    },
    Set(QueryExprSeq<'a>),
    FileSet(QueryExprSeq<'a>),
}

/// View of a run of query-expression child refs.
#[derive(Copy, Clone)]
pub(super) struct QueryExprSeq<'a> {
    pub(super) run: RefRun<'a>,
}

impl<'a> QueryExprSeq<'a> {
    pub(super) fn len(&self) -> u32 {
        self.run.len()
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = QueryExprRef<'a>> + 'a {
        let arena = self.run.arena;
        self.run.iter().map(move |r| QueryExprRef { arena, r })
    }
}

/// View of a binop node's tail: `n` operators in the head, paired with `n`
/// operand refs.
#[derive(Copy, Clone)]
pub(super) struct QueryExprOpSeq<'a> {
    pub(super) ops: RefRun<'a>,
    pub(super) run: RefRun<'a>,
}

impl<'a> QueryExprOpSeq<'a> {
    pub(super) fn len(&self) -> u32 {
        self.run.len()
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = (BinaryOp, QueryExprRef<'a>)> + 'a {
        let arena = self.run.arena;
        self.ops
            .iter_raw()
            .zip(self.run.iter())
            .map(move |(op, r)| (binary_op_from_bits(op), QueryExprRef { arena, r }))
    }
}

impl<'a> QueryExprRef<'a> {
    pub(super) fn view(&self) -> QueryExprView<'a> {
        let arena = self.arena;
        let rec = arena.open(self.r);
        let subtag = rec.head.raw(0);
        let nv = rec.head.raw(1);
        let vals = rec.head.skip(2);
        match subtag {
            Q_EXPR_NONE => QueryExprView::None,
            Q_EXPR_WORD | Q_EXPR_STRING => {
                let text = arena.str_at(rec.refs().get(0));
                if subtag == Q_EXPR_WORD {
                    QueryExprView::Word(text)
                } else {
                    QueryExprView::String(text)
                }
            }
            Q_EXPR_INT => QueryExprView::Int(vals.raw(0) as u64 | ((vals.raw(1) as u64) << 32)),
            Q_EXPR_LABEL => QueryExprView::Label(&arena.providers_labels[vals.raw(0) as usize]),
            Q_EXPR_FUNCTION => {
                let refs = rec.refs();
                QueryExprView::Function {
                    name: arena.str_at(refs.get(0)),
                    args: QueryExprSeq { run: refs.skip(1) },
                }
            }
            Q_EXPR_BINOP => {
                let refs = rec.refs();
                QueryExprView::BinOp {
                    first: QueryExprRef {
                        arena,
                        r: refs.get(0),
                    },
                    rest: QueryExprOpSeq {
                        ops: RefRun { len: nv, ..vals },
                        run: refs.skip(1),
                    },
                }
            }
            Q_EXPR_SET | Q_EXPR_FILESET => {
                let seq = QueryExprSeq { run: rec.refs() };
                if subtag == Q_EXPR_SET {
                    QueryExprView::Set(seq)
                } else {
                    QueryExprView::FileSet(seq)
                }
            }
            _ => unreachable!("should be valid: query nodes are only written by the encoder"),
        }
    }

    /// Reprints the node in the parser `Display` form
    /// (`buck2_query_parser::Expr`), handing each literal label and its span
    /// in the reprint to `lit`. Reparsing the reprint yields the same
    /// expression.
    pub(super) fn reprint(
        &self,
        out: &mut String,
        lit: &mut dyn FnMut((usize, usize), &'a ProvidersLabel),
    ) {
        match self.view() {
            QueryExprView::None => out.push_str("None"),
            QueryExprView::Word(t) if buck2_query_parser::is_unquoted_word(t) => out.push_str(t),
            QueryExprView::Word(t) | QueryExprView::String(t) => {
                let quote = Self::quote_for(t);
                out.push(quote);
                out.push_str(t);
                out.push(quote);
            }
            QueryExprView::Int(v) => write!(out, "{v}").expect("should write: String"),
            QueryExprView::Label(label) => {
                let mut start = out.len();
                write!(out, "{label}").expect("should write: String");
                // Target names and subtarget brackets reach outside the
                // unquoted-word grammar.
                if !buck2_query_parser::is_unquoted_word(&out[start..]) {
                    let quote = Self::quote_for(&out[start..]);
                    out.insert(start, quote);
                    out.push(quote);
                    start += 1;
                    lit((start, out.len() - 1 - start), label);
                } else {
                    lit((start, out.len() - start), label);
                }
            }
            QueryExprView::Function { name, args } => {
                out.push_str(name);
                out.push('(');
                for (i, a) in args.iter().enumerate() {
                    if i != 0 {
                        out.push_str(", ");
                    }
                    a.reprint(out, lit);
                }
                out.push(')');
            }
            QueryExprView::BinOp { first, rest } => {
                for _ in 0..rest.len() {
                    out.push('(');
                }
                out.push(' ');
                first.reprint(out, lit);
                for (op, ex) in rest.iter() {
                    out.push(' ');
                    out.push_str(match op {
                        BinaryOp::Intersect => "^",
                        BinaryOp::Except => "-",
                        BinaryOp::Union => "+",
                    });
                    out.push(' ');
                    ex.reprint(out, lit);
                    out.push(')');
                }
            }
            QueryExprView::Set(items) => Self::reprint_set("set(", items, out, lit),
            QueryExprView::FileSet(items) => Self::reprint_set("fileset(", items, out, lit),
        }
    }

    /// The query grammar has no escapes, so quoted text holds at most one
    /// quote kind.
    fn quote_for(t: &str) -> char {
        if t.contains('\'') { '"' } else { '\'' }
    }

    pub(super) fn reprint_set(
        prefix: &str,
        items: QueryExprSeq<'a>,
        out: &mut String,
        lit: &mut dyn FnMut((usize, usize), &'a ProvidersLabel),
    ) {
        out.push_str(prefix);
        for (i, v) in items.iter().enumerate() {
            if i != 0 {
                out.push(' ');
            }
            v.reprint(out, lit);
        }
        out.push(')');
    }

    /// Collects the node's literal labels, in expression order.
    pub(crate) fn labels(&self, out: &mut Vec<&'a ProvidersLabel>) {
        match self.view() {
            QueryExprView::Label(l) => out.push(l),
            QueryExprView::Function { args, .. } => {
                for a in args.iter() {
                    a.labels(out);
                }
            }
            QueryExprView::BinOp { first, rest } => {
                first.labels(out);
                for (_, ex) in rest.iter() {
                    ex.labels(out);
                }
            }
            QueryExprView::Set(items) | QueryExprView::FileSet(items) => {
                for v in items.iter() {
                    v.labels(out);
                }
            }
            QueryExprView::None
            | QueryExprView::Word(_)
            | QueryExprView::String(_)
            | QueryExprView::Int(_) => {}
        }
    }
}

/// Subtag plus content, children in order.
impl QueryExprRef<'_> {
    /// Equal expressions reprint identically with identical literal maps.
    pub(super) fn same_expr(&self, other: &QueryExprRef<'_>) -> bool {
        fn reprinted(e: &QueryExprRef<'_>) -> (String, BTreeMap<(usize, usize), ProvidersLabel>) {
            let mut out = String::new();
            let mut lits = BTreeMap::new();
            e.reprint(&mut out, &mut |span, l| {
                lits.insert(span, l.dupe());
            });
            (out, lits)
        }
        reprinted(self) == reprinted(other)
    }
}

impl StrongHash for QueryExprRef<'_> {
    fn strong_hash<H: Hasher>(&self, h: &mut H) {
        match self.view() {
            QueryExprView::None => h.write_u32(Q_EXPR_NONE),
            QueryExprView::Word(t) => {
                h.write_u32(Q_EXPR_WORD);
                t.strong_hash(h);
            }
            QueryExprView::String(t) => {
                h.write_u32(Q_EXPR_STRING);
                t.strong_hash(h);
            }
            QueryExprView::Int(v) => {
                h.write_u32(Q_EXPR_INT);
                h.write_u64(v);
            }
            QueryExprView::Label(l) => {
                h.write_u32(Q_EXPR_LABEL);
                l.strong_hash(h);
            }
            QueryExprView::Function { name, args } => {
                h.write_u32(Q_EXPR_FUNCTION);
                name.strong_hash(h);
                h.write_u32(args.len());
                for a in args.iter() {
                    a.strong_hash(h);
                }
            }
            QueryExprView::BinOp { first, rest } => {
                h.write_u32(Q_EXPR_BINOP);
                first.strong_hash(h);
                h.write_u32(rest.len());
                for (op, ex) in rest.iter() {
                    h.write_u32(binary_op_bits(op));
                    ex.strong_hash(h);
                }
            }
            QueryExprView::Set(items) => {
                h.write_u32(Q_EXPR_SET);
                h.write_u32(items.len());
                for v in items.iter() {
                    v.strong_hash(h);
                }
            }
            QueryExprView::FileSet(items) => {
                h.write_u32(Q_EXPR_FILESET);
                h.write_u32(items.len());
                for v in items.iter() {
                    v.strong_hash(h);
                }
            }
        }
    }
}

impl CoercedAttrArenaBuilder {
    /// Encode an `attrs.query()` value as its parsed expression.
    pub fn encode_query(
        &mut self,
        q: &QueryAttrBase<ProvidersLabel>,
    ) -> buck2_error::Result<AttrToken> {
        let parsed = buck2_query_parser::parse_expr(&q.query)?;
        let expr = self.encode_query_expr(&parsed, &q.query, &q.resolved_literals)?;
        self.encode_parsed_query(expr)
    }

    /// [`Self::encode_query_expr_inner`] from a decoded expression: node for
    /// node and hash for hash.
    pub(super) fn encode_query_expr_view(
        &mut self,
        e: QueryExprRef<'_>,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        match e.view() {
            QueryExprView::None => {
                let hash = content_hash(AttrKind::QueryExpr, |h| Q_EXPR_NONE.hash(h));
                self.encode_query_node(hash, Q_EXPR_NONE, &[], &[])
            }
            QueryExprView::Word(t) | QueryExprView::String(t) => {
                let subtag = if matches!(e.view(), QueryExprView::Word(_)) {
                    Q_EXPR_WORD
                } else {
                    Q_EXPR_STRING
                };
                let (sr, sh) = self.encode_str(AttrKind::String, t)?;
                let hash = content_hash(AttrKind::QueryExpr, |h| (subtag, sh).hash(h));
                self.encode_query_node(hash, subtag, &[], &[sr])
            }
            QueryExprView::Int(v) => {
                let hash = content_hash(AttrKind::QueryExpr, |h| (Q_EXPR_INT, v).hash(h));
                self.encode_query_node(hash, Q_EXPR_INT, &[v as u32, (v >> 32) as u32], &[])
            }
            QueryExprView::Label(label) => {
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                let hash = content_hash(AttrKind::QueryExpr, |h| (Q_EXPR_LABEL, lh).hash(h));
                self.encode_query_node(hash, Q_EXPR_LABEL, &[idx], &[])
            }
            QueryExprView::Function { name, args } => {
                let (nr, nh) = self.encode_str(AttrKind::String, name)?;
                let mut refs = EncodeScratchRefs::with_capacity(1 + args.len() as usize);
                refs.push(nr);
                let mut hashes =
                    SmallVec::<[CoercedAttrHash; 8]>::with_capacity(args.len() as usize);
                for a in args.iter() {
                    let (ar, ah) = self.encode_query_expr_view(a)?;
                    refs.push(ar);
                    hashes.push(ah);
                }
                let hash = content_hash(AttrKind::QueryExpr, |h| {
                    (Q_EXPR_FUNCTION, nh, &hashes).hash(h)
                });
                self.encode_query_node(hash, Q_EXPR_FUNCTION, &[], &refs)
            }
            QueryExprView::BinOp { first, rest } => {
                let (fr, fh) = self.encode_query_expr_view(first)?;
                let mut refs = EncodeScratchRefs::with_capacity(1 + rest.len() as usize);
                refs.push(fr);
                let mut ops = SmallVec::<[u32; 8]>::with_capacity(rest.len() as usize);
                let mut hashes =
                    SmallVec::<[CoercedAttrHash; 8]>::with_capacity(rest.len() as usize);
                for (op, ex) in rest.iter() {
                    let opw = binary_op_bits(op);
                    let (xr, xh) = self.encode_query_expr_view(ex)?;
                    ops.push(opw);
                    refs.push(xr);
                    hashes.push(CoercedAttrHash(xh.0 ^ ((opw as u64) << 60)));
                }
                let hash =
                    content_hash(AttrKind::QueryExpr, |h| (Q_EXPR_BINOP, fh, &hashes).hash(h));
                self.encode_query_node(hash, Q_EXPR_BINOP, &ops, &refs)
            }
            QueryExprView::Set(items) | QueryExprView::FileSet(items) => {
                let subtag = if matches!(e.view(), QueryExprView::Set(_)) {
                    Q_EXPR_SET
                } else {
                    Q_EXPR_FILESET
                };
                let mut refs = EncodeScratchRefs::with_capacity(items.len() as usize);
                let mut hashes =
                    SmallVec::<[CoercedAttrHash; 16]>::with_capacity(items.len() as usize);
                for v in items.iter() {
                    let (mr, mh) = self.encode_query_expr_view(v)?;
                    refs.push(mr);
                    hashes.push(mh);
                }
                let hash = content_hash(AttrKind::QueryExpr, |h| (subtag, &hashes).hash(h));
                self.encode_query_node(hash, subtag, &[], &refs)
            }
        }
    }

    /// Encode one parsed query expression node. Literal text (from
    /// `resolved_literals`) becomes providers-label refs; every other text
    /// becomes a shared text record.
    ///
    /// Coercion records one literal per distinct text, keyed by the span of
    /// its first occurrence, and evaluation resolves literals by text; so a
    /// token is a literal whenever its text matches a recorded one, repeated
    /// occurrences included.
    pub(super) fn encode_query_expr(
        &mut self,
        e: &buck2_query_parser::SpannedExpr<'_>,
        src: &str,
        lits: &ResolvedQueryLiterals<ProvidersLabel>,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        let by_text: BuckMutMap<&str, &ProvidersLabel> = lits
            .0
            .iter()
            .map(|(&(off, len), label)| (&src[off..off + len], label))
            .collect();
        let mut matched: BuckMutMap<&str, ()> = BuckMutMap::default();
        let r = self.encode_query_expr_inner(e, &by_text, &mut matched)?;
        if matched.len() != by_text.len() {
            return Err(internal_error!(
                "query literals must all match expression nodes: matched {} of {}",
                matched.len(),
                by_text.len()
            ));
        }
        Ok(r)
    }

    /// One `QueryExpr` record: `[subtag, nv, values..., n]` then refs.
    pub(super) fn encode_query_node(
        &mut self,
        hash: CoercedAttrHash,
        subtag: u32,
        values: &[u32],
        refs: &[AttrWord],
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        let mut head = EncodeScratchFields::with_capacity(3 + values.len());
        head.push(subtag);
        head.push(values.len() as u32);
        head.extend_from_slice(values);
        head.push(refs.len() as u32);
        let r = self.encode_record(AttrKind::QueryExpr, hash, &head, refs)?;
        Ok((r, hash))
    }

    pub(super) fn encode_query_word<'s>(
        &mut self,
        subtag: u32,
        text: &'s str,
        by_text: &BuckMutMap<&'s str, &ProvidersLabel>,
        matched: &mut BuckMutMap<&'s str, ()>,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        if let Some(label) = by_text.get(text) {
            matched.insert(text, ());
            let (idx, lh) = self.providers_label_idx_and_hash(label);
            let hash = content_hash(AttrKind::QueryExpr, |h| (Q_EXPR_LABEL, lh).hash(h));
            return self.encode_query_node(hash, Q_EXPR_LABEL, &[idx], &[]);
        }
        let (sr, sh) = self.encode_str(AttrKind::String, text)?;
        let hash = content_hash(AttrKind::QueryExpr, |h| (subtag, sh).hash(h));
        self.encode_query_node(hash, subtag, &[], &[sr])
    }

    pub(super) fn encode_query_expr_inner<'s>(
        &mut self,
        e: &buck2_query_parser::SpannedExpr<'s>,
        by_text: &BuckMutMap<&'s str, &ProvidersLabel>,
        matched: &mut BuckMutMap<&'s str, ()>,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        match &e.value {
            Expr::None => {
                let hash = content_hash(AttrKind::QueryExpr, |h| Q_EXPR_NONE.hash(h));
                self.encode_query_node(hash, Q_EXPR_NONE, &[], &[])
            }
            Expr::String(w) => self.encode_query_word(Q_EXPR_STRING, w, by_text, matched),
            Expr::Integer(v) => {
                let hash = content_hash(AttrKind::QueryExpr, |h| (Q_EXPR_INT, *v).hash(h));
                self.encode_query_node(hash, Q_EXPR_INT, &[*v as u32, (*v >> 32) as u32], &[])
            }
            Expr::Function {
                function_name,
                args,
            } => {
                let (nr, nh) = self.encode_str(AttrKind::String, function_name.fragment())?;
                let mut refs = EncodeScratchRefs::with_capacity(1 + args.len());
                refs.push(nr);
                let mut hashes = SmallVec::<[CoercedAttrHash; 8]>::with_capacity(args.len());
                for a in args {
                    let (ar, ah) = self.encode_query_expr_inner(a, by_text, matched)?;
                    refs.push(ar);
                    hashes.push(ah);
                }
                let hash = content_hash(AttrKind::QueryExpr, |h| {
                    (Q_EXPR_FUNCTION, nh, &hashes).hash(h)
                });
                self.encode_query_node(hash, Q_EXPR_FUNCTION, &[], &refs)
            }
            Expr::BinaryOpSequence(first, rest) => {
                let (fr, fh) = self.encode_query_expr_inner(first, by_text, matched)?;
                let mut refs = EncodeScratchRefs::with_capacity(1 + rest.len());
                refs.push(fr);
                let mut ops = SmallVec::<[u32; 8]>::with_capacity(rest.len());
                let mut hashes = SmallVec::<[CoercedAttrHash; 8]>::with_capacity(rest.len());
                for (op, ex) in rest {
                    let opw = binary_op_bits(*op);
                    let (xr, xh) = self.encode_query_expr_inner(ex, by_text, matched)?;
                    ops.push(opw);
                    refs.push(xr);
                    hashes.push(CoercedAttrHash(xh.0 ^ ((opw as u64) << 60)));
                }
                let hash =
                    content_hash(AttrKind::QueryExpr, |h| (Q_EXPR_BINOP, fh, &hashes).hash(h));
                self.encode_query_node(hash, Q_EXPR_BINOP, &ops, &refs)
            }
            Expr::Set(vals) | Expr::FileSet(vals) => {
                let subtag = if matches!(&e.value, Expr::Set(_)) {
                    Q_EXPR_SET
                } else {
                    Q_EXPR_FILESET
                };
                let mut refs = EncodeScratchRefs::with_capacity(vals.len());
                let mut hashes = SmallVec::<[CoercedAttrHash; 16]>::with_capacity(vals.len());
                for v in vals {
                    let (mr, mh) =
                        self.encode_query_word(Q_EXPR_WORD, v.fragment(), by_text, matched)?;
                    refs.push(mr);
                    hashes.push(mh);
                }
                let hash = content_hash(AttrKind::QueryExpr, |h| (subtag, &hashes).hash(h));
                self.encode_query_node(hash, subtag, &[], &refs)
            }
        }
    }
}
