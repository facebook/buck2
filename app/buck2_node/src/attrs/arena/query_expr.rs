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
use smallvec::smallvec;
use strong_hash::StrongHash;

use crate::attrs::arena::builder::CoercedAttrArenaBuilder;
use crate::attrs::arena::builder::EncodeScratchFields;
use crate::attrs::arena::builder::EncodeScratchRefs;
use crate::attrs::arena::builder::content_hash;
use crate::attrs::arena::fields::IdHasher;
use crate::attrs::arena::kinds;
use crate::attrs::arena::kinds::AttrKind;
use crate::attrs::arena::schema::Kind;
use crate::attrs::arena::schema::Storage;
use crate::attrs::arena::storage::AttrToken;
use crate::attrs::arena::storage::AttrWord;
use crate::attrs::arena::storage::CoercedAttrArena;
use crate::attrs::arena::storage::CoercedAttrHash;
use crate::attrs::arena::storage::RefRun;
use crate::attrs::arena::storage::TailLen;
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
    const STORAGE: Storage = Storage::Record;
    const SCALAR: bool = false;
    type View<'a> = QueryExprRef<'a>;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        let nfields = 3u32.checked_add(peek(1)?)?;
        Some((nfields, TailLen::Refs(peek(nfields - 1)?)))
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> QueryExprRef<'a> {
        QueryExprRef { arena, r }
    }

    fn copy_into(
        parts: QueryExprRef<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken> {
        b.encode_query_expr_view(parts)
    }

    fn identity(parts: QueryExprRef<'_>, h: &mut IdHasher<'_>) {
        parts.strong_hash(&mut h.h);
    }

    fn same(a: QueryExprRef<'_>, b: QueryExprRef<'_>) -> bool {
        a.same_expr(&b)
    }
}

/// Handle to one stored query-expression node ([`AttrKind::QueryExpr`]);
/// `view` decodes it shallowly.
#[derive(Copy, Clone)]
pub struct QueryExprRef<'a> {
    pub(super) arena: &'a CoercedAttrArena,
    pub(super) r: AttrWord,
}

/// Shallow decode of a [`QueryExprRef`]. Children decode lazily via further
/// `view` calls.
pub(super) enum QueryExprKind<'a> {
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
    /// The canonical reprint of the stored expression plus its literal map,
    /// with offsets into the reprint. One walk produces both, so the map's
    /// spans always agree with the returned text.
    pub fn query(&self) -> (String, ResolvedQueryLiterals<ProvidersLabel>) {
        let mut out = String::new();
        let mut lits = BTreeMap::new();
        self.reprint(&mut out, &mut |span, l| {
            lits.insert(span, l.dupe());
        });
        (out, ResolvedQueryLiterals(lits))
    }

    /// The `attrs.query()` value this expression is, configured.
    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> QueryAttr<ConfiguredProvidersLabel> {
        let mut query = String::new();
        let mut lits = BTreeMap::new();
        self.reprint(&mut query, &mut |span, l| {
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

    pub(super) fn kind(&self) -> QueryExprKind<'a> {
        let arena = self.arena;
        let rec = arena.open(self.r);
        let subtag = rec.head.raw(0);
        let nv = rec.head.raw(1);
        let vals = rec.head.skip(2);
        match subtag {
            Q_EXPR_NONE => QueryExprKind::None,
            Q_EXPR_WORD | Q_EXPR_STRING => {
                let text = arena.str_at(rec.refs().get(0));
                if subtag == Q_EXPR_WORD {
                    QueryExprKind::Word(text)
                } else {
                    QueryExprKind::String(text)
                }
            }
            Q_EXPR_INT => QueryExprKind::Int(vals.raw(0) as u64 | ((vals.raw(1) as u64) << 32)),
            Q_EXPR_LABEL => QueryExprKind::Label(&arena.providers_labels[vals.raw(0) as usize]),
            Q_EXPR_FUNCTION => {
                let refs = rec.refs();
                QueryExprKind::Function {
                    name: arena.str_at(refs.get(0)),
                    args: QueryExprSeq { run: refs.skip(1) },
                }
            }
            Q_EXPR_BINOP => {
                let refs = rec.refs();
                QueryExprKind::BinOp {
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
                    QueryExprKind::Set(seq)
                } else {
                    QueryExprKind::FileSet(seq)
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
        match self.kind() {
            QueryExprKind::None => out.push_str("None"),
            QueryExprKind::Word(t) if buck2_query_parser::is_unquoted_word(t) => out.push_str(t),
            QueryExprKind::Word(t) | QueryExprKind::String(t) => {
                let quote = Self::quote_for(t);
                out.push(quote);
                out.push_str(t);
                out.push(quote);
            }
            QueryExprKind::Int(v) => write!(out, "{v}").expect("should write: String"),
            QueryExprKind::Label(label) => {
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
            QueryExprKind::Function { name, args } => {
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
            QueryExprKind::BinOp { first, rest } => {
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
            QueryExprKind::Set(items) => Self::reprint_set("set(", items, out, lit),
            QueryExprKind::FileSet(items) => Self::reprint_set("fileset(", items, out, lit),
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
        match self.kind() {
            QueryExprKind::Label(l) => out.push(l),
            QueryExprKind::Function { args, .. } => {
                for a in args.iter() {
                    a.labels(out);
                }
            }
            QueryExprKind::BinOp { first, rest } => {
                first.labels(out);
                for (_, ex) in rest.iter() {
                    ex.labels(out);
                }
            }
            QueryExprKind::Set(items) | QueryExprKind::FileSet(items) => {
                for v in items.iter() {
                    v.labels(out);
                }
            }
            QueryExprKind::None
            | QueryExprKind::Word(_)
            | QueryExprKind::String(_)
            | QueryExprKind::Int(_) => {}
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

impl PartialEq for QueryExprRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.same_expr(other)
    }
}

impl StrongHash for QueryExprRef<'_> {
    fn strong_hash<H: Hasher>(&self, h: &mut H) {
        match self.kind() {
            QueryExprKind::None => h.write_u32(Q_EXPR_NONE),
            QueryExprKind::Word(t) => {
                h.write_u32(Q_EXPR_WORD);
                t.strong_hash(h);
            }
            QueryExprKind::String(t) => {
                h.write_u32(Q_EXPR_STRING);
                t.strong_hash(h);
            }
            QueryExprKind::Int(v) => {
                h.write_u32(Q_EXPR_INT);
                h.write_u64(v);
            }
            QueryExprKind::Label(l) => {
                h.write_u32(Q_EXPR_LABEL);
                l.strong_hash(h);
            }
            QueryExprKind::Function { name, args } => {
                h.write_u32(Q_EXPR_FUNCTION);
                name.strong_hash(h);
                h.write_u32(args.len());
                for a in args.iter() {
                    a.strong_hash(h);
                }
            }
            QueryExprKind::BinOp { first, rest } => {
                h.write_u32(Q_EXPR_BINOP);
                first.strong_hash(h);
                h.write_u32(rest.len());
                for (op, ex) in rest.iter() {
                    h.write_u32(binary_op_bits(op));
                    ex.strong_hash(h);
                }
            }
            QueryExprKind::Set(items) => {
                h.write_u32(Q_EXPR_SET);
                h.write_u32(items.len());
                for v in items.iter() {
                    v.strong_hash(h);
                }
            }
            QueryExprKind::FileSet(items) => {
                h.write_u32(Q_EXPR_FILESET);
                h.write_u32(items.len());
                for v in items.iter() {
                    v.strong_hash(h);
                }
            }
        }
    }
}

/// One node of a query expression as its record stores it, children
/// already encoded. Both the parser's expression and a decoded node convert
/// to this, so the record layout and its content hash are defined once.
enum QueryNode<'a> {
    None,
    Word(&'a str),
    String(&'a str),
    Int(u64),
    Label(&'a ProvidersLabel),
    Function {
        name: &'a str,
        args: SmallVec<[AttrToken; 8]>,
    },
    BinOp {
        first: AttrToken,
        rest: SmallVec<[(BinaryOp, AttrToken); 8]>,
    },
    Set(SmallVec<[AttrToken; 16]>),
    FileSet(SmallVec<[AttrToken; 16]>),
}

/// The record parts of a `set(...)` or `fileset(...)` node over encoded members.
fn set_parts(
    subtag: u32,
    items: &[AttrToken],
) -> (u32, SmallVec<[u32; 8]>, EncodeScratchRefs, CoercedAttrHash) {
    let hashes: SmallVec<[CoercedAttrHash; 16]> = items.iter().map(|i| i.1).collect();
    (
        subtag,
        smallvec![],
        items.iter().map(|i| i.0).collect(),
        content_hash(AttrKind::QueryExpr, |h| (subtag, &hashes).hash(h)),
    )
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

    /// Encode a parsed query expression. Literal text (from
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
    ) -> buck2_error::Result<AttrToken> {
        let by_text: BuckMutMap<&str, &ProvidersLabel> = lits
            .0
            .iter()
            .map(|(&(off, len), label)| (&src[off..off + len], label))
            .collect();
        let mut matched: BuckMutMap<&str, ()> = BuckMutMap::default();
        let r = self.encode_parsed_node(e, &by_text, &mut matched)?;
        if matched.len() != by_text.len() {
            return Err(internal_error!(
                "query literals must all match expression nodes: matched {} of {}",
                matched.len(),
                by_text.len()
            ));
        }
        Ok(r)
    }

    fn encode_parsed_node<'s>(
        &mut self,
        e: &buck2_query_parser::SpannedExpr<'s>,
        by_text: &BuckMutMap<&'s str, &ProvidersLabel>,
        matched: &mut BuckMutMap<&'s str, ()>,
    ) -> buck2_error::Result<AttrToken> {
        // A word or string whose text names a resolved literal is that
        // literal.
        let mut literal_or =
            |text: &'s str, as_text: fn(&'s str) -> QueryNode<'s>| match by_text.get(text) {
                Some(label) => {
                    matched.insert(text, ());
                    QueryNode::Label(label)
                }
                None => as_text(text),
            };
        let node = match &e.value {
            Expr::None => QueryNode::None,
            Expr::String(w) => literal_or(w, QueryNode::String),
            Expr::Integer(v) => QueryNode::Int(*v),
            Expr::Function {
                function_name,
                args,
            } => QueryNode::Function {
                name: function_name.fragment(),
                args: args
                    .iter()
                    .map(|a| self.encode_parsed_node(a, by_text, matched))
                    .collect::<buck2_error::Result<_>>()?,
            },
            Expr::BinaryOpSequence(first, rest) => QueryNode::BinOp {
                first: self.encode_parsed_node(first, by_text, matched)?,
                rest: rest
                    .iter()
                    .map(|(op, ex)| Ok((*op, self.encode_parsed_node(ex, by_text, matched)?)))
                    .collect::<buck2_error::Result<_>>()?,
            },
            Expr::Set(vals) | Expr::FileSet(vals) => {
                let items = vals
                    .iter()
                    .map(|v| self.encode_query_node(literal_or(v.fragment(), QueryNode::Word)))
                    .collect::<buck2_error::Result<_>>()?;
                match &e.value {
                    Expr::Set(_) => QueryNode::Set(items),
                    _ => QueryNode::FileSet(items),
                }
            }
        };
        self.encode_query_node(node)
    }

    /// [`Self::encode_query_expr`] from a decoded expression: node for node
    /// and hash for hash.
    pub(super) fn encode_query_expr_view(
        &mut self,
        e: QueryExprRef<'_>,
    ) -> buck2_error::Result<AttrToken> {
        let node = match e.kind() {
            QueryExprKind::None => QueryNode::None,
            QueryExprKind::Word(t) => QueryNode::Word(t),
            QueryExprKind::String(t) => QueryNode::String(t),
            QueryExprKind::Int(v) => QueryNode::Int(v),
            QueryExprKind::Label(l) => QueryNode::Label(l),
            QueryExprKind::Function { name, args } => QueryNode::Function {
                name,
                args: args
                    .iter()
                    .map(|a| self.encode_query_expr_view(a))
                    .collect::<buck2_error::Result<_>>()?,
            },
            QueryExprKind::BinOp { first, rest } => QueryNode::BinOp {
                first: self.encode_query_expr_view(first)?,
                rest: rest
                    .iter()
                    .map(|(op, ex)| Ok((op, self.encode_query_expr_view(ex)?)))
                    .collect::<buck2_error::Result<_>>()?,
            },
            QueryExprKind::Set(items) => QueryNode::Set(
                items
                    .iter()
                    .map(|v| self.encode_query_expr_view(v))
                    .collect::<buck2_error::Result<_>>()?,
            ),
            QueryExprKind::FileSet(items) => QueryNode::FileSet(
                items
                    .iter()
                    .map(|v| self.encode_query_expr_view(v))
                    .collect::<buck2_error::Result<_>>()?,
            ),
        };
        self.encode_query_node(node)
    }

    /// One `QueryExpr` record: `[subtag, nv, values..., n]` then refs; the
    /// content hash folds the subtag and the node's content.
    fn encode_query_node(&mut self, node: QueryNode<'_>) -> buck2_error::Result<AttrToken> {
        let (subtag, values, refs, hash): (
            u32,
            SmallVec<[u32; 8]>,
            EncodeScratchRefs,
            CoercedAttrHash,
        ) = match node {
            QueryNode::None => (
                Q_EXPR_NONE,
                smallvec![],
                EncodeScratchRefs::new(),
                content_hash(AttrKind::QueryExpr, |h| Q_EXPR_NONE.hash(h)),
            ),
            QueryNode::Word(t) | QueryNode::String(t) => {
                let subtag = match node {
                    QueryNode::Word(_) => Q_EXPR_WORD,
                    _ => Q_EXPR_STRING,
                };
                let AttrToken(sr, sh) = self.encode_str(AttrKind::String, t)?;
                (
                    subtag,
                    smallvec![],
                    smallvec![sr],
                    content_hash(AttrKind::QueryExpr, |h| (subtag, sh).hash(h)),
                )
            }
            QueryNode::Int(v) => (
                Q_EXPR_INT,
                smallvec![v as u32, (v >> 32) as u32],
                EncodeScratchRefs::new(),
                content_hash(AttrKind::QueryExpr, |h| (Q_EXPR_INT, v).hash(h)),
            ),
            QueryNode::Label(label) => {
                let (idx, lh) = self.providers_label_idx_and_hash(label);
                (
                    Q_EXPR_LABEL,
                    smallvec![idx],
                    EncodeScratchRefs::new(),
                    content_hash(AttrKind::QueryExpr, |h| (Q_EXPR_LABEL, lh).hash(h)),
                )
            }
            QueryNode::Function { name, args } => {
                let AttrToken(nr, nh) = self.encode_str(AttrKind::String, name)?;
                let hashes: SmallVec<[CoercedAttrHash; 8]> = args.iter().map(|a| a.1).collect();
                let refs = std::iter::once(nr)
                    .chain(args.iter().map(|a| a.0))
                    .collect();
                (
                    Q_EXPR_FUNCTION,
                    smallvec![],
                    refs,
                    content_hash(AttrKind::QueryExpr, |h| {
                        (Q_EXPR_FUNCTION, nh, &hashes).hash(h)
                    }),
                )
            }
            QueryNode::BinOp { first, rest } => {
                let ops: SmallVec<[u32; 8]> =
                    rest.iter().map(|(op, _)| binary_op_bits(*op)).collect();
                let hashes: SmallVec<[CoercedAttrHash; 8]> = rest
                    .iter()
                    .zip(&ops)
                    .map(|((_, x), &opw)| CoercedAttrHash(x.1.0 ^ ((opw as u64) << 60)))
                    .collect();
                let refs = std::iter::once(first.0)
                    .chain(rest.iter().map(|(_, x)| x.0))
                    .collect();
                (
                    Q_EXPR_BINOP,
                    ops,
                    refs,
                    content_hash(AttrKind::QueryExpr, |h| {
                        (Q_EXPR_BINOP, first.1, &hashes).hash(h)
                    }),
                )
            }
            QueryNode::Set(items) => set_parts(Q_EXPR_SET, &items),
            QueryNode::FileSet(items) => set_parts(Q_EXPR_FILESET, &items),
        };
        let mut head = EncodeScratchFields::with_capacity(3 + values.len());
        head.push(subtag);
        head.push(values.len() as u32);
        head.extend_from_slice(&values);
        head.push(refs.len() as u32);
        let r = self.encode_record(AttrKind::QueryExpr, hash, &head, &refs)?;
        Ok(AttrToken(r, hash))
    }
}
