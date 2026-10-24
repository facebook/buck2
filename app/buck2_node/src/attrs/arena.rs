/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Arena storage for coerced attributes.
//!
//! An attribute value is a tagged record in a per-package byte buffer; an
//! [`AttrWord`] is the byte offset of the record's tag.
//!
//! ```text
//! AttrWord: [ 1 | kind:5 | payload:26 ]      inline leaf: None, bools,
//!                                            small ints, table indices
//!           [ 0 | record offset:31 ]         everything else
//!
//! record:   tag:u8 = [ width:2 | kind:6 ]   head fields...   tail
//!
//!   head    the record's immediate content (counts, table indices, split
//!           ints) as `width + 1`-byte little-endian fields; how many, and
//!           what follows, is the kind's layout (`AttrKind::layout`);
//!   tail    text bytes (length in the first head field), or child refs,
//!           `width + 1` bytes each: bit 0 set = an inline leaf, kind in the
//!           next five bits and payload above; bit 0 clear = a record ref,
//!           the backward delta from the record's own offset above; a zero
//!           field marks an absent value. The width is chosen per record
//!           from its largest head field or ref.
//! ```
//!
//! A value's kind costs at most one byte read: inline leaves carry it in
//! the word, records in their tag, so a traversal over deps opens only
//! containers.
//! Each kind's representation is documented on its `AttrKind` variant.
//! Worked example — `{"srcs": ["a.c"]}`, a dict of one pair whose value is
//! a one-element list of a source file:
//!
//! ```text
//! bytes:  [String  4 "srcs"]   [List  1  SourceFile#0]   [Dict  1  ->"srcs" ->list]
//!          ^ S                  ^ L                      ^ D = the value's own ref
//!
//! the list's one ref is the inline source leaf (path table index 0); the
//! dict's refs are deltas D - S and D - L, all one byte wide here.
//! ```
//!
//! One [`CoercedAttrArena`] holds every attribute value coerced for one package:
//! the byte buffer plus typed side tables — shares of globally interned
//! labels and of the package listing's paths, and a `Legacy` table of plain
//! [`CoercedAttr`]s for variants too rare in real graphs to warrant a
//! native encoding. An arena is *sealed* when its package finishes
//! evaluating: writes end, reads begin (distinct from starlark *freezing*,
//! which happens earlier, to the values attrs are coerced from). Three
//! properties carry the design:
//!
//! - offsets are indices, not addresses: the buffer can grow while the
//!   package coerces, and the sealed form is position-independent — one
//!   pagable blob per package instead of one allocation per value;
//! - an `AttrWord` is a shared handle: value content is stored once (the
//!   builder dedups on encode) and referenced from any number of parent
//!   records; refs within a record are fixed-stride, so they stay
//!   indexable;
//! - refs only ever point at already-encoded values (children are encoded
//!   before their parents), so a ref cannot be observed half-built and
//!   deltas are never negative.

use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;
use std::sync::OnceLock;

use allocative::Allocative;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::package::PackageLabel;
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::soft_error;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_hash::BuckHasher;
use buck2_hash::BuckMutMap;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use buck2_util::pointer_identity::StaticPointerIdentity;
use buck2_util::strong_hasher::Blake3StrongHasher;
use buck2_util::strong_hasher::StrongHash128;
use display_container::fmt_container;
use display_container::fmt_keyed_container;
use dupe::Dupe;
use either::Either;
use itertools::Itertools;
use pagable::Pagable;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use ref_cast::RefCast;
use serde_json::to_value;
use smallvec::SmallVec;
use strong_hash::StrongHash;
use strum::FromRepr;

use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::attr_type::arg::ConfiguredStringWithMacros;
use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::attr_config::source_file_display;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::label::LabelAttrType;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::coerced_attr::CoercedConcat;
use crate::attrs::coerced_attr::CoercedSelector;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::configuration::resolved::ConfigurationSettingKey;

/// Tag byte of a record: `[ width:2 | kind:6 ]`. `width` is the byte width
/// of every head field and ref in the record, minus one.
const TAG_KIND_MASK: u8 = 0x3F;
const TAG_WIDTH_SHIFT: u8 = 6;

/// Record kinds. Each variant documents its head and refs; a kind with
/// neither is a tag-only record.
#[derive(Copy, Clone, PartialEq, Eq, Debug, FromRepr)]
#[repr(u8)]
enum AttrKind {
    /// Inline, no payload.
    None = 0,
    BoolFalse = 1,
    BoolTrue = 2,
    /// Inline as a 26-bit sign-extended payload when it fits; otherwise a
    /// record with head `[lo, hi]`: the zigzag-encoded value in 32-bit halves.
    Int = 3,
    /// Head: `[byte_len]`; tail: the UTF-8 bytes.
    String = 5,
    /// Same layout as `String`.
    EnumVariant = 6,
    /// Head: `[n]`; refs: `n` items.
    List = 7,
    Tuple = 8,
    /// Head: `[n]`; refs: `2n`, interleaved key, value.
    Dict = 9,
    /// Head: `[n]`; refs: `[default or absent, k0, v0, k1, v1, ...]`; keys
    /// are inline `Label` leaves.
    Selector = 10,
    /// Head: `[n]`; refs: `n` items.
    Concat = 11,
    /// Head: `[oneof_variant_idx]`; refs: `[item]`.
    OneOf = 12,
    /// Inline; payload: providers-label table index.
    Dep = 13,
    Label = 14,
    SourceLabel = 15,
    ConfigurationDep = 16,
    SplitTransitionDep = 17,
    /// Inline; payload: target-label table index.
    PluginDep = 18,
    /// Inline; payload: path table index.
    SourceFile = 19,
    /// Inline; payload: legacy table index: variants without a native
    /// encoding.
    Legacy = 20,
    /// `Arg` with no macros (`StringWithMacros::StringPart`) -- most `Arg`
    /// values in real graphs contain no macros. Same layout as `String`.
    ArgSimple = 21,
}

/// How a kind's record body is laid out: how many head fields it has and
/// what follows them. One definition serves decoding and page-in
/// validation.
#[derive(Copy, Clone)]
struct Layout {
    head: HeadLen,
    tail: TailKind,
}

#[derive(Copy, Clone)]
enum HeadLen {
    Fixed(u32),
}

#[derive(Copy, Clone)]
enum TailKind {
    None,
    /// Bytes, as many as field 0 says.
    Text,
    Refs(RefCount),
}

#[derive(Copy, Clone)]
enum RefCount {
    Field0,
    TwiceField0,
    TwiceField0Plus1,
    One,
}

impl AttrKind {
    fn layout(self) -> Layout {
        use HeadLen::*;
        use RefCount::*;
        use TailKind::*;
        let (head, tail) = match self {
            AttrKind::Int => (Fixed(2), None),
            AttrKind::String | AttrKind::EnumVariant | AttrKind::ArgSimple => (Fixed(1), Text),
            AttrKind::List | AttrKind::Tuple | AttrKind::Concat => (Fixed(1), Refs(Field0)),
            AttrKind::Dict => (Fixed(1), Refs(TwiceField0)),
            AttrKind::Selector => (Fixed(1), Refs(TwiceField0Plus1)),
            AttrKind::OneOf => (Fixed(1), Refs(One)),
            AttrKind::None
            | AttrKind::BoolFalse
            | AttrKind::BoolTrue
            | AttrKind::Dep
            | AttrKind::Label
            | AttrKind::SourceLabel
            | AttrKind::ConfigurationDep
            | AttrKind::SplitTransitionDep
            | AttrKind::PluginDep
            | AttrKind::SourceFile
            | AttrKind::Legacy => unreachable!("should be inline: leaf kinds have no record"),
        };
        Layout { head, tail }
    }
}

/// The tail of a record body, sized.
#[derive(Copy, Clone)]
enum TailLen {
    None,
    Text(u32),
    Refs(u32),
}

/// Head field count and tail of a `kind` record whose head field `i` reads
/// as `field(i)`; `None` when a field the layout depends on is missing.
#[inline]
fn shape(kind: AttrKind, field: impl Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
    let Layout { head, tail } = kind.layout();
    let HeadLen::Fixed(nfields) = head;
    let tail = match tail {
        TailKind::None => TailLen::None,
        TailKind::Text => TailLen::Text(field(0)?),
        TailKind::Refs(count) => TailLen::Refs(match count {
            RefCount::Field0 => field(0)?,
            RefCount::TwiceField0 => field(0)?.checked_mul(2)?,
            RefCount::TwiceField0Plus1 => field(0)?.checked_mul(2)?.checked_add(1)?,
            RefCount::One => 1,
        }),
    };
    Some((nfields, tail))
}

/// A `width`-byte little-endian field at `at`, if in bounds.
#[inline]
fn field_at(bytes: &[u8], at: usize, width: u8) -> Option<u32> {
    let b = bytes.get(at..at + width as usize)?;
    Some(match width {
        1 => b[0] as u32,
        2 => u16::from_le_bytes([b[0], b[1]]) as u32,
        3 => (b[0] as u32) | ((b[1] as u32) << 8) | ((b[2] as u32) << 16),
        _ => u32::from_le_bytes([b[0], b[1], b[2], b[3]]),
    })
}

#[inline]
fn zigzag(i: i64) -> u64 {
    ((i << 1) ^ (i >> 63)) as u64
}

#[inline]
fn unzigzag(v: u64) -> i64 {
    ((v >> 1) as i64) ^ -((v & 1) as i64)
}

/// Reference to an attribute value in a [`CoercedAttrArena`]: the byte
/// offset of the value's record. Only meaningful together with the arena
/// that minted it.
#[derive(
    Copy, Clone, Dupe, PartialEq, Eq, Hash, StrongHash, Debug, Allocative, Pagable
)]
pub struct AttrWord(u32);

/// Content hash of an encoded attribute value, as computed by
/// [`CoercedAttrArenaBuilder`]: kind-tagged, folded over child hashes, and stable
/// across arenas. Travels alongside an [`AttrWord`] so consumers can hash
/// and prefilter equality without decoding.
#[derive(
    Copy, Clone, Dupe, PartialEq, Eq, Hash, StrongHash, Debug, Allocative, Pagable
)]
pub struct CoercedAttrHash(pub u64);

impl AttrKind {
    /// Kinds encoded in the word itself, never as a record.
    fn is_inline(self) -> bool {
        matches!(
            self,
            AttrKind::None
                | AttrKind::BoolFalse
                | AttrKind::BoolTrue
                | AttrKind::Dep
                | AttrKind::Label
                | AttrKind::SourceLabel
                | AttrKind::ConfigurationDep
                | AttrKind::SplitTransitionDep
                | AttrKind::PluginDep
                | AttrKind::SourceFile
                | AttrKind::Legacy
        )
    }

    /// Kinds whose values hold no deps, inputs, or labels: a traversal can
    /// pass over them without decoding.
    fn is_scalar(self) -> bool {
        matches!(
            self,
            AttrKind::None
                | AttrKind::BoolFalse
                | AttrKind::BoolTrue
                | AttrKind::Int
                | AttrKind::String
                | AttrKind::EnumVariant
                | AttrKind::ArgSimple
        )
    }
}

const INLINE_BIT: u32 = 1 << 31;
const OFFSET_MASK: u32 = INLINE_BIT - 1;
const INLINE_KIND_SHIFT: u32 = 26;
const INLINE_PAYLOAD_BITS: u32 = 26;
const INLINE_PAYLOAD_MASK: u32 = (1 << INLINE_PAYLOAD_BITS) - 1;

/// Small ints are sign-extended inline payloads.
const INT_INLINE_MAX: i64 = (1 << (INLINE_PAYLOAD_BITS - 1)) - 1;
const INT_INLINE_MIN: i64 = -(1 << (INLINE_PAYLOAD_BITS - 1));

impl AttrWord {
    /// An inline leaf. Ints that do not fit the payload spill to records, so
    /// overflow means one package holds more distinct values of `kind` than
    /// the payload can index.
    fn inline(kind: AttrKind, payload: u32) -> buck2_error::Result<AttrWord> {
        if payload > INLINE_PAYLOAD_MASK {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "too many distinct `{kind:?}` attribute values in one package: at most {} are supported",
                INLINE_PAYLOAD_MASK as u64 + 1
            ));
        }
        Ok(AttrWord(
            INLINE_BIT | ((kind as u32) << INLINE_KIND_SHIFT) | payload,
        ))
    }

    /// The kind and payload of an inline leaf, or `None` for a record ref.
    #[inline]
    fn inline_parts(self) -> Option<(AttrKind, u32)> {
        if self.0 & INLINE_BIT == 0 {
            return None;
        }
        let kind = AttrKind::from_repr(((self.0 >> INLINE_KIND_SHIFT) & 0x1F) as u8)
            .expect("should be valid: words are only minted with known kinds");
        Some((kind, self.0 & INLINE_PAYLOAD_MASK))
    }

    /// A record ref to the record at `off`.
    #[inline]
    fn record(off: u32) -> AttrWord {
        AttrWord(off)
    }

    /// Byte offset of this record ref's tag.
    #[inline]
    fn off(self) -> usize {
        debug_assert!(self.0 & INLINE_BIT == 0, "inline leaves have no record");
        (self.0 & OFFSET_MASK) as usize
    }

    /// The field value that encodes this word inside the record at `rec`.
    #[inline]
    fn to_field(self, rec: u32) -> u32 {
        if self == ABSENT_REF {
            0
        } else if let Some((kind, payload)) = self.inline_parts() {
            (payload << 6) | ((kind as u32) << 1) | 1
        } else {
            (rec - self.0) << 1
        }
    }

    /// Inverse of [`Self::to_field`]; `None` for an absent value.
    #[inline]
    fn from_field(field: u32, rec: u32) -> Option<AttrWord> {
        if field == 0 {
            None
        } else if field & 1 == 1 {
            Some(AttrWord(
                INLINE_BIT | (((field >> 1) & 0x1F) << INLINE_KIND_SHIFT) | (field >> 6),
            ))
        } else {
            Some(AttrWord(rec - (field >> 1)))
        }
    }
}

/// A ref slot with nothing in it (a selector without a default); written
/// as a zero field.
const ABSENT_REF: AttrWord = AttrWord(u32::MAX);

/// The record buffer. Reads take its text records as UTF-8 on trust: the
/// encoder only writes `&str` content, and a buffer from anywhere else is
/// validated once, as it pages in.
#[derive(Debug, Default, Allocative)]
struct RecordBytes(Vec<u8>);

impl std::ops::Deref for RecordBytes {
    type Target = Vec<u8>;

    fn deref(&self) -> &Vec<u8> {
        &self.0
    }
}

impl std::ops::DerefMut for RecordBytes {
    fn deref_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl PagableSerialize for RecordBytes {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        self.0.pagable_serialize(serializer)
    }
}

impl<'de> PagableDeserialize<'de> for RecordBytes {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        let bytes = Vec::<u8>::pagable_deserialize(deserializer)?;
        validate_records(&bytes).map_err(|e| pagable::anyhow!("Corrupt attr arena: {e}"))?;
        Ok(RecordBytes(bytes))
    }
}

/// Checks that `bytes` is a well-formed sequence of records whose text
/// records hold UTF-8 and whose refs land on earlier records. Records are
/// self-delimiting, so this is one pass in storage order.
fn validate_records(bytes: &[u8]) -> Result<(), String> {
    let mut starts: Vec<u32> = Vec::new();
    let mut pos = 0usize;
    while pos < bytes.len() {
        let rec = pos;
        let t = bytes[pos];
        let kind = AttrKind::from_repr(t & TAG_KIND_MASK)
            .filter(|k| !k.is_inline())
            .ok_or_else(|| format!("record at {rec} has invalid tag {t:#x}"))?;
        let width = (t >> TAG_WIDTH_SHIFT) + 1;
        let start = pos + 1;
        let (nfields, tail) = shape(kind, |i| {
            field_at(bytes, start + i as usize * width as usize, width)
        })
        .ok_or_else(|| format!("record at {rec} is truncated"))?;
        let tail_start = start + nfields as usize * width as usize;
        let end = match tail {
            TailLen::None => tail_start,
            TailLen::Text(len) => {
                let text = bytes
                    .get(tail_start..tail_start + len as usize)
                    .ok_or_else(|| format!("record at {rec} is truncated"))?;
                std::str::from_utf8(text)
                    .map_err(|_| format!("text record at {rec} is not UTF-8"))?;
                tail_start + len as usize
            }
            TailLen::Refs(n) => {
                for i in 0..n as usize {
                    let field = field_at(bytes, tail_start + i * width as usize, width)
                        .ok_or_else(|| format!("record at {rec} is truncated"))?;
                    // Zero is an absent ref and odd fields are inline leaves;
                    // the rest are backward deltas.
                    if field != 0 && field & 1 == 0 {
                        (rec as u32)
                            .checked_sub(field >> 1)
                            .filter(|target| starts.binary_search(target).is_ok())
                            .ok_or_else(|| {
                                format!("record at {rec} has a ref that is not an earlier record")
                            })?;
                    }
                }
                tail_start + n as usize * width as usize
            }
        };
        if end > bytes.len() {
            return Err(format!("record at {rec} is truncated"));
        }
        starts.push(rec as u32);
        pos = end;
    }
    Ok(())
}

/// Sealed arena of coerced attribute values for one package.
#[derive(Debug, Default, Allocative, Pagable)]
pub struct CoercedAttrArena {
    /// The buffer every [`AttrWord`] points into: one tagged record per
    /// value.
    bytes: RecordBytes,
    /// Globally interned `ProvidersLabel`s, indexed by `Dep`, `Label`,
    /// `SourceLabel`, `ConfigurationDep`, and `SplitTransitionDep` leaves.
    providers_labels: Vec<ProvidersLabel>,
    /// Globally interned `TargetLabel`s, indexed by `PluginDep` records.
    target_labels: Vec<TargetLabel>,
    /// Source paths, indexed by `SourceFile` records.
    paths: Vec<CoercedPath>,
    /// Whole values for `Legacy` records: variants without a native encoding.
    legacy: Vec<CoercedAttr>,
    /// Identity-grade content hashes for root values (the values referenced
    /// from outside the arena: one per unique top-level attribute value),
    /// sorted by word for binary search. Read on the value-identity paths:
    /// attr equality for early cutoff when a package re-evaluates, and
    /// target hashing. Interior values carry no retained strong hash;
    /// 128-bit blake3 over full content, so equal hash implies equal value.
    // TODO(jtbraun): fold these parallel vecs into a `starlark_map` `Vec2`
    // once `pagable` has an impl for it.
    root_words: Vec<u32>,
    root_strong: Vec<StrongHash128>,
}

/// Per-package attribute storage shared by every `TargetNode` of the package.
///
/// Created before evaluation starts (values are built incrementally during
/// evaluation and each holds an `Arc` of this), sealed exactly once when the
/// package's `EvaluationResult` is assembled. Attribute reads before sealing
/// are a logic error.
#[derive(Debug, Default, Allocative, Pagable)]
pub struct PackageAttrStorage {
    arena: OnceLock<CoercedAttrArena>,
}

impl PackageAttrStorage {
    pub fn seal(&self, arena: CoercedAttrArena) {
        self.arena
            .set(arena)
            .expect("package attr storage sealed twice");
    }

    pub fn arena(&self) -> &CoercedAttrArena {
        self.arena
            .get()
            .expect("should be sealed: attrs are only read after package evaluation completes")
    }
}

/// The owned form of one attribute value, for holders that outlive any
/// package (rule defaults): arena-encoded in its own single-value storage,
/// unlike a plain [`CoercedAttr`] tree. Equality and hashing are
/// content-based, so two `OwnedCoercedAttr`s encoding equal values compare
/// equal regardless of which storage holds them.
#[derive(Clone, Dupe, Debug, Allocative, Pagable)]
pub struct OwnedCoercedAttr {
    storage: Arc<PackageAttrStorage>,
    word: AttrWord,
    hash: CoercedAttrHash,
}

impl OwnedCoercedAttr {
    pub fn try_new(attr: &CoercedAttr) -> buck2_error::Result<OwnedCoercedAttr> {
        // An `Encoded` value is a ref into the builder that minted it; a
        // fresh builder would pass the ref through and decode garbage.
        if matches!(attr, CoercedAttr::Encoded(..)) {
            return Err(internal_error!(
                "Encoded attr requires its minting builder; use `seal_from_builder`"
            ));
        }
        Self::seal_from_builder(CoercedAttrArenaBuilder::default(), attr)
    }

    /// Seal `builder` into this value's own storage. Any `Encoded` nodes in
    /// `attr` must be refs minted by `builder`.
    pub fn seal_from_builder(
        mut builder: CoercedAttrArenaBuilder,
        attr: &CoercedAttr,
    ) -> buck2_error::Result<OwnedCoercedAttr> {
        let (word, hash) = builder.encode_root(attr)?;
        let storage = Arc::new(PackageAttrStorage::default());
        storage.seal(builder.finish());
        Ok(OwnedCoercedAttr {
            storage,
            word,
            hash,
        })
    }

    pub fn as_ref(&self) -> CoercedAttrRef<'_> {
        self.storage.arena().view(self.word)
    }

    /// The identity-grade strong hash of the value.
    pub fn strong_hash(&self) -> StrongHash128 {
        self.storage
            .arena()
            .root_strong_hash(self.word)
            .expect("should be present: sealed with its value as a root")
    }
}

impl PartialEq for OwnedCoercedAttr {
    fn eq(&self, other: &OwnedCoercedAttr) -> bool {
        self.hash == other.hash && self.as_ref().content_eq(&other.as_ref())
    }
}

impl Eq for OwnedCoercedAttr {}

impl Hash for OwnedCoercedAttr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl CoercedAttrArena {
    pub fn builder() -> CoercedAttrArenaBuilder {
        CoercedAttrArenaBuilder::default()
    }

    /// Total retained bytes of the record buffer.
    pub fn byte_len(&self) -> usize {
        self.bytes.len()
    }

    pub fn view(&self, r: AttrWord) -> CoercedAttrRef<'_> {
        CoercedAttrRef { arena: self, r }
    }

    /// The identity-grade content hash of a root value, if `r` was interned
    /// as a root (`CoercedAttrArenaBuilder::encode_root`).
    pub fn root_strong_hash(&self, r: AttrWord) -> Option<StrongHash128> {
        self.root_words
            .binary_search(&r.0)
            .ok()
            .map(|i| self.root_strong[i])
    }

    /// The record's kind and the byte width of its refs. Record refs only.
    #[inline]
    fn tag(&self, r: AttrWord) -> (AttrKind, u8) {
        let t = self.bytes[r.off()];
        let kind = AttrKind::from_repr(t & TAG_KIND_MASK)
            .expect("should be valid: records are only written by the encoder");
        (kind, (t >> TAG_WIDTH_SHIFT) + 1)
    }

    #[inline]
    fn kind_of(&self, r: AttrWord) -> AttrKind {
        match r.inline_parts() {
            Some((kind, _)) => kind,
            None => self.tag(r).0,
        }
    }

    /// The record at `r`, located per its kind's layout. Record refs only.
    #[inline]
    fn open(&self, r: AttrWord) -> Rec<'_> {
        let (kind, width) = self.tag(r);
        let rec = r.off() as u32;
        let start = r.off() + 1;
        let (nfields, tail) = shape(kind, |i| {
            field_at(&self.bytes, start + i as usize * width as usize, width)
        })
        .expect("should be in bounds: records are only written by the encoder");
        let run = |start: usize, len: u32| RefRun {
            arena: self,
            rec,
            start,
            len,
            width,
        };
        let tail_start = start + nfields as usize * width as usize;
        let tail = match tail {
            TailLen::None => Tail::None,
            TailLen::Text(len) => Tail::Text(&self.bytes[tail_start..tail_start + len as usize]),
            TailLen::Refs(n) => Tail::Refs(run(tail_start, n)),
        };
        Rec {
            kind,
            head: run(start, nfields),
            tail,
        }
    }

    /// Identity-grade content hash of one value: blake3 over the value's kind,
    /// its immediate content via `StrongHash`, and its children's strong hashes.
    /// Equal content yields equal hashes across arenas and processes.
    fn strong_of(&self, r: AttrWord, memo: &mut BuckMutMap<u32, StrongHash128>) -> StrongHash128 {
        if let Some(&h) = memo.get(&r.0) {
            return h;
        }
        let mut h = Blake3StrongHasher::new();
        let kind = self.kind_of(r);
        h.write_u8(kind as u8);
        match self.view(r).kind() {
            CoercedAttrRefKind::None => {}
            // True/false are distinct kinds; the tag byte is the content.
            CoercedAttrRefKind::Bool(_) => {}
            CoercedAttrRefKind::Int(i) => h.write_i64(i),
            CoercedAttrRefKind::String(s)
            | CoercedAttrRefKind::EnumVariant(s)
            | CoercedAttrRefKind::ArgSimple(s) => s.strong_hash(&mut h),
            CoercedAttrRefKind::List(items)
            | CoercedAttrRefKind::Tuple(items)
            | CoercedAttrRefKind::Concat(items) => {
                h.write_usize(items.len());
                for v in items.iter() {
                    self.strong_of(v.r, memo).strong_hash(&mut h);
                }
            }
            CoercedAttrRefKind::Dict(pairs) => {
                h.write_usize(pairs.len());
                for (k, v) in pairs.iter() {
                    self.strong_of(k.r, memo).strong_hash(&mut h);
                    self.strong_of(v.r, memo).strong_hash(&mut h);
                }
            }
            CoercedAttrRefKind::Selector { entries, default } => {
                h.write_usize(entries.len());
                for (k, v) in entries.iter() {
                    k.strong_hash(&mut h);
                    self.strong_of(v.r, memo).strong_hash(&mut h);
                }
                match default {
                    Some(d) => {
                        h.write_u8(1);
                        self.strong_of(d.r, memo).strong_hash(&mut h);
                    }
                    None => h.write_u8(0),
                }
            }
            CoercedAttrRefKind::OneOf(l, idx) => {
                h.write_u32(idx);
                self.strong_of(l.r, memo).strong_hash(&mut h);
            }
            CoercedAttrRefKind::Dep(l)
            | CoercedAttrRefKind::Label(l)
            | CoercedAttrRefKind::SourceLabel(l)
            | CoercedAttrRefKind::ConfigurationDep(l)
            | CoercedAttrRefKind::SplitTransitionDep(l) => l.strong_hash(&mut h),
            CoercedAttrRefKind::PluginDep(t) => t.strong_hash(&mut h),
            CoercedAttrRefKind::SourceFile(p) => p.strong_hash(&mut h),
            CoercedAttrRefKind::Legacy(a) => a.strong_hash(&mut h),
        }
        let hash = h.finalize128();
        memo.insert(r.0, hash);
        hash
    }

    /// Raw bytes of a text record, without UTF-8 validation: the string-dedup
    /// equality probe needs only byte equality.
    fn bytes_at(&self, r: AttrWord) -> &[u8] {
        match self.open(r).tail {
            Tail::Text(bytes) => bytes,
            _ => unreachable!("should be text: only text records are read as bytes"),
        }
    }

    fn str_at(&self, r: AttrWord) -> &str {
        let bytes = self.bytes_at(r);
        debug_assert!(std::str::from_utf8(bytes).is_ok());
        // SAFETY: text records are written from `&str`, and a buffer from
        // outside this process is validated when it pages in (`RecordBytes`).
        unsafe { std::str::from_utf8_unchecked(bytes) }
    }
}

/// One located record: its head fields and tail.
#[derive(Copy, Clone)]
struct Rec<'a> {
    kind: AttrKind,
    head: RefRun<'a>,
    tail: Tail<'a>,
}

#[derive(Copy, Clone)]
enum Tail<'a> {
    None,
    Text(&'a [u8]),
    Refs(RefRun<'a>),
}

impl<'a> Rec<'a> {
    /// The ref tail of a record whose layout has one.
    #[inline]
    fn refs(&self) -> RefRun<'a> {
        match self.tail {
            Tail::Refs(run) => run,
            _ => unreachable!("should have refs: the kind's layout says so"),
        }
    }
}

/// A run of fixed-width values inside one record: head fields or ref
/// fields (see [`AttrWord::to_field`]).
#[derive(Copy, Clone)]
struct RefRun<'a> {
    arena: &'a CoercedAttrArena,
    rec: u32,
    start: usize,
    len: u32,
    width: u8,
}

impl<'a> RefRun<'a> {
    /// The `i`th fixed-width value, little-endian.
    #[inline]
    fn raw(&self, i: u32) -> u32 {
        let w = self.width as usize;
        let b = &self.arena.bytes[self.start + i as usize * w..][..w];
        match w {
            1 => b[0] as u32,
            2 => u16::from_le_bytes([b[0], b[1]]) as u32,
            3 => (b[0] as u32) | ((b[1] as u32) << 8) | ((b[2] as u32) << 16),
            _ => u32::from_le_bytes([b[0], b[1], b[2], b[3]]),
        }
    }

    #[inline]
    fn get(&self, i: u32) -> AttrWord {
        self.get_opt(i)
            .expect("should be present: only a selector default may be absent")
    }

    #[inline]
    fn get_opt(&self, i: u32) -> Option<AttrWord> {
        AttrWord::from_field(self.raw(i), self.rec)
    }

    fn len(&self) -> u32 {
        self.len
    }

    /// The run without its first `n` refs.
    fn skip(&self, n: u32) -> RefRun<'a> {
        RefRun {
            start: self.start + n as usize * self.width as usize,
            len: self.len - n,
            ..*self
        }
    }

    /// The raw values in order. One bounds check for the whole run; the
    /// width is matched once so each arm is a fixed-stride loop.
    fn iter_raw(&self) -> impl Iterator<Item = u32> + use<'a> {
        let w = self.width as usize;
        let bytes = &self.arena.bytes[self.start..self.start + self.len as usize * w];
        match w {
            1 => Either::Left(Either::Left(bytes.iter().map(|&b| b as u32))),
            2 => Either::Left(Either::Right(
                bytes
                    .as_chunks::<2>()
                    .0
                    .iter()
                    .map(|c| u16::from_le_bytes([c[0], c[1]]) as u32),
            )),
            3 => {
                Either::Right(Either::Left(bytes.as_chunks::<3>().0.iter().map(|c| {
                    (c[0] as u32) | ((c[1] as u32) << 8) | ((c[2] as u32) << 16)
                })))
            }
            _ => Either::Right(Either::Right(
                bytes
                    .as_chunks::<4>()
                    .0
                    .iter()
                    .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]])),
            )),
        }
    }

    fn iter(&self) -> impl Iterator<Item = AttrWord> + use<'a> {
        let rec = self.rec;
        self.iter_raw().map(move |f| {
            AttrWord::from_field(f, rec)
                .expect("should be present: only a selector default may be absent")
        })
    }
}

/// A decoded (shallow) view of one attribute value. Children decode lazily via
/// further `view` calls.
#[derive(Copy, Clone)]
pub struct CoercedAttrRef<'a> {
    arena: &'a CoercedAttrArena,
    r: AttrWord,
}

/// Shallow decode of an [`CoercedAttrRef`]. Mirrors the hot subset of
/// [`CoercedAttr`]; everything else surfaces as `Legacy`.
pub enum CoercedAttrRefKind<'a> {
    None,
    Bool(bool),
    Int(i64),
    String(&'a str),
    EnumVariant(&'a str),
    ArgSimple(&'a str),
    List(AttrRefSeq<'a>),
    Tuple(AttrRefSeq<'a>),
    Dict(AttrRefDict<'a>),
    Selector {
        entries: AttrRefSelectorEntries<'a>,
        default: Option<CoercedAttrRef<'a>>,
    },
    Concat(AttrRefSeq<'a>),
    OneOf(CoercedAttrRef<'a>, u32),
    Dep(&'a ProvidersLabel),
    Label(&'a ProvidersLabel),
    SourceLabel(&'a ProvidersLabel),
    ConfigurationDep(&'a ProvidersLabel),
    SplitTransitionDep(&'a ProvidersLabel),
    PluginDep(&'a TargetLabel),
    SourceFile(&'a CoercedPath),
    Legacy(&'a CoercedAttr),
}

/// View of a sequence value's child refs (`len`/`iter`).
#[derive(Copy, Clone)]
pub struct AttrRefSeq<'a> {
    run: RefRun<'a>,
}

impl<'a> AttrRefSeq<'a> {
    pub fn len(&self) -> usize {
        self.run.len() as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = CoercedAttrRef<'a>> + use<'a> {
        let arena = self.run.arena;
        self.run.iter().map(move |r| arena.view(r))
    }
}

/// View of a dict value's (key, value) child refs.
#[derive(Copy, Clone)]
pub struct AttrRefDict<'a> {
    /// Interleaved key, value refs, two per entry.
    run: RefRun<'a>,
}

impl<'a> AttrRefDict<'a> {
    pub fn len(&self) -> usize {
        (self.run.len() / 2) as usize
    }

    pub fn iter(&self) -> impl Iterator<Item = (CoercedAttrRef<'a>, CoercedAttrRef<'a>)> + 'a {
        let arena = self.run.arena;
        self.run
            .iter()
            .tuples()
            .map(move |(k, v)| (arena.view(k), arena.view(v)))
    }
}

/// View of a selector value's (configuration setting key, value) entries.
#[derive(Copy, Clone)]
pub struct AttrRefSelectorEntries<'a> {
    /// Interleaved key, value refs; keys are inline `Label` leaves.
    run: RefRun<'a>,
}

impl<'a> AttrRefSelectorEntries<'a> {
    pub fn len(&self) -> usize {
        self.run.len() as usize / 2
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'a ProvidersLabel, CoercedAttrRef<'a>)> + 'a {
        let arena = self.run.arena;
        let mut refs = self.run.iter();
        std::iter::from_fn(move || {
            let k = refs.next()?;
            let v = refs
                .next()
                .expect("should be paired: entries are key, value");
            let idx = match k.inline_parts() {
                Some((AttrKind::Label, idx)) => idx,
                _ => unreachable!("should be a label leaf: selector keys are encoded inline"),
            };
            Some((&arena.providers_labels[idx as usize], arena.view(v)))
        })
    }
}

impl<'a> CoercedAttrRef<'a> {
    pub fn kind(&self) -> CoercedAttrRefKind<'a> {
        let arena = self.arena;
        let r = self.r;
        if let Some((kind, p)) = r.inline_parts() {
            return match kind {
                AttrKind::None => CoercedAttrRefKind::None,
                AttrKind::BoolFalse => CoercedAttrRefKind::Bool(false),
                AttrKind::BoolTrue => CoercedAttrRefKind::Bool(true),
                AttrKind::Int => {
                    // Sign-extend the 26-bit payload.
                    let shifted = (p << (32 - INLINE_PAYLOAD_BITS)) as i32;
                    CoercedAttrRefKind::Int((shifted >> (32 - INLINE_PAYLOAD_BITS)) as i64)
                }
                AttrKind::Dep => CoercedAttrRefKind::Dep(&arena.providers_labels[p as usize]),
                AttrKind::Label => CoercedAttrRefKind::Label(&arena.providers_labels[p as usize]),
                AttrKind::SourceLabel => {
                    CoercedAttrRefKind::SourceLabel(&arena.providers_labels[p as usize])
                }
                AttrKind::ConfigurationDep => {
                    CoercedAttrRefKind::ConfigurationDep(&arena.providers_labels[p as usize])
                }
                AttrKind::SplitTransitionDep => {
                    CoercedAttrRefKind::SplitTransitionDep(&arena.providers_labels[p as usize])
                }
                AttrKind::PluginDep => {
                    CoercedAttrRefKind::PluginDep(&arena.target_labels[p as usize])
                }
                AttrKind::SourceFile => CoercedAttrRefKind::SourceFile(&arena.paths[p as usize]),
                AttrKind::Legacy => CoercedAttrRefKind::Legacy(&arena.legacy[p as usize]),
                _ => unreachable!("should be a leaf kind: only leaves are inline"),
            };
        }
        let rec = arena.open(r);
        match rec.kind {
            AttrKind::None
            | AttrKind::BoolFalse
            | AttrKind::BoolTrue
            | AttrKind::Dep
            | AttrKind::Label
            | AttrKind::SourceLabel
            | AttrKind::ConfigurationDep
            | AttrKind::SplitTransitionDep
            | AttrKind::PluginDep
            | AttrKind::SourceFile
            | AttrKind::Legacy => unreachable!("should be inline: leaf kinds have no record"),
            AttrKind::Int => CoercedAttrRefKind::Int(unzigzag(
                rec.head.raw(0) as u64 | ((rec.head.raw(1) as u64) << 32),
            )),
            AttrKind::String => CoercedAttrRefKind::String(arena.str_at(r)),
            AttrKind::EnumVariant => CoercedAttrRefKind::EnumVariant(arena.str_at(r)),
            AttrKind::ArgSimple => CoercedAttrRefKind::ArgSimple(arena.str_at(r)),
            AttrKind::List => CoercedAttrRefKind::List(AttrRefSeq { run: rec.refs() }),
            AttrKind::Tuple => CoercedAttrRefKind::Tuple(AttrRefSeq { run: rec.refs() }),
            AttrKind::Concat => CoercedAttrRefKind::Concat(AttrRefSeq { run: rec.refs() }),
            AttrKind::Dict => CoercedAttrRefKind::Dict(AttrRefDict { run: rec.refs() }),
            AttrKind::Selector => {
                let refs = rec.refs();
                CoercedAttrRefKind::Selector {
                    entries: AttrRefSelectorEntries { run: refs.skip(1) },
                    default: refs.get_opt(0).map(|d| arena.view(d)),
                }
            }
            AttrKind::OneOf => {
                let child = arena.view(rec.refs().get(0));
                CoercedAttrRefKind::OneOf(child, rec.head.raw(0))
            }
        }
    }

    /// Reconstruct an owned [`CoercedAttr`]. Bridge for consumers that take
    /// owned enum values, and for round-trip testing.
    pub fn to_coerced(&self) -> buck2_error::Result<CoercedAttr> {
        Ok(match self.kind() {
            CoercedAttrRefKind::None => CoercedAttr::None,
            CoercedAttrRefKind::Bool(b) => CoercedAttr::Bool(BoolLiteral(b)),
            CoercedAttrRefKind::Int(i) => CoercedAttr::Int(i),
            CoercedAttrRefKind::String(s) => CoercedAttr::String(StringLiteral(ArcStr::from(s))),
            CoercedAttrRefKind::EnumVariant(s) => {
                CoercedAttr::EnumVariant(StringLiteral(ArcStr::from(s)))
            }
            CoercedAttrRefKind::ArgSimple(s) => {
                CoercedAttr::Arg(StringWithMacros::StringPart(ArcStr::from(s)))
            }
            CoercedAttrRefKind::List(seq) => CoercedAttr::List(ListLiteral(
                seq.iter()
                    .map(|v| v.to_coerced())
                    .collect::<buck2_error::Result<ArcSlice<_>>>()?,
            )),
            CoercedAttrRefKind::Tuple(seq) => CoercedAttr::Tuple(TupleLiteral(
                seq.iter()
                    .map(|v| v.to_coerced())
                    .collect::<buck2_error::Result<ArcSlice<_>>>()?,
            )),
            CoercedAttrRefKind::Dict(pairs) => CoercedAttr::Dict(DictLiteral(
                pairs
                    .iter()
                    .map(|(k, v)| Ok((k.to_coerced()?, v.to_coerced()?)))
                    .collect::<buck2_error::Result<ArcSlice<_>>>()?,
            )),
            CoercedAttrRefKind::Selector { entries, default } => {
                let entries = entries
                    .iter()
                    .map(|(key, v)| Ok((ConfigurationSettingKey(key.dupe()), v.to_coerced()?)))
                    .collect::<buck2_error::Result<ArcSlice<_>>>()?;
                let default = default.map(|d| d.to_coerced()).transpose()?;
                CoercedAttr::Selector(Arc::new(
                    CoercedSelector::new(entries, default)
                        .internal_error("should be valid: encoded from a valid selector")?,
                ))
            }
            CoercedAttrRefKind::Concat(seq) => CoercedAttr::Concat(CoercedConcat(
                seq.iter()
                    .map(|v| v.to_coerced())
                    .collect::<buck2_error::Result<ArcSlice<_>>>()?,
            )),
            CoercedAttrRefKind::OneOf(child, idx) => {
                CoercedAttr::OneOf(Box::new(child.to_coerced()?), idx)
            }
            CoercedAttrRefKind::Dep(l) => CoercedAttr::Dep(l.dupe()),
            CoercedAttrRefKind::Label(l) => CoercedAttr::Label(l.dupe()),
            CoercedAttrRefKind::SourceLabel(l) => CoercedAttr::SourceLabel(l.dupe()),
            CoercedAttrRefKind::ConfigurationDep(l) => CoercedAttr::ConfigurationDep(l.dupe()),
            CoercedAttrRefKind::SplitTransitionDep(l) => CoercedAttr::SplitTransitionDep(l.dupe()),
            CoercedAttrRefKind::PluginDep(l) => CoercedAttr::PluginDep(l.dupe()),
            CoercedAttrRefKind::SourceFile(p) => CoercedAttr::SourceFile(p.clone()),
            CoercedAttrRefKind::Legacy(a) => a.clone(),
        })
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// The stored enum value for variants without a native encoding, if this
    /// value is one.
    pub fn unpack_legacy(&self) -> Option<&'a CoercedAttr> {
        match self.kind() {
            CoercedAttrRefKind::Legacy(a) => Some(a),
            _ => None,
        }
    }

    /// Whether evaluating this value can ever yield `None`, directly or
    /// through a `select()` branch.
    pub fn may_return_none(&self) -> bool {
        match self.kind() {
            CoercedAttrRefKind::None => true,
            CoercedAttrRefKind::Selector { entries, default } => {
                entries.iter().any(|(_, v)| v.may_return_none())
                    || default.is_some_and(|d| d.may_return_none())
            }
            CoercedAttrRefKind::Legacy(a) => a.may_return_none(),
            _ => false,
        }
    }

    /// The stored identity-grade content hash, if this ref was interned as
    /// a root. Equal content yields equal hashes across arenas and processes.
    pub fn root_strong_hash(&self) -> Option<StrongHash128> {
        self.arena.root_strong_hash(self.r)
    }

    /// Structural equality across (possibly different) arenas. Refs are
    /// arena-local, so cutoff comparisons between old and new values must
    /// compare content.
    pub fn content_eq(&self, other: &CoercedAttrRef<'_>) -> bool {
        match (self.kind(), other.kind()) {
            (CoercedAttrRefKind::None, CoercedAttrRefKind::None) => true,
            (CoercedAttrRefKind::Bool(a), CoercedAttrRefKind::Bool(b)) => a == b,
            (CoercedAttrRefKind::Int(a), CoercedAttrRefKind::Int(b)) => a == b,
            (CoercedAttrRefKind::String(a), CoercedAttrRefKind::String(b)) => a == b,
            (CoercedAttrRefKind::EnumVariant(a), CoercedAttrRefKind::EnumVariant(b)) => a == b,
            (CoercedAttrRefKind::ArgSimple(a), CoercedAttrRefKind::ArgSimple(b)) => a == b,
            (CoercedAttrRefKind::List(a), CoercedAttrRefKind::List(b))
            | (CoercedAttrRefKind::Tuple(a), CoercedAttrRefKind::Tuple(b))
            | (CoercedAttrRefKind::Concat(a), CoercedAttrRefKind::Concat(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.content_eq(&y))
            }
            (CoercedAttrRefKind::Dict(a), CoercedAttrRefKind::Dict(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|((ka, va), (kb, vb))| ka.content_eq(&kb) && va.content_eq(&vb))
            }
            (
                CoercedAttrRefKind::Selector {
                    entries: ea,
                    default: da,
                },
                CoercedAttrRefKind::Selector {
                    entries: eb,
                    default: db,
                },
            ) => {
                ea.len() == eb.len()
                    && match (da, db) {
                        (None, None) => true,
                        (Some(x), Some(y)) => x.content_eq(&y),
                        _ => false,
                    }
                    && ea
                        .iter()
                        .zip(eb.iter())
                        .all(|((ka, va), (kb, vb))| ka == kb && va.content_eq(&vb))
            }
            (CoercedAttrRefKind::OneOf(a, ia), CoercedAttrRefKind::OneOf(b, ib)) => {
                ia == ib && a.content_eq(&b)
            }
            (CoercedAttrRefKind::Dep(a), CoercedAttrRefKind::Dep(b))
            | (CoercedAttrRefKind::Label(a), CoercedAttrRefKind::Label(b))
            | (CoercedAttrRefKind::SourceLabel(a), CoercedAttrRefKind::SourceLabel(b))
            | (CoercedAttrRefKind::ConfigurationDep(a), CoercedAttrRefKind::ConfigurationDep(b))
            | (
                CoercedAttrRefKind::SplitTransitionDep(a),
                CoercedAttrRefKind::SplitTransitionDep(b),
            ) => a == b,
            (CoercedAttrRefKind::PluginDep(a), CoercedAttrRefKind::PluginDep(b)) => a == b,
            (CoercedAttrRefKind::SourceFile(a), CoercedAttrRefKind::SourceFile(b)) => a == b,
            (CoercedAttrRefKind::Legacy(a), CoercedAttrRefKind::Legacy(b)) => a == b,
            _ => false,
        }
    }

    /// Type-threaded traversal, mirroring `CoercedAttr::traverse` /
    /// `CoercedAttrWithType::pack`. Callback borrows come from the arena's
    /// side tables, so they carry the arena's lifetime.
    pub fn traverse(
        &self,
        t: &AttrType,
        pkg: Option<PackageLabel>,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        if self.arena.kind_of(self.r).is_scalar() {
            return Ok(());
        }
        match self.kind() {
            // Selects and legacy values are type-transparent here: legacy
            // delegates to the enum path with the full type so Option/Any
            // unwrapping stays in one place.
            CoercedAttrRefKind::Selector { entries, default } => {
                for (key, v) in entries.iter() {
                    traversal.configuration_dep(key, ConfigurationDepKind::SelectKey)?;
                    v.traverse(t, pkg, traversal)?;
                }
                if let Some(d) = default {
                    d.traverse(t, pkg, traversal)?;
                }
                Ok(())
            }
            CoercedAttrRefKind::Concat(items) => {
                for v in items.iter() {
                    v.traverse(t, pkg, traversal)?;
                }
                Ok(())
            }
            CoercedAttrRefKind::None => Ok(()),
            CoercedAttrRefKind::Legacy(a) => a.traverse(t, pkg, traversal),
            kind => Self::traverse_typed(kind, t, pkg, traversal),
        }
    }

    fn traverse_typed(
        kind: CoercedAttrRefKind<'a>,
        t: &AttrType,
        pkg: Option<PackageLabel>,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> buck2_error::Result<()> {
        match &t.0.inner {
            AttrTypeInner::Option(opt) => Self::traverse_typed(kind, &opt.inner, pkg, traversal),
            AttrTypeInner::Any(_) => match kind {
                CoercedAttrRefKind::List(items) | CoercedAttrRefKind::Tuple(items) => {
                    for v in items.iter() {
                        v.traverse(t, pkg, traversal)?;
                    }
                    Ok(())
                }
                CoercedAttrRefKind::Dict(pairs) => {
                    for (k, v) in pairs.iter() {
                        k.traverse(t, pkg, traversal)?;
                        v.traverse(t, pkg, traversal)?;
                    }
                    Ok(())
                }
                CoercedAttrRefKind::Bool(..)
                | CoercedAttrRefKind::Int(..)
                | CoercedAttrRefKind::String(..) => Ok(()),
                _ => Err(internal_error!(
                    "attr arena traverse: unexpected any-typed kind"
                )),
            },
            inner => match (kind, inner) {
                (CoercedAttrRefKind::Bool(..), AttrTypeInner::Bool(_))
                | (CoercedAttrRefKind::Int(..), AttrTypeInner::Int(_))
                | (CoercedAttrRefKind::String(..), AttrTypeInner::String(_))
                | (CoercedAttrRefKind::EnumVariant(..), AttrTypeInner::Enum(_))
                | (CoercedAttrRefKind::ArgSimple(..), AttrTypeInner::Arg(_)) => Ok(()),
                (CoercedAttrRefKind::List(items), AttrTypeInner::List(t)) => {
                    for v in items.iter() {
                        v.traverse(&t.inner, pkg, traversal)?;
                    }
                    Ok(())
                }
                (CoercedAttrRefKind::Tuple(items), AttrTypeInner::Tuple(ty)) => {
                    if items.len() != ty.xs.len() {
                        return Err(internal_error!("Inconsistent number of elements in tuple"));
                    }
                    for (v, vt) in items.iter().zip(&ty.xs) {
                        v.traverse(vt, pkg, traversal)?;
                    }
                    Ok(())
                }
                (CoercedAttrRefKind::Dict(pairs), AttrTypeInner::Dict(ty)) => {
                    for (k, v) in pairs.iter() {
                        k.traverse(&ty.key, pkg, traversal)?;
                        v.traverse(&ty.value, pkg, traversal)?;
                    }
                    Ok(())
                }
                (CoercedAttrRefKind::OneOf(v, i), AttrTypeInner::OneOf(ty)) => {
                    let item_type = ty
                        .xs
                        .get(i as usize)
                        .ok_or_else(|| internal_error!("invalid oneof index"))?;
                    v.traverse(item_type, pkg, traversal)
                }
                (CoercedAttrRefKind::Dep(d), AttrTypeInner::Dep(ty)) => {
                    DepAttr::<ProvidersLabel>::traverse(d, ty, traversal)
                }
                (CoercedAttrRefKind::SourceLabel(s), AttrTypeInner::Source(_)) => traversal.dep(s),
                (CoercedAttrRefKind::Label(l), AttrTypeInner::Label(_)) => traversal.label(l),
                (CoercedAttrRefKind::ConfigurationDep(d), AttrTypeInner::ConfigurationDep(ty)) => {
                    traversal.configuration_dep(d, ty.0)
                }
                (
                    CoercedAttrRefKind::SplitTransitionDep(d),
                    AttrTypeInner::SplitTransitionDep(ty),
                ) => traversal.split_transition_dep(d, &ty.transition),
                (CoercedAttrRefKind::PluginDep(d), AttrTypeInner::PluginDep(ty)) => {
                    traversal.plugin_dep(d, ty.kind())
                }
                (CoercedAttrRefKind::SourceFile(source), AttrTypeInner::Source(_)) => {
                    let pkg = pkg.ok_or_else(|| {
                        internal_error!(
                            "Expected a package when traversing coerced source attribute"
                        )
                    })?;
                    for x in source.inputs() {
                        traversal.input(SourcePathRef::new(pkg, x))?;
                    }
                    Ok(())
                }
                _ => Err(internal_error!(
                    "attr arena traverse: kind and type mismatch"
                )),
            },
        }
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// Native mirror of `CoercedAttr::configure` (via
    /// `CoercedAttrWithType::pack`): builds the `ConfiguredAttr` straight off
    /// the arena without materializing a transient `CoercedAttr` tree.
    ///
    /// TODO(jtbraun): apply `ErrorTag::ConfigureAttr` to errors and replicate the
    /// `select_first_match_differs` soft error, matching the enum path.
    pub fn configure(
        &self,
        t: &AttrType,
        ctx: &dyn AttrConfigurationContext,
        attr_name: Option<&str>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        let configured = match self.kind() {
            CoercedAttrRefKind::Selector { entries, default } => {
                match Self::select_view(ctx, entries, default)? {
                    Some(selected) => return selected.configure(t, ctx, attr_name),
                    None => {
                        return buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "None of {} conditions matched configuration `{}` and no default was set:\n{}",
                            entries.len(),
                            ctx.cfg().cfg(),
                            entries.iter().map(|(k, _)| format!("  {k}")).join("\n"),
                        )
                        .into();
                    }
                }
            }
            CoercedAttrRefKind::Concat(items) => {
                let singleton = items.len() == 1;
                let mut configured = Vec::with_capacity(items.len());
                for item in items.iter() {
                    configured.push(item.configure(t, ctx, attr_name)?);
                }
                let mut iter = configured.into_iter();
                let first = match iter.next() {
                    Some(first) => first,
                    None => return internal_error!("concat with no items").into(),
                };
                if singleton {
                    first
                } else {
                    first.concat(t, &mut iter.map(Ok))?
                }
            }
            CoercedAttrRefKind::None => ConfiguredAttr::None,
            // Full type: pack handles Option/Any unwrapping and the
            // SelectFail/SelectIncompatible outcomes for legacy values.
            CoercedAttrRefKind::Legacy(a) => return a.configure(t, ctx, attr_name),
            kind => return Self::configure_typed(kind, t, ctx, attr_name),
        };
        ResultMaybeCompatible::Compatible(configured)
    }

    /// Resolve a select against the configuration: most specific matching
    /// entry, or `Ok(None)` for "use the default / no match".
    fn select_view(
        ctx: &dyn AttrConfigurationContext,
        entries: AttrRefSelectorEntries<'a>,
        default: Option<CoercedAttrRef<'a>>,
    ) -> buck2_error::Result<Option<CoercedAttrRef<'a>>> {
        let matched_cfg_keys = ctx.matched_cfg_keys();
        let resolved: SmallVec<[(&ProvidersLabel, &ConfigSettingData, CoercedAttrRef<'a>); 17]> =
            entries
                .iter()
                .filter_map(|(k, v)| {
                    matched_cfg_keys
                        .setting_matches(ConfigurationSettingKey::ref_cast(k))
                        .map(|d| (k, d, v))
                })
                .collect();

        let mut iter = resolved.iter();
        let Some(mut matching) = iter.next() else {
            return Ok(default);
        };
        for entry in iter {
            let (_, conf, _) = entry;
            let (_, prev_conf, _) = matching;
            if conf.refines(prev_conf) {
                matching = entry;
            } else if prev_conf.refines(conf) {
                // keep `matching`
            } else {
                return Self::select_view_slow(&resolved).map(Some);
            }
        }
        Ok(Some(matching.2))
    }

    fn select_view_slow(
        resolved: &[(&ProvidersLabel, &ConfigSettingData, CoercedAttrRef<'a>)],
    ) -> buck2_error::Result<CoercedAttrRef<'a>> {
        let mut entries: Vec<(&ProvidersLabel, &ConfigSettingData, CoercedAttrRef<'a>)> =
            Vec::new();
        for (k, d, v) in resolved {
            if entries.iter().any(|(_, prev_d, _)| prev_d.refines(d)) {
                continue;
            }
            entries.retain(|(_, prev_d, _)| !d.refines(prev_d));
            entries.push((k, d, *v));
        }
        match entries.as_slice() {
            [] => Err(internal_error!(
                "no entries after slow select the most specific"
            )),
            [(.., x)] => Ok(*x),
            multiple_entries => {
                let (first_key, _, first_value) = &multiple_entries[0];
                let different_value_entry = multiple_entries
                    .iter()
                    .skip(1)
                    .find(|(_, _, v)| !v.content_eq(first_value));
                if let Some((different_key, _, _)) = different_value_entry {
                    Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Both select keys `{first_key}` and `{different_key}` match the configuration, but neither is more specific and they have different values"
                    ))
                } else {
                    Ok(*first_value)
                }
            }
        }
    }

    fn configure_typed(
        kind: CoercedAttrRefKind<'a>,
        t: &AttrType,
        ctx: &dyn AttrConfigurationContext,
        attr_name: Option<&str>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        let configured = match &t.0.inner {
            AttrTypeInner::Option(opt) => {
                return Self::configure_typed(kind, &opt.inner, ctx, attr_name);
            }
            AttrTypeInner::Any(_) => return Self::configure_any(kind, ctx, attr_name),
            inner => match (kind, inner) {
                (CoercedAttrRefKind::Bool(v), AttrTypeInner::Bool(_)) => {
                    ConfiguredAttr::Bool(BoolLiteral(v))
                }
                (CoercedAttrRefKind::Int(v), AttrTypeInner::Int(_)) => ConfiguredAttr::Int(v),
                (CoercedAttrRefKind::String(s), AttrTypeInner::String(_)) => {
                    ConfiguredAttr::String(StringLiteral(ArcStr::from(s)))
                }
                (CoercedAttrRefKind::EnumVariant(s), AttrTypeInner::Enum(_)) => {
                    ConfiguredAttr::EnumVariant(StringLiteral(ArcStr::from(s)))
                }
                (CoercedAttrRefKind::List(items), AttrTypeInner::List(ty)) => {
                    ConfiguredAttr::List(ListLiteral(
                        items
                            .iter()
                            .map(|v| v.configure(&ty.inner, ctx, attr_name))
                            .collect::<ResultMaybeCompatible<Vec<_>>>()?
                            .into(),
                    ))
                }
                (CoercedAttrRefKind::Tuple(items), AttrTypeInner::Tuple(ty)) => {
                    if items.len() != ty.xs.len() {
                        return internal_error!("Inconsistent number of elements in tuple").into();
                    }
                    ConfiguredAttr::Tuple(TupleLiteral(
                        items
                            .iter()
                            .zip(&ty.xs)
                            .map(|(v, vt)| v.configure(vt, ctx, attr_name))
                            .collect::<ResultMaybeCompatible<_>>()?,
                    ))
                }
                (CoercedAttrRefKind::Dict(pairs), AttrTypeInner::Dict(ty)) => {
                    ConfiguredAttr::Dict(DictLiteral(
                        pairs
                            .iter()
                            .map(|(k, v)| {
                                let k2 = k.configure(&ty.key, ctx, attr_name)?;
                                let v2 = v.configure(&ty.value, ctx, attr_name)?;
                                ResultMaybeCompatible::Compatible((k2, v2))
                            })
                            .collect::<ResultMaybeCompatible<Vec<_>>>()?
                            .into(),
                    ))
                }
                (CoercedAttrRefKind::OneOf(v, i), AttrTypeInner::OneOf(ty)) => {
                    let item_ty = match ty.xs.get(i as usize) {
                        Some(ty) => ty,
                        None => return internal_error!("invalid oneof index").into(),
                    };
                    let configured = v.configure(item_ty, ctx, attr_name)?;
                    ConfiguredAttr::OneOf(Box::new(configured), i)
                }
                (CoercedAttrRefKind::Dep(d), AttrTypeInner::Dep(ty)) => ty.configure(d, ctx)?,
                (CoercedAttrRefKind::SourceLabel(s), AttrTypeInner::Source(_)) => {
                    ConfiguredAttr::SourceLabel(s.configure_pair(ctx.cfg().cfg_pair().dupe()))
                }
                (CoercedAttrRefKind::Label(l), AttrTypeInner::Label(_)) => {
                    LabelAttrType::configure(ctx, l)?
                }
                (CoercedAttrRefKind::ConfigurationDep(d), AttrTypeInner::ConfigurationDep(_)) => {
                    ConfigurationDepAttrType::configure(ctx, d)?
                }
                (
                    CoercedAttrRefKind::SplitTransitionDep(d),
                    AttrTypeInner::SplitTransitionDep(ty),
                ) => ty.configure(d, ctx)?,
                (CoercedAttrRefKind::PluginDep(d), AttrTypeInner::PluginDep(ty)) => {
                    ConfiguredAttr::PluginDep(d.dupe(), ty.kind().dupe())
                }
                (CoercedAttrRefKind::SourceFile(s), AttrTypeInner::Source(_)) => {
                    ConfiguredAttr::SourceFile(s.clone())
                }
                (CoercedAttrRefKind::ArgSimple(s), AttrTypeInner::Arg(ty)) => {
                    ConfiguredAttr::Arg(ConfiguredStringWithMacros {
                        string_with_macros: StringWithMacros::StringPart(ArcStr::from(s)),
                        anon_target_compatible: ty.anon_target_compatible,
                    })
                }
                _ => return internal_error!("attr arena configure: kind and type mismatch").into(),
            },
        };
        ResultMaybeCompatible::Compatible(configured)
    }

    fn configure_any(
        kind: CoercedAttrRefKind<'a>,
        ctx: &dyn AttrConfigurationContext,
        attr_name: Option<&str>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        let configured = match kind {
            CoercedAttrRefKind::Bool(v) => ConfiguredAttr::Bool(BoolLiteral(v)),
            CoercedAttrRefKind::Int(v) => ConfiguredAttr::Int(v),
            CoercedAttrRefKind::String(s) => ConfiguredAttr::String(StringLiteral(ArcStr::from(s))),
            CoercedAttrRefKind::List(items) | CoercedAttrRefKind::Tuple(items) => {
                let configured = items
                    .iter()
                    .map(|v| v.configure(AttrType::any_ref(), ctx, attr_name))
                    .collect::<ResultMaybeCompatible<Vec<_>>>()?;
                if matches!(kind, CoercedAttrRefKind::List(_)) {
                    ConfiguredAttr::List(ListLiteral(configured.into()))
                } else {
                    ConfiguredAttr::Tuple(TupleLiteral(configured.into()))
                }
            }
            CoercedAttrRefKind::Dict(pairs) => ConfiguredAttr::Dict(DictLiteral(
                pairs
                    .iter()
                    .map(|(k, v)| {
                        let k2 = k.configure(AttrType::any_ref(), ctx, attr_name)?;
                        let v2 = v.configure(AttrType::any_ref(), ctx, attr_name)?;
                        ResultMaybeCompatible::Compatible((k2, v2))
                    })
                    .collect::<ResultMaybeCompatible<Vec<_>>>()?
                    .into(),
            )),
            _ => return internal_error!("attr arena configure: unexpected any-typed kind").into(),
        };
        ResultMaybeCompatible::Compatible(configured)
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// The value's JSON rendering. Mirrors `CoercedAttr::to_json` arm for
    /// arm; `Legacy` delegates to it.
    pub fn to_json(
        &self,
        ctx: &crate::attrs::fmt_context::AttrFmtContext,
    ) -> buck2_error::Result<serde_json::Value> {
        match self.kind() {
            CoercedAttrRefKind::None => Ok(serde_json::Value::Null),
            CoercedAttrRefKind::Bool(b) => Ok(to_value(b)?),
            CoercedAttrRefKind::Int(i) => Ok(to_value(i)?),
            CoercedAttrRefKind::String(s)
            | CoercedAttrRefKind::EnumVariant(s)
            | CoercedAttrRefKind::ArgSimple(s) => Ok(to_value(s)?),
            CoercedAttrRefKind::List(items) | CoercedAttrRefKind::Tuple(items) => {
                Ok(serde_json::Value::Array(
                    items
                        .iter()
                        .map(|v| v.to_json(ctx))
                        .collect::<buck2_error::Result<_>>()?,
                ))
            }
            CoercedAttrRefKind::Dict(pairs) => {
                let mut res = serde_json::Map::with_capacity(pairs.len());
                for (k, v) in pairs.iter() {
                    res.insert(
                        k.to_json(ctx)?
                            .as_str()
                            .ok_or_else(|| {
                                internal_error!("dict keys must render to JSON strings")
                            })?
                            .to_owned(),
                        v.to_json(ctx)?,
                    );
                }
                Ok(serde_json::Value::Object(res))
            }
            CoercedAttrRefKind::Selector { entries, default } => {
                let mut map = serde_json::Map::new();
                for (key, value) in entries.iter() {
                    map.insert(key.to_string(), value.to_json(ctx)?);
                }
                if let Some(default) = default {
                    map.insert("DEFAULT".to_owned(), default.to_json(ctx)?);
                }
                Ok(serde_json::Value::Object(serde_json::Map::from_iter([
                    (
                        "__type".to_owned(),
                        serde_json::Value::String("selector".to_owned()),
                    ),
                    ("entries".to_owned(), serde_json::Value::Object(map)),
                ])))
            }
            CoercedAttrRefKind::Concat(items) => {
                Ok(serde_json::Value::Object(serde_json::Map::from_iter([
                    (
                        "__type".to_owned(),
                        serde_json::Value::String("concat".to_owned()),
                    ),
                    (
                        "items".to_owned(),
                        serde_json::Value::Array(
                            items
                                .iter()
                                .map(|v| v.to_json(ctx))
                                .collect::<buck2_error::Result<_>>()?,
                        ),
                    ),
                ])))
            }
            CoercedAttrRefKind::OneOf(l, _) => l.to_json(ctx),
            CoercedAttrRefKind::Dep(l)
            | CoercedAttrRefKind::Label(l)
            | CoercedAttrRefKind::SourceLabel(l)
            | CoercedAttrRefKind::ConfigurationDep(l)
            | CoercedAttrRefKind::SplitTransitionDep(l) => Ok(to_value(l.to_string())?),
            CoercedAttrRefKind::PluginDep(t) => Ok(to_value(t.to_string())?),
            CoercedAttrRefKind::SourceFile(p) => {
                Ok(to_value(source_file_display(ctx, p).to_string())?)
            }
            CoercedAttrRefKind::Legacy(a) => a.to_json(ctx),
        }
    }

    /// Whether any leaf's string form matches the filter. Mirrors
    /// `CoercedAttr::any_matches` arm for arm; `Legacy` delegates to it.
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        match self.kind() {
            CoercedAttrRefKind::None => Ok(false),
            CoercedAttrRefKind::Bool(b) => BoolLiteral(b).any_matches(filter),
            CoercedAttrRefKind::Int(i) => filter(&i.to_string()),
            CoercedAttrRefKind::String(s)
            | CoercedAttrRefKind::EnumVariant(s)
            | CoercedAttrRefKind::ArgSimple(s) => filter(s),
            CoercedAttrRefKind::List(items)
            | CoercedAttrRefKind::Tuple(items)
            | CoercedAttrRefKind::Concat(items) => {
                for v in items.iter() {
                    if v.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            CoercedAttrRefKind::Dict(pairs) => {
                for (k, v) in pairs.iter() {
                    if k.any_matches(filter)? || v.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            CoercedAttrRefKind::Selector { entries, default } => {
                for (_, v) in entries.iter() {
                    if v.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                if let Some(default) = default {
                    if default.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            CoercedAttrRefKind::OneOf(l, _) => l.any_matches(filter),
            CoercedAttrRefKind::Dep(l)
            | CoercedAttrRefKind::Label(l)
            | CoercedAttrRefKind::SourceLabel(l)
            | CoercedAttrRefKind::ConfigurationDep(l)
            | CoercedAttrRefKind::SplitTransitionDep(l) => filter(&l.to_string()),
            CoercedAttrRefKind::PluginDep(t) => filter(&t.to_string()),
            CoercedAttrRefKind::SourceFile(p) => filter(&p.path().to_string()),
            CoercedAttrRefKind::Legacy(a) => a.any_matches(filter),
        }
    }

    /// Hash the value's content with `CoercedAttr`'s `Hash` semantics
    /// (target hashing). Bridged through the owned form.
    pub fn hash_coerced<H: Hasher>(&self, state: &mut H) {
        self.to_coerced()
            .expect("should decode: encoded from a valid attribute")
            .hash(state);
    }

    /// The configuration-dep label, or `None` for an explicit `None` value.
    ///
    /// Errors on any other kind: only for attributes the coercer has already
    /// verified to be a (non-configurable) configuration dep.
    pub fn unpack_configuration_dep(&self) -> buck2_error::Result<Option<&'a ProvidersLabel>> {
        match self.kind() {
            CoercedAttrRefKind::None => Ok(None),
            CoercedAttrRefKind::ConfigurationDep(l) => Ok(Some(l)),
            _ => Err(internal_error!(
                "should be a configuration dep: verified by the attr coercer"
            )),
        }
    }

    /// The list elements, or `None` when the value is not a list.
    pub fn unpack_list(&self) -> Option<impl Iterator<Item = CoercedAttrRef<'a>> + use<'a>> {
        match self.kind() {
            CoercedAttrRefKind::List(items) => Some(items.iter()),
            _ => None,
        }
    }

    /// The string content, or `None` when the value is not a string.
    pub fn unpack_str(&self) -> Option<&'a str> {
        match self.kind() {
            CoercedAttrRefKind::String(s) => Some(s),
            _ => None,
        }
    }
}

/// `as_display` for owned [`CoercedAttrRef`]s: the standard adapter borrows its
/// value, which cannot outlive a closure mapping `Copy` views.
struct AttrRefDisplay<'a, 'c> {
    ctx: &'c AttrFmtContext,
    r: CoercedAttrRef<'a>,
}

impl Display for AttrRefDisplay<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        AttrDisplayWithContext::fmt(&self.r, self.ctx, f)
    }
}

/// Mirrors `AttrDisplayWithContext for CoercedAttr` arm for arm; `Legacy`
/// delegates to it.
impl crate::attrs::display::AttrDisplayWithContext for CoercedAttrRef<'_> {
    fn fmt(
        &self,
        ctx: &crate::attrs::fmt_context::AttrFmtContext,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self.kind() {
            CoercedAttrRefKind::None => write!(f, "None"),
            CoercedAttrRefKind::Bool(b) => write!(f, "{}", BoolLiteral(b)),
            CoercedAttrRefKind::Int(i) => write!(f, "{i}"),
            CoercedAttrRefKind::String(s) | CoercedAttrRefKind::EnumVariant(s) => {
                if ctx.options.exclude_quotes {
                    f.write_str(s)
                } else {
                    write!(f, "\"{s}\"")
                }
            }
            CoercedAttrRefKind::ArgSimple(s) => write!(f, "\"{s}\""),
            CoercedAttrRefKind::List(items) => fmt_container(
                f,
                "[",
                "]",
                items.iter().map(|v| AttrRefDisplay { ctx, r: v }),
            ),
            CoercedAttrRefKind::Tuple(items) => fmt_container(
                f,
                "(",
                ")",
                items.iter().map(|v| AttrRefDisplay { ctx, r: v }),
            ),
            CoercedAttrRefKind::Dict(pairs) => fmt_keyed_container(
                f,
                "{",
                "}",
                ": ",
                pairs
                    .iter()
                    .map(|(k, v)| (AttrRefDisplay { ctx, r: k }, AttrRefDisplay { ctx, r: v })),
            ),
            CoercedAttrRefKind::Selector { entries, default } => {
                write!(f, "select(")?;
                fmt_keyed_container(
                    f,
                    "{",
                    "}",
                    ": ",
                    entries
                        .iter()
                        .map(|(k, v)| (format!("\"{k}\""), AttrRefDisplay { ctx, r: v }))
                        .chain(
                            default
                                .map(|d| ("\"DEFAULT\"".to_owned(), AttrRefDisplay { ctx, r: d })),
                        ),
                )?;
                write!(f, ")")
            }
            CoercedAttrRefKind::Concat(items) => items
                .iter()
                .map(|a| AttrRefDisplay { ctx, r: a })
                .format("+")
                .fmt(f),
            CoercedAttrRefKind::OneOf(l, _) => AttrDisplayWithContext::fmt(&l, ctx, f),
            CoercedAttrRefKind::Dep(l)
            | CoercedAttrRefKind::Label(l)
            | CoercedAttrRefKind::SourceLabel(l)
            | CoercedAttrRefKind::ConfigurationDep(l) => write!(f, "\"{l}\""),
            CoercedAttrRefKind::SplitTransitionDep(l) => std::fmt::Display::fmt(l, f),
            CoercedAttrRefKind::PluginDep(t) => write!(f, "\"{t}\""),
            CoercedAttrRefKind::SourceFile(p) => write!(f, "\"{}\"", source_file_display(ctx, p)),
            CoercedAttrRefKind::Legacy(a) => AttrDisplayWithContext::fmt(a, ctx, f),
        }
    }
}

impl std::fmt::Debug for CoercedAttrRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.to_coerced() {
            Ok(a) => write!(f, "CoercedAttrRef({a:?})"),
            Err(_) => write!(f, "CoercedAttrRef(<decode error>)"),
        }
    }
}

/// Builds an [`CoercedAttrArena`], deduplicating structurally equal values and
/// side-table payloads.
///
/// Records are encoded bottom-up; every value gets a content hash computed
/// Merkle-style from its immediate content plus its children's content hashes.
/// Dedup keys off that hash and verifies against the already-encoded record
/// (head bytes compared directly, refs by the value they resolve to;
/// O(immediate value)) — no owned copies of attribute trees are kept
/// anywhere.
#[derive(Default, Debug)]
pub struct CoercedAttrArenaBuilder {
    arena: CoercedAttrArena,
    /// Records by content hash. Keys ARE hashes, so the table is a raw
    /// `HashTable` probed with the hash directly -- no rehash of the key.
    /// Inline leaves never enter (their words are already canonical),
    /// except `Legacy`, whose table index is what the probe finds.
    value_dedup: hashbrown::HashTable<(u64, AttrWord)>,
    /// Text records by full byte hash; raw `HashTable`, hash used directly.
    /// One record per (kind, content): `String`/`EnumVariant`/`ArgSimple`
    /// of equal text are distinct records. (A cheap len/head/tail key was
    /// tried and collided pathologically on fbcode's mid-string-differing
    /// generated names.)
    str_dedup: hashbrown::HashTable<(u64, AttrWord)>,
    /// Interned `TargetLabel` address -> (providers-label table index, label
    /// content hash), for default-providers labels. `TargetLabel`s are
    /// globally interned, so pointer equality holds exactly for them.
    label_by_ptr: BuckMutMap<StaticPointerIdentity<TargetLabel>, (u32, u64)>,
    /// Non-default providers names only: `ProvidersLabel` itself is not
    /// interned, so equal labels can be distinct allocations.
    providers_label_dedup: BuckMutMap<ProvidersLabel, u32>,
    /// Keyed by pointer identity: `TargetLabel`s are globally interned,
    /// so equal labels share one allocation.
    target_label_dedup: BuckMutMap<StaticPointerIdentity<TargetLabel>, u32>,
    /// Words interned as roots (top-level attribute values); the sealed
    /// arena retains an identity-grade strong hash for exactly these.
    roots: Vec<AttrWord>,
}

/// The builder's result for one value: a ref plus the content hash a parent
/// folds into its own.
fn token((r, h): (AttrWord, CoercedAttrHash)) -> CoercedAttr {
    CoercedAttr::Encoded(r, h)
}

fn parts(attr: CoercedAttr) -> (AttrWord, CoercedAttrHash) {
    match attr {
        CoercedAttr::Encoded(r, h) => (r, h),
        _ => unreachable!("should be encoded: the builder only produces `Encoded` values"),
    }
}

fn content_hash(tag: AttrKind, f: impl FnOnce(&mut BuckHasher)) -> CoercedAttrHash {
    let mut h = BuckHasher::default();
    Hash::hash(&(tag as u8), &mut h);
    f(&mut h);
    CoercedAttrHash(h.finish())
}

/// Fold a precomputed component hash under a kind tag: one multiply-rotate
/// mix (splitmix-style) instead of a full hasher instantiation.
#[inline]
fn mix_kind(kind: AttrKind, h: u64) -> CoercedAttrHash {
    let x = h ^ (kind as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    let x = (x ^ (x >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    let x = (x ^ (x >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    CoercedAttrHash(x ^ (x >> 31))
}

/// Content hashes of the payload-free kinds, computed once.
static FIXED_KIND_HASHES: std::sync::LazyLock<[CoercedAttrHash; 3]> =
    std::sync::LazyLock::new(|| {
        [
            mix_kind(AttrKind::None, 0),
            mix_kind(AttrKind::BoolFalse, 0),
            mix_kind(AttrKind::BoolTrue, 0),
        ]
    });

/// Child refs of one record while encoding: covers values with up to 32
/// children on the stack — beyond typical select/list/dict fan-out.
type EncodeScratchRefs = SmallVec<[AttrWord; 32]>;

/// Child content hashes gathered alongside [`EncodeScratchRefs`]; same
/// sizing rationale.
type EncodeScratchHashes = SmallVec<[CoercedAttrHash; 32]>;

/// Smallest width (1-4 bytes) holding `v`.
#[inline]
fn width_for(v: u32) -> u8 {
    if v < 1 << 8 {
        1
    } else if v < 1 << 16 {
        2
    } else if v < 1 << 24 {
        3
    } else {
        4
    }
}

impl CoercedAttrArenaBuilder {
    /// View an already-encoded value while still building (reads between
    /// appends; the borrow prevents concurrent mutation).
    pub fn view(&self, r: AttrWord) -> CoercedAttrRef<'_> {
        self.arena.view(r)
    }

    pub fn finish(self) -> CoercedAttrArena {
        let mut arena = self.arena;
        arena.bytes.shrink_to_fit();
        arena.providers_labels.shrink_to_fit();
        arena.target_labels.shrink_to_fit();
        arena.paths.shrink_to_fit();
        arena.legacy.shrink_to_fit();

        // Strong hashes are computed at seal, from content the arena
        // already holds: the intern hot path never touches blake3, and a
        // value shared by many roots is walked once via the memo.
        let mut roots: Vec<u32> = self.roots.iter().map(|r| r.0).collect();
        roots.sort_unstable();
        roots.dedup();
        let mut memo: BuckMutMap<u32, StrongHash128> = BuckMutMap::default();
        arena.root_strong = roots
            .iter()
            .map(|&w| arena.strong_of(AttrWord(w), &mut memo))
            .collect();
        arena.root_words = roots;
        arena
    }

    /// Content-keyed dedup, for non-default providers names only; the
    /// default-providers path dedups by interned-target pointer.
    fn providers_label_idx(&mut self, l: &ProvidersLabel) -> u32 {
        if let Some(&i) = self.providers_label_dedup.get(l) {
            return i;
        }
        let i = self.push_providers_label(l);
        self.providers_label_dedup.insert(l.dupe(), i);
        i
    }

    fn push_providers_label(&mut self, l: &ProvidersLabel) -> u32 {
        let i = self.arena.providers_labels.len() as u32;
        self.arena.providers_labels.push(l.dupe());
        i
    }

    fn target_label_idx(&mut self, l: TargetLabel) -> u32 {
        let ptr = l.pointer_identity();
        if let Some(&i) = self.target_label_dedup.get(&ptr) {
            return i;
        }
        let i = self.arena.target_labels.len() as u32;
        self.arena.target_labels.push(l);
        self.target_label_dedup.insert(ptr, i);
        i
    }

    /// Whether the record at `r` is `kind` with head `fields`, text `text`,
    /// and refs that resolve to `refs`. Refs are compared by target since
    /// their widths and deltas depend on placement.
    fn record_matches(
        &self,
        r: AttrWord,
        kind: AttrKind,
        fields: &[u32],
        text: &[u8],
        refs: &[AttrWord],
    ) -> bool {
        let rec = self.arena.open(r);
        if rec.kind != kind || rec.head.len() as usize != fields.len() {
            return false;
        }
        if !rec.head.iter_raw().eq(fields.iter().copied()) {
            return false;
        }
        match rec.tail {
            Tail::None => refs.is_empty(),
            Tail::Text(got) => got == text,
            Tail::Refs(run) => {
                run.len() as usize == refs.len()
                    && refs
                        .iter()
                        .enumerate()
                        .all(|(i, &want)| match run.get_opt(i as u32) {
                            Some(got) => got == want,
                            None => want == ABSENT_REF,
                        })
            }
        }
    }

    /// Append one record: tag, head fields, text, then refs, every field
    /// and ref in the smallest width that fits the largest of them.
    fn append_record(
        &mut self,
        kind: AttrKind,
        fields: &[u32],
        text: &[u8],
        refs: &[AttrWord],
    ) -> buck2_error::Result<AttrWord> {
        let off = self.arena.bytes.len();
        let rec = off as u32;
        let max_field = fields
            .iter()
            .copied()
            .chain(refs.iter().map(|&r| r.to_field(rec)))
            .max()
            .unwrap_or(0);
        let width = width_for(max_field);
        let end = off + 1 + (fields.len() + refs.len()) * width as usize + text.len();
        // Record offsets are 31 bits: an encoder-bug backstop at 2 GiB per
        // package, deliberately not a user-input limit.
        const CAPACITY: usize = INLINE_BIT as usize;
        if end > CAPACITY {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Package attribute storage exceeds 2GiB of encoded attribute content; split the package or reduce attribute sizes"
            ));
        }
        // One warning per arena, on the crossing allocation: offsets are
        // monotone, so the threshold is crossed exactly once.
        const CAPACITY_WARN: usize = CAPACITY / 2;
        if off < CAPACITY_WARN && end >= CAPACITY_WARN {
            let _unused = soft_error!(
                "attr_arena_near_capacity",
                buck2_error!(
                    buck2_error::ErrorTag::Environment,
                    "Attr arena buffer past 50% of its 2GiB capacity ({end} bytes)"
                ),
                quiet: true
            );
        }
        self.arena
            .bytes
            .push(kind as u8 | ((width - 1) << TAG_WIDTH_SHIFT));
        let w = width as usize;
        for &f in fields {
            self.arena.bytes.extend_from_slice(&f.to_le_bytes()[..w]);
        }
        self.arena.bytes.extend_from_slice(text);
        for &r in refs {
            self.arena
                .bytes
                .extend_from_slice(&r.to_field(rec).to_le_bytes()[..w]);
        }
        Ok(AttrWord::record(rec))
    }

    /// Dedup-or-append a record: reuse an existing value when one with the
    /// same content hash has the same head and refs (children are deduped
    /// before parents, so equal content implies equal child refs within
    /// this arena).
    fn encode_record(
        &mut self,
        kind: AttrKind,
        hash: CoercedAttrHash,
        fields: &[u32],
        refs: &[AttrWord],
    ) -> buck2_error::Result<AttrWord> {
        if let Some(&(_, r)) = self.value_dedup.find(hash.0, |&(h, r)| {
            h == hash.0 && self.record_matches(r, kind, fields, &[], refs)
        }) {
            return Ok(r);
        }
        let r = self.append_record(kind, fields, &[], refs)?;
        self.value_dedup
            .insert_unique(hash.0, (hash.0, r), |&(h, _)| h);
        Ok(r)
    }

    /// [`Self::encode`] for a root: a top-level attribute value referenced
    /// from outside the arena. The sealed arena retains an identity-grade
    /// strong hash for every root (`CoercedAttrArena::root_strong_hash`).
    pub fn encode_root(
        &mut self,
        attr: &CoercedAttr,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        let (r, h) = self.encode(attr)?;
        self.roots.push(r);
        Ok((r, h))
    }

    /// Encode one value straight from its parts (children may already be
    /// `Encoded`), so the enum form never materializes for arena-bound
    /// values. The result is the value's `Encoded` token; [`Self::encode`]
    /// and [`Self::encode_root`] are the whole-tree imports and hand back
    /// the ref and hash themselves.
    pub fn encode_string(&mut self, s: &str) -> buck2_error::Result<CoercedAttr> {
        self.encode_str(AttrKind::String, s).map(token)
    }

    pub fn encode_list(&mut self, items: &[CoercedAttr]) -> buck2_error::Result<CoercedAttr> {
        self.encode_seq(AttrKind::List, items).map(token)
    }

    pub fn encode_tuple(&mut self, items: &[CoercedAttr]) -> buck2_error::Result<CoercedAttr> {
        self.encode_seq(AttrKind::Tuple, items).map(token)
    }

    pub fn encode_dict(
        &mut self,
        pairs: &[(CoercedAttr, CoercedAttr)],
    ) -> buck2_error::Result<CoercedAttr> {
        let mut refs = EncodeScratchRefs::with_capacity(pairs.len() * 2);
        let mut hashes = EncodeScratchHashes::with_capacity(pairs.len() * 2);
        for (k, v) in pairs.iter() {
            let (kr, kh) = self.encode(k)?;
            let (vr, vh) = self.encode(v)?;
            refs.push(kr);
            refs.push(vr);
            hashes.push(kh);
            hashes.push(vh);
        }
        let hash = content_hash(AttrKind::Dict, |h| hashes.hash(h));
        let r = self.encode_record(AttrKind::Dict, hash, &[pairs.len() as u32], &refs)?;
        Ok(CoercedAttr::Encoded(r, hash))
    }

    pub fn encode_one_of(
        &mut self,
        idx: u32,
        inner: &CoercedAttr,
    ) -> buck2_error::Result<CoercedAttr> {
        let (cr, ch) = self.encode(inner)?;
        let hash = mix_kind(AttrKind::OneOf, ch.0 ^ ((idx as u64) << 32));
        let r = self.encode_record(AttrKind::OneOf, hash, &[idx], &[cr])?;
        Ok(CoercedAttr::Encoded(r, hash))
    }

    pub fn encode_selector(
        &mut self,
        entries: &[(ConfigurationSettingKey, CoercedAttr)],
        default: Option<&CoercedAttr>,
    ) -> buck2_error::Result<CoercedAttr> {
        CoercedSelector::check_all_keys_unique(entries)?;
        self.encode_selector_parts(entries, default).map(token)
    }

    fn encode_selector_parts(
        &mut self,
        entries: &[(ConfigurationSettingKey, CoercedAttr)],
        default: Option<&CoercedAttr>,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        let (default_ref, default_hash) = match default {
            Some(d) => {
                let (r, h) = self.encode(d)?;
                (r, Some(h))
            }
            None => (ABSENT_REF, None),
        };
        let mut refs = EncodeScratchRefs::with_capacity(1 + 2 * entries.len());
        refs.push(default_ref);
        let mut hasher_parts = EncodeScratchHashes::new();
        let mut hash = BuckHasher::default();
        Hash::hash(&(AttrKind::Selector as u8), &mut hash);
        default_hash.hash(&mut hash);
        for (key, v) in entries.iter() {
            let (key_idx, key_hash) = self.providers_label_idx_and_hash(&key.0);
            let (vr, vh) = self.encode(v)?;
            refs.push(AttrWord::inline(AttrKind::Label, key_idx)?);
            refs.push(vr);
            key_hash.hash(&mut hash);
            hasher_parts.push(vh);
        }
        hasher_parts.hash(&mut hash);
        let hash = CoercedAttrHash(hash.finish());
        let r = self.encode_record(AttrKind::Selector, hash, &[entries.len() as u32], &refs)?;
        Ok((r, hash))
    }

    pub fn encode_none(&mut self) -> buck2_error::Result<CoercedAttr> {
        Ok(CoercedAttr::Encoded(
            AttrWord::inline(AttrKind::None, 0)?,
            FIXED_KIND_HASHES[0],
        ))
    }

    pub fn encode_bool(&mut self, b: bool) -> buck2_error::Result<CoercedAttr> {
        let (kind, h) = if b {
            (AttrKind::BoolTrue, FIXED_KIND_HASHES[2])
        } else {
            (AttrKind::BoolFalse, FIXED_KIND_HASHES[1])
        };
        Ok(CoercedAttr::Encoded(AttrWord::inline(kind, 0)?, h))
    }

    pub fn encode_int(&mut self, i: i64) -> buck2_error::Result<CoercedAttr> {
        // Inline and record ints share a hash tag: same value, same hash,
        // whichever encoding it lands in.
        let hash = mix_kind(AttrKind::Int, i as u64);
        if (INT_INLINE_MIN..=INT_INLINE_MAX).contains(&i) {
            return Ok(CoercedAttr::Encoded(
                AttrWord::inline(AttrKind::Int, (i as u32) & INLINE_PAYLOAD_MASK)?,
                hash,
            ));
        }
        let z = zigzag(i);
        let r = self.encode_record(AttrKind::Int, hash, &[z as u32, (z >> 32) as u32], &[])?;
        Ok(CoercedAttr::Encoded(r, hash))
    }

    pub fn encode_enum_variant(&mut self, s: &str) -> buck2_error::Result<CoercedAttr> {
        self.encode_str(AttrKind::EnumVariant, s).map(token)
    }

    pub fn encode_dep(&mut self, l: &ProvidersLabel) -> buck2_error::Result<CoercedAttr> {
        self.encode_providers_label(AttrKind::Dep, l).map(token)
    }

    pub fn encode_label(&mut self, l: &ProvidersLabel) -> buck2_error::Result<CoercedAttr> {
        self.encode_providers_label(AttrKind::Label, l).map(token)
    }

    pub fn encode_source_label(&mut self, l: &ProvidersLabel) -> buck2_error::Result<CoercedAttr> {
        self.encode_providers_label(AttrKind::SourceLabel, l)
            .map(token)
    }

    pub fn encode_configuration_dep(
        &mut self,
        l: &ProvidersLabel,
    ) -> buck2_error::Result<CoercedAttr> {
        self.encode_providers_label(AttrKind::ConfigurationDep, l)
            .map(token)
    }

    pub fn encode_split_transition_dep(
        &mut self,
        l: &ProvidersLabel,
    ) -> buck2_error::Result<CoercedAttr> {
        self.encode_providers_label(AttrKind::SplitTransitionDep, l)
            .map(token)
    }

    pub fn encode_plugin_dep(&mut self, l: &TargetLabel) -> buck2_error::Result<CoercedAttr> {
        let idx = self.target_label_idx(l.dupe());
        let hash = content_hash(AttrKind::PluginDep, |h| l.hash(h));
        Ok(CoercedAttr::Encoded(
            AttrWord::inline(AttrKind::PluginDep, idx)?,
            hash,
        ))
    }

    pub fn encode_source_file(&mut self, p: &CoercedPath) -> buck2_error::Result<CoercedAttr> {
        let idx = self.arena.paths.len() as u32;
        self.arena.paths.push(p.clone());
        let hash = content_hash(AttrKind::SourceFile, |h| p.hash(h));
        Ok(CoercedAttr::Encoded(
            AttrWord::inline(AttrKind::SourceFile, idx)?,
            hash,
        ))
    }

    /// Encode an `arg()` value: plain text natively, args carrying macros
    /// whole into the legacy table.
    pub fn encode_arg(
        &mut self,
        arg: &StringWithMacros<ProvidersLabel>,
    ) -> buck2_error::Result<CoercedAttr> {
        match arg {
            StringWithMacros::StringPart(part) => {
                self.encode_str(AttrKind::ArgSimple, part).map(token)
            }
            StringWithMacros::ManyParts(_) => self.encode_legacy(&CoercedAttr::Arg(arg.clone())),
        }
    }

    /// Intern a whole-value variant into the legacy table.
    pub fn encode_legacy(&mut self, legacy: &CoercedAttr) -> buck2_error::Result<CoercedAttr> {
        let hash = content_hash(AttrKind::Legacy, |h| legacy.hash(h));
        if let Some(&(_, r)) = self.value_dedup.find(hash.0, |&(h, r)| {
            h == hash.0
                && matches!(
                    r.inline_parts(),
                    Some((AttrKind::Legacy, idx)) if &self.arena.legacy[idx as usize] == legacy
                )
        }) {
            return Ok(CoercedAttr::Encoded(r, hash));
        }
        let idx = self.arena.legacy.len() as u32;
        self.arena.legacy.push(legacy.clone());
        let r = AttrWord::inline(AttrKind::Legacy, idx)?;
        self.value_dedup
            .insert_unique(hash.0, (hash.0, r), |&(h, _)| h);
        Ok(CoercedAttr::Encoded(r, hash))
    }

    /// Encode one attribute value (recursively) into the arena. Returns the
    /// ref and the value's content hash.
    pub fn encode(
        &mut self,
        attr: &CoercedAttr,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        match attr {
            // Already encoded during coercion.
            CoercedAttr::Encoded(r, h) => Ok((*r, *h)),
            CoercedAttr::None => self.encode_none().map(parts),
            CoercedAttr::Bool(BoolLiteral(b)) => self.encode_bool(*b).map(parts),
            CoercedAttr::Int(i) => self.encode_int(*i).map(parts),
            CoercedAttr::String(StringLiteral(s)) => self.encode_str(AttrKind::String, s),
            CoercedAttr::EnumVariant(StringLiteral(s)) => self.encode_str(AttrKind::EnumVariant, s),
            CoercedAttr::List(ListLiteral(xs)) => self.encode_seq(AttrKind::List, xs),
            CoercedAttr::Tuple(TupleLiteral(xs)) => self.encode_seq(AttrKind::Tuple, xs),
            CoercedAttr::Concat(CoercedConcat(xs)) => self.encode_seq(AttrKind::Concat, xs),
            CoercedAttr::Dict(DictLiteral(pairs)) => self.encode_dict(pairs).map(parts),
            CoercedAttr::Selector(s) => {
                // Keys were validated unique when the selector was built.
                self.encode_selector_parts(&s.entries, s.default.as_ref())
            }
            CoercedAttr::OneOf(child, idx) => self.encode_one_of(*idx, child).map(parts),
            CoercedAttr::Dep(l) => self.encode_providers_label(AttrKind::Dep, l),
            CoercedAttr::Label(l) => self.encode_providers_label(AttrKind::Label, l),
            CoercedAttr::SourceLabel(l) => self.encode_providers_label(AttrKind::SourceLabel, l),
            CoercedAttr::ConfigurationDep(l) => {
                self.encode_providers_label(AttrKind::ConfigurationDep, l)
            }
            CoercedAttr::SplitTransitionDep(l) => {
                self.encode_providers_label(AttrKind::SplitTransitionDep, l)
            }
            CoercedAttr::PluginDep(l) => self.encode_plugin_dep(l).map(parts),
            CoercedAttr::SourceFile(p) => self.encode_source_file(p).map(parts),
            CoercedAttr::Arg(arg) => self.encode_arg(arg).map(parts),
            legacy @ (CoercedAttr::SelectFail(_)
            | CoercedAttr::SelectIncompatible(_)
            | CoercedAttr::Visibility(_)
            | CoercedAttr::WithinView(_)
            | CoercedAttr::ExplicitConfiguredDep(_)
            | CoercedAttr::TransitionDep(_)
            | CoercedAttr::Query(_)
            | CoercedAttr::Metadata(_)
            | CoercedAttr::TargetModifiers(_)) => self.encode_legacy(legacy).map(parts),
        }
    }

    /// Table index and content hash for a label. Default-providers labels
    /// (the common case) key by the interned target label's address:
    /// pointer equality implies label equality, and the hash is computed
    /// once per unique label.
    fn providers_label_idx_and_hash(&mut self, l: &ProvidersLabel) -> (u32, u64) {
        if matches!(
            l.name(),
            buck2_core::provider::label::ProvidersName::Default
        ) {
            let ptr = l.target().pointer_identity();
            if let Some(&(idx, label_hash)) = self.label_by_ptr.get(&ptr) {
                return (idx, label_hash);
            }
            let idx = self.push_providers_label(l);
            let mut lh = BuckHasher::default();
            l.hash(&mut lh);
            let label_hash = lh.finish();
            self.label_by_ptr.insert(ptr, (idx, label_hash));
            return (idx, label_hash);
        }
        let idx = self.providers_label_idx(l);
        let mut lh = BuckHasher::default();
        l.hash(&mut lh);
        (idx, lh.finish())
    }

    fn encode_providers_label(
        &mut self,
        kind: AttrKind,
        l: &ProvidersLabel,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        let (idx, label_hash) = self.providers_label_idx_and_hash(l);
        Ok((AttrWord::inline(kind, idx)?, mix_kind(kind, label_hash)))
    }

    fn encode_str(
        &mut self,
        kind: AttrKind,
        s: &str,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        let bytes = s.as_bytes();
        let mut byte_hasher = BuckHasher::default();
        bytes.hash(&mut byte_hasher);
        let byte_hash = byte_hasher.finish();
        // Content hash is kind-tagged over the byte hash, so
        // String("foo") != EnumVariant("foo").
        let hash = mix_kind(kind, byte_hash);

        if let Some(&(_, r)) = self.str_dedup.find(byte_hash, |&(h, r)| {
            h == byte_hash && self.arena.kind_of(r) == kind && self.arena.bytes_at(r) == bytes
        }) {
            return Ok((r, hash));
        }
        let r = self.append_record(kind, &[bytes.len() as u32], bytes, &[])?;
        self.str_dedup
            .insert_unique(byte_hash, (byte_hash, r), |&(h, _)| h);
        Ok((r, hash))
    }

    fn encode_seq(
        &mut self,
        kind: AttrKind,
        children: &[CoercedAttr],
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
        let mut refs = EncodeScratchRefs::with_capacity(children.len());
        let mut hashes = EncodeScratchHashes::with_capacity(children.len());
        for c in children {
            let (r, h) = self.encode(c)?;
            refs.push(r);
            hashes.push(h);
        }
        let hash = content_hash(kind, |h| hashes.hash(h));
        let r = self.encode_record(kind, hash, &[children.len() as u32], &refs)?;
        Ok((r, hash))
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::provider::label::testing::ProvidersLabelTestExt;

    use super::*;
    use crate::attrs::attr_type::string::StringLiteral;
    use crate::attrs::display::AttrDisplayWithContextExt;

    fn label(pkg: &str, name: &str) -> ProvidersLabel {
        ProvidersLabel::testing_new("root", pkg, name, None)
    }

    fn roundtrip(attrs: &[CoercedAttr]) -> CoercedAttrArena {
        let mut b = CoercedAttrArena::builder();
        let refs: Vec<AttrWord> = attrs.iter().map(|a| b.encode(a).unwrap().0).collect();
        let arena = b.finish();
        for (a, r) in attrs.iter().zip(&refs) {
            let view = arena.view(*r);
            let back = view.to_coerced().unwrap();
            assert_eq!(a, &back, "round-trip should preserve the value");
            output_parity(a, view);
        }
        arena
    }

    /// The native output paths must render exactly what the enum's do.
    fn output_parity(a: &CoercedAttr, view: CoercedAttrRef<'_>) {
        let ctx = &crate::attrs::fmt_context::AttrFmtContext::NO_CONTEXT;
        assert_eq!(
            a.as_display(ctx).to_string(),
            view.as_display(ctx).to_string(),
            "display should match the enum rendering"
        );
        assert_eq!(
            a.to_json(ctx).unwrap(),
            view.to_json(ctx).unwrap(),
            "JSON should match the enum rendering"
        );
        for probe in ["a", ":", "x", "1", "no-such-string"] {
            let filter = |s: &str| Ok(s.contains(probe));
            assert_eq!(
                a.any_matches(&filter).unwrap(),
                view.any_matches(&filter).unwrap(),
                "any_matches({probe:?}) should match the enum result"
            );
        }
    }

    fn s(v: &str) -> CoercedAttr {
        CoercedAttr::String(StringLiteral(ArcStr::from(v)))
    }

    #[test]
    fn test_page_in_validates_text_records() {
        use pagable::testing::TestingDeserializer;
        use pagable::testing::TestingSerializer;

        let mut b = CoercedAttrArena::builder();
        let r = b.encode(&s("héllo")).unwrap().0;
        let arena = b.finish();

        let mut ser = TestingSerializer::new();
        arena.pagable_serialize(&mut ser).unwrap();
        let blob = ser.finish();
        let back =
            CoercedAttrArena::pagable_deserialize(&mut TestingDeserializer::new(&blob)).unwrap();
        assert_eq!(back.str_at(r), "héllo");

        let text_off = arena.bytes_at(r).as_ptr() as usize - arena.bytes.as_ptr() as usize;
        let mut corrupt = arena.bytes.0.clone();
        corrupt[text_off + 1] = 0xFF;
        let mut ser = TestingSerializer::new();
        RecordBytes(corrupt).pagable_serialize(&mut ser).unwrap();
        let blob = ser.finish();
        let err = RecordBytes::pagable_deserialize(&mut TestingDeserializer::new(&blob))
            .expect_err("corrupt text must be rejected");
        assert!(err.to_string().contains("not UTF-8"), "{err}");
    }

    #[test]
    fn test_page_in_validates_ref_targets() {
        use pagable::testing::TestingDeserializer;
        use pagable::testing::TestingSerializer;

        let mut b = CoercedAttrArena::builder();
        let r = b
            .encode(&CoercedAttr::List(ListLiteral(vec![s("héllo")].into())))
            .unwrap()
            .0;
        let arena = b.finish();

        // The list record's single ref field follows its tag byte and one
        // head field; point it further back than the buffer start.
        let list_off = (r.0 & OFFSET_MASK) as usize;
        let width = (arena.bytes[list_off] >> TAG_WIDTH_SHIFT) as usize + 1;
        let mut corrupt = arena.bytes.0.clone();
        corrupt[list_off + 1 + width] = 0xFE;
        let mut ser = TestingSerializer::new();
        RecordBytes(corrupt).pagable_serialize(&mut ser).unwrap();
        let blob = ser.finish();
        let err = RecordBytes::pagable_deserialize(&mut TestingDeserializer::new(&blob))
            .expect_err("dangling ref must be rejected");
        assert!(err.to_string().contains("not an earlier record"), "{err}");
    }

    #[test]
    fn test_immediates_roundtrip_and_cost_no_bytes() {
        let arena = roundtrip(&[
            CoercedAttr::None,
            CoercedAttr::Bool(BoolLiteral(true)),
            CoercedAttr::Bool(BoolLiteral(false)),
            CoercedAttr::Int(0),
            CoercedAttr::Int(-1),
            CoercedAttr::Int(INT_INLINE_MAX),
            CoercedAttr::Int(INT_INLINE_MIN),
            CoercedAttr::Dep(label("foo", "bar")),
        ]);
        assert_eq!(
            arena.byte_len(),
            0,
            "inline leaves should use no arena bytes"
        );
    }

    #[test]
    fn test_root_strong_hash_identity() {
        let list = CoercedAttr::List(ListLiteral(ArcSlice::new([
            s("x"),
            CoercedAttr::Int(3),
            CoercedAttr::Dep(label("foo", "bar")),
        ])));
        let other = CoercedAttr::List(ListLiteral(ArcSlice::new([s("y")])));

        // Same value in two arenas, interleaved with different neighbors:
        // strong hashes must agree (identity is content, not placement).
        let mut b1 = CoercedAttrArena::builder();
        let r1 = b1.encode_root(&list).unwrap().0;
        let a1 = b1.finish();

        let mut b2 = CoercedAttrArena::builder();
        let o2 = b2.encode_root(&other).unwrap().0;
        let r2 = b2.encode_root(&list).unwrap().0;
        let a2 = b2.finish();

        assert_eq!(
            a1.root_strong_hash(r1).unwrap(),
            a2.root_strong_hash(r2).unwrap(),
            "equal content should have equal strong hashes across arenas"
        );
        assert_ne!(
            a2.root_strong_hash(o2).unwrap(),
            a2.root_strong_hash(r2).unwrap(),
            "different content should have different strong hashes"
        );

        // Non-roots retain no strong hash.
        let mut b3 = CoercedAttrArena::builder();
        let interior = b3.encode(&list).unwrap().0;
        let a3 = b3.finish();
        assert_eq!(
            a3.root_strong_hash(interior),
            None,
            "non-root values should not retain strong hashes"
        );

        // OwnedCoercedAttr agrees with a package arena holding the same value.
        let sealed = OwnedCoercedAttr::try_new(&list).unwrap();
        assert_eq!(
            sealed.strong_hash(),
            a1.root_strong_hash(r1).unwrap(),
            "sealed storage should produce the same identity hash"
        );
    }

    #[test]
    fn test_big_ints() {
        let arena = roundtrip(&[
            CoercedAttr::Int(INT_INLINE_MAX + 1),
            CoercedAttr::Int(INT_INLINE_MIN - 1),
            CoercedAttr::Int(i64::MAX),
            CoercedAttr::Int(i64::MIN),
        ]);
        // Each is a tag plus the two 32-bit halves of the zigzag value, at
        // four bytes each once the low half needs them.
        assert_eq!(arena.byte_len(), 4 * (1 + 2 * 4));
    }

    #[test]
    fn test_strings() {
        roundtrip(&[
            s(""),
            s("a"),
            s("abcd"),
            s("abcde"),
            s("unicode: \u{1F600} end"),
        ]);
    }

    #[test]
    fn test_containers() {
        let list = CoercedAttr::List(ListLiteral(ArcSlice::new([
            s("x"),
            CoercedAttr::Int(3),
            CoercedAttr::None,
        ])));
        let tuple = CoercedAttr::Tuple(TupleLiteral(ArcSlice::new([
            list.clone(),
            CoercedAttr::Bool(BoolLiteral(false)),
        ])));
        let dict = CoercedAttr::Dict(DictLiteral(ArcSlice::new([
            (s("k1"), list.clone()),
            (s("k2"), tuple.clone()),
        ])));
        let oneof = CoercedAttr::OneOf(Box::new(dict.clone()), 7);
        roundtrip(&[list, tuple, dict, oneof]);
    }

    #[test]
    fn test_labels_and_selector() {
        let dep = CoercedAttr::Dep(label("foo", "bar"));
        let sel = CoercedAttr::Selector(Arc::new(
            CoercedSelector::new(
                ArcSlice::new([
                    (ConfigurationSettingKey(label("cfg", "a")), s("on-a")),
                    (ConfigurationSettingKey(label("cfg", "b")), dep.clone()),
                ]),
                Some(s("dflt")),
            )
            .unwrap(),
        ));
        let sel_no_default = CoercedAttr::Selector(Arc::new(
            CoercedSelector::new(
                ArcSlice::new([(ConfigurationSettingKey(label("cfg", "a")), s("v"))]),
                None,
            )
            .unwrap(),
        ));
        let concat = CoercedAttr::Concat(CoercedConcat(ArcSlice::new([sel.clone(), s("suffix")])));
        roundtrip(&[dep, sel, sel_no_default, concat]);
    }

    #[test]
    fn test_legacy_variants() {
        roundtrip(&[
            CoercedAttr::SelectFail(ArcStr::from("nope")),
            CoercedAttr::Metadata(Default::default()),
        ]);
    }

    #[test]
    fn test_arg_simple_native_and_many_parts_legacy() {
        let simple = CoercedAttr::Arg(StringWithMacros::StringPart(ArcStr::from("-DFOO=1")));
        let many = CoercedAttr::Arg(StringWithMacros::ManyParts(Box::new([])));
        let arena = roundtrip(&[simple, many]);
        assert_eq!(
            arena.legacy.len(),
            1,
            "only the ManyParts arg should ride the legacy table"
        );
    }

    #[test]
    fn test_dedup_shares_nodes_and_labels() {
        let shared = CoercedAttr::List(ListLiteral(ArcSlice::new([
            s("shared"),
            CoercedAttr::Dep(label("foo", "bar")),
        ])));
        let mut b = CoercedAttrArena::builder();
        let r1 = b.encode(&shared).unwrap().0;
        let bytes_after = b.arena.bytes.len();
        let r2 = b.encode(&shared.clone()).unwrap().0;
        assert_eq!(r1, r2, "structurally equal values should dedup to one ref");
        assert_eq!(
            bytes_after,
            b.arena.bytes.len(),
            "dedup hit should allocate nothing"
        );
        let r3 = b
            .encode(&CoercedAttr::Label(label("foo", "bar")))
            .unwrap()
            .0;
        // A different kind over the same label is an inline leaf sharing
        // the table entry: no bytes.
        assert_eq!(b.arena.bytes.len(), bytes_after);
        let arena = b.finish();
        assert_eq!(
            arena.providers_labels.len(),
            1,
            "same label via Dep and Label should share one table entry"
        );
        // Distinct-kind refs to the same label index must stay distinct.
        assert_ne!(r1, r3);
    }
}
