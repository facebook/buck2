/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The record buffer and the words that address it: the byte format the
//! module doc of [`crate::attrs::arena`] describes.

use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::buck2_error;
use buck2_hash::BuckMutMap;
use buck2_util::strong_hasher::Blake3StrongHasher;
use buck2_util::strong_hasher::StrongHash128;
use dupe::Dupe;
use either::Either;
use pagable::Pagable;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use strong_hash::StrongHash;

use crate::attrs::arena::builder::CoercedAttrArenaBuilder;
use crate::attrs::arena::fields::IdHasher;
use crate::attrs::arena::schema::AttrKind;
use crate::attrs::arena::schema::HashParts;
use crate::attrs::arena::schema::Shape;
use crate::attrs::arena::schema::dispatch;
use crate::attrs::arena::view::CoercedAttrRef;
use crate::attrs::coerced_path::CoercedPath;
use crate::metadata::map::MetadataMap;
use crate::modifiers::CfgModifiers;
use crate::visibility::VisibilitySpecification;
use crate::visibility::WithinViewSpecification;

/// Tag byte of a record: `[ width:2 | kind:6 ]`. `width` is the byte width
/// of every head field and ref in the record, minus one.
pub(super) const TAG_KIND_MASK: u8 = 0x3F;
pub(super) const TAG_WIDTH_SHIFT: u8 = 6;

/// The tail of a record body, sized.
#[derive(Copy, Clone)]
pub(crate) enum TailLen {
    None,
    Text(u32),
    Refs(u32),
}

/// Head field count and tail of a `kind` record whose head field `i` reads
/// as `field(i)`; `None` when a field the layout depends on is missing.
#[inline]
pub(super) fn shape(kind: AttrKind, field: impl Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
    dispatch(kind, Shape { peek: &field })
}

/// A `width`-byte little-endian field at `at`, if in bounds.
#[inline]
pub(super) fn field_at(bytes: &[u8], at: usize, width: u8) -> Option<u32> {
    let b = bytes.get(at..at + width as usize)?;
    Some(match width {
        1 => b[0] as u32,
        2 => u16::from_le_bytes([b[0], b[1]]) as u32,
        3 => (b[0] as u32) | ((b[1] as u32) << 8) | ((b[2] as u32) << 16),
        _ => u32::from_le_bytes([b[0], b[1], b[2], b[3]]),
    })
}

#[inline]
pub(super) fn zigzag(i: i64) -> u64 {
    ((i << 1) ^ (i >> 63)) as u64
}

#[inline]
pub(super) fn unzigzag(v: u64) -> i64 {
    ((v >> 1) as i64) ^ -((v & 1) as i64)
}

/// Reference to an attribute value in a [`CoercedAttrArena`]: the byte
/// offset of the value's record. Only meaningful together with the arena
/// that minted it.
#[derive(
    Copy, Clone, Dupe, PartialEq, Eq, Hash, StrongHash, Debug, Allocative, Pagable
)]
pub struct AttrWord(pub(super) u32);

/// Content hash of an encoded attribute value, as computed by
/// [`CoercedAttrArenaBuilder`]: kind-tagged, folded over child hashes, and stable
/// across arenas. Travels alongside an [`AttrWord`] so consumers can hash
/// and prefilter equality without decoding.
#[derive(
    Copy, Clone, Dupe, PartialEq, Eq, Hash, StrongHash, Debug, Allocative, Pagable
)]
pub struct CoercedAttrHash(pub u64);

/// A value encoded into an arena: its ref, and the content hash a parent
/// folds into its own. This is what coercion hands around and what the
/// per-kind encoders return.
#[derive(Copy, Clone, Dupe, PartialEq, Eq, Debug, Allocative)]
pub struct AttrToken(pub AttrWord, pub CoercedAttrHash);

impl AttrToken {
    pub fn word(self) -> AttrWord {
        self.0
    }
}

pub(super) const INLINE_BIT: u32 = 1 << 31;
pub(super) const OFFSET_MASK: u32 = INLINE_BIT - 1;
pub(super) const INLINE_KIND_SHIFT: u32 = 26;
pub(super) const INLINE_PAYLOAD_BITS: u32 = 26;
pub(super) const INLINE_PAYLOAD_MASK: u32 = (1 << INLINE_PAYLOAD_BITS) - 1;

/// Small ints are sign-extended inline payloads.
pub(super) const INT_INLINE_MAX: i64 = (1 << (INLINE_PAYLOAD_BITS - 1)) - 1;
pub(super) const INT_INLINE_MIN: i64 = -(1 << (INLINE_PAYLOAD_BITS - 1));

impl AttrWord {
    /// An inline leaf. Ints that do not fit the payload spill to records, so
    /// overflow means one package holds more distinct values of `kind` than
    /// the payload can index.
    pub(super) fn inline(kind: AttrKind, payload: u32) -> buck2_error::Result<AttrWord> {
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
    pub(super) fn inline_parts(self) -> Option<(AttrKind, u32)> {
        if self.0 & INLINE_BIT == 0 {
            return None;
        }
        let kind = AttrKind::from_repr(((self.0 >> INLINE_KIND_SHIFT) & 0x1F) as u8)
            .expect("should be valid: words are only minted with known kinds");
        Some((kind, self.0 & INLINE_PAYLOAD_MASK))
    }

    /// A record ref to the record at `off`.
    #[inline]
    pub(super) fn record(off: u32) -> AttrWord {
        AttrWord(off)
    }

    /// Byte offset of this record ref's tag.
    #[inline]
    pub(super) fn off(self) -> usize {
        debug_assert!(self.0 & INLINE_BIT == 0, "inline leaves have no record");
        (self.0 & OFFSET_MASK) as usize
    }

    /// The field value that encodes this word inside the record at `rec`.
    #[inline]
    pub(super) fn to_field(self, rec: u32) -> u32 {
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
    pub(super) fn from_field(field: u32, rec: u32) -> Option<AttrWord> {
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
pub(super) const ABSENT_REF: AttrWord = AttrWord(u32::MAX);

/// The record buffer. Reads take its text records as UTF-8 on trust: the
/// encoder only writes `&str` content, and a buffer from anywhere else is
/// validated once, as it pages in.
#[derive(Debug, Default, Allocative)]
pub(super) struct RecordBytes(pub(super) Vec<u8>);

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
pub(super) fn validate_records(bytes: &[u8]) -> Result<(), String> {
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
    pub(super) bytes: RecordBytes,
    /// Globally interned `ProvidersLabel`s, indexed by `Dep`, `Label`,
    /// `SourceLabel`, `ConfigurationDep`, `SplitTransitionDep`, and
    /// `SelectorKeys` records.
    pub(super) providers_labels: Vec<ProvidersLabel>,
    /// Globally interned `TargetLabel`s, indexed by `PluginDep` records.
    pub(super) target_labels: Vec<TargetLabel>,
    /// Source paths, indexed by `SourceFile` records.
    pub(super) paths: Vec<CoercedPath>,
    /// Whole-value leaves by table index, one entry per distinct value in
    /// the package.
    pub(super) visibility: Vec<VisibilitySpecification>,
    pub(super) within_view: Vec<WithinViewSpecification>,
    pub(super) metadata: Vec<MetadataMap>,
    /// `modifiers` values, indexed by `Modifiers` records; each shared with
    /// every holder that outlives the arena.
    pub(super) modifiers: Vec<CfgModifiers>,
    /// Identity-grade content hashes for root values (the values referenced
    /// from outside the arena: one per unique top-level attribute value),
    /// sorted by word for binary search. Read on the value-identity paths:
    /// attr equality for early cutoff when a package re-evaluates, and
    /// target hashing. Interior values carry no retained strong hash;
    /// 128-bit blake3 over full content, so equal hash implies equal value.
    // TODO(jtbraun): fold these parallel vecs into a `starlark_map` `Vec2`
    // once `pagable` has an impl for it.
    pub(super) root_words: Vec<u32>,
    pub(super) root_strong: Vec<StrongHash128>,
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
    /// as a root (`CoercedAttrArenaBuilder::root`).
    pub fn root_strong_hash(&self, r: AttrWord) -> Option<StrongHash128> {
        self.root_words
            .binary_search(&r.0)
            .ok()
            .map(|i| self.root_strong[i])
    }

    /// The record's kind and the byte width of its refs. Record refs only.
    #[inline]
    pub(super) fn tag(&self, r: AttrWord) -> (AttrKind, u8) {
        let t = self.bytes[r.off()];
        let kind = AttrKind::from_repr(t & TAG_KIND_MASK)
            .expect("should be valid: records are only written by the encoder");
        (kind, (t >> TAG_WIDTH_SHIFT) + 1)
    }

    #[inline]
    pub(super) fn kind_of(&self, r: AttrWord) -> AttrKind {
        match r.inline_parts() {
            Some((kind, _)) => kind,
            None => self.tag(r).0,
        }
    }

    /// The record at `r`, located per its kind's layout. Record refs only.
    #[inline]
    pub(super) fn open(&self, r: AttrWord) -> Rec<'_> {
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

    /// Identity-grade content hash of one value: blake3 over the value's kind
    /// and its parts, children by their own identity hashes. Equal content
    /// yields equal hashes across arenas and processes.
    pub(super) fn strong_of(
        &self,
        r: AttrWord,
        memo: &mut BuckMutMap<u32, StrongHash128>,
    ) -> StrongHash128 {
        if let Some(&h) = memo.get(&r.0) {
            return h;
        }
        let kind = self.kind_of(r);
        let mut h = IdHasher {
            h: Blake3StrongHasher::new(),
            memo,
        };
        h.h.write_u8(kind as u8);
        dispatch(
            kind,
            HashParts {
                v: self.view(r),
                h: &mut h,
            },
        );
        let hash = h.h.finalize128();
        memo.insert(r.0, hash);
        hash
    }

    /// The value of an `Int`, inline or record.
    pub(super) fn int_at(&self, r: AttrWord) -> i64 {
        match r.inline_parts() {
            // Sign-extend the 26-bit payload.
            Some((_, p)) => {
                let shifted = (p << (32 - INLINE_PAYLOAD_BITS)) as i32;
                (shifted >> (32 - INLINE_PAYLOAD_BITS)) as i64
            }
            None => {
                let rec = self.open(r);
                unzigzag(rec.head.raw(0) as u64 | ((rec.head.raw(1) as u64) << 32))
            }
        }
    }

    /// Raw bytes of a text record, without UTF-8 validation: the string-dedup
    /// equality probe needs only byte equality.
    pub(super) fn bytes_at(&self, r: AttrWord) -> &[u8] {
        match self.open(r).tail {
            Tail::Text(bytes) => bytes,
            _ => unreachable!("should be text: only text records are read as bytes"),
        }
    }

    pub(super) fn str_at(&self, r: AttrWord) -> &str {
        let bytes = self.bytes_at(r);
        debug_assert!(std::str::from_utf8(bytes).is_ok());
        // SAFETY: text records are written from `&str`, and a buffer from
        // outside this process is validated when it pages in (`RecordBytes`).
        unsafe { std::str::from_utf8_unchecked(bytes) }
    }
}

/// One located record: its head fields and tail.
#[derive(Copy, Clone)]
pub(super) struct Rec<'a> {
    pub(super) kind: AttrKind,
    pub(super) head: RefRun<'a>,
    pub(super) tail: Tail<'a>,
}

#[derive(Copy, Clone)]
pub(super) enum Tail<'a> {
    None,
    Text(&'a [u8]),
    Refs(RefRun<'a>),
}

impl<'a> Rec<'a> {
    /// The ref tail of a record whose layout has one.
    #[inline]
    pub(super) fn refs(&self) -> RefRun<'a> {
        match self.tail {
            Tail::Refs(run) => run,
            _ => unreachable!("should have refs: the kind's layout says so"),
        }
    }
}

/// A run of fixed-width values inside one record: ref fields (see
/// [`AttrWord::to_field`]) or raw indices (`SelectorKeys`).
#[derive(Copy, Clone)]
pub(super) struct RefRun<'a> {
    pub(super) arena: &'a CoercedAttrArena,
    pub(super) rec: u32,
    pub(super) start: usize,
    pub(super) len: u32,
    pub(super) width: u8,
}

impl<'a> RefRun<'a> {
    /// The `i`th fixed-width value, little-endian.
    #[inline]
    pub(super) fn raw(&self, i: u32) -> u32 {
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
    pub(super) fn get(&self, i: u32) -> AttrWord {
        self.get_opt(i)
            .expect("should be present: only a selector default may be absent")
    }

    #[inline]
    pub(super) fn get_opt(&self, i: u32) -> Option<AttrWord> {
        AttrWord::from_field(self.raw(i), self.rec)
    }

    pub(super) fn len(&self) -> u32 {
        self.len
    }

    /// The run without its first `n` refs.
    pub(super) fn skip(&self, n: u32) -> RefRun<'a> {
        RefRun {
            start: self.start + n as usize * self.width as usize,
            len: self.len - n,
            ..*self
        }
    }

    /// The raw values in order. One bounds check for the whole run; the
    /// width is matched once so each arm is a fixed-stride loop.
    pub(super) fn iter_raw(&self) -> impl ExactSizeIterator<Item = u32> + use<'a> {
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

    pub(super) fn iter(&self) -> impl ExactSizeIterator<Item = AttrWord> + use<'a> {
        let rec = self.rec;
        self.iter_raw().map(move |f| {
            AttrWord::from_field(f, rec)
                .expect("should be present: only a selector default may be absent")
        })
    }
}
