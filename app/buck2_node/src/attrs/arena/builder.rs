/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::hash::Hash;
use std::hash::Hasher;

use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_hash::BuckHasher;
use buck2_hash::BuckMutMap;
use buck2_util::arc_str::ArcS;
use buck2_util::pointer_identity::StaticPointerIdentity;
use buck2_util::pointer_identity::mix_addr;
use buck2_util::strong_hasher::StrongHash128;
use dupe::Dupe;
use smallvec::SmallVec;

use crate::attrs::arena::fields::RecordWriter;
use crate::attrs::arena::schema::AttrKind;
use crate::attrs::arena::schema::CopyParts;
use crate::attrs::arena::schema::Storage;
use crate::attrs::arena::schema::dispatch;
use crate::attrs::arena::storage::AttrToken;
use crate::attrs::arena::storage::AttrWord;
use crate::attrs::arena::storage::CoercedAttrArena;
use crate::attrs::arena::storage::CoercedAttrHash;
use crate::attrs::arena::storage::INLINE_PAYLOAD_MASK;
use crate::attrs::arena::storage::INT_INLINE_MAX;
use crate::attrs::arena::storage::INT_INLINE_MIN;
use crate::attrs::arena::storage::zigzag;
use crate::attrs::arena::view::CoercedAttrRef;
use crate::attrs::coerced_path::CoercedPath;

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
    pub(super) arena: CoercedAttrArena,
    /// Records by content hash. Keys ARE hashes, so the table is a raw
    /// `HashTable` probed with the hash directly -- no rehash of the key.
    /// Inline leaves never enter (their words are already canonical),
    /// except the whole-value kinds, whose table index is what the probe
    /// finds.
    pub(super) value_dedup: hashbrown::HashTable<(u64, AttrWord)>,
    /// Text records by full byte hash; raw `HashTable`, hash used directly.
    /// One record per (kind, content): `String`/`EnumVariant`/`ArgSimple`
    /// of equal text are distinct records. (A cheap len/head/tail key was
    /// tried and collided pathologically on fbcode's mid-string-differing
    /// generated names.)
    pub(super) str_dedup: hashbrown::HashTable<(u64, AttrWord)>,
    /// Interned `TargetLabel` address -> (providers-label table index, label
    /// content hash), for default-providers labels. `TargetLabel`s are
    /// globally interned, so pointer equality holds exactly for them.
    pub(super) label_by_ptr: BuckMutMap<StaticPointerIdentity<TargetLabel>, (u32, u64)>,
    /// Non-default providers names only: `ProvidersLabel` itself is not
    /// interned, so equal labels can be distinct allocations.
    pub(super) providers_label_dedup: BuckMutMap<ProvidersLabel, u32>,
    /// Keyed by pointer identity: `TargetLabel`s are globally interned,
    /// so equal labels share one allocation.
    pub(super) target_label_dedup: BuckMutMap<StaticPointerIdentity<TargetLabel>, u32>,
    /// Source-file lookaside, probed by data address: (retained path, path
    /// table index, path content hash). Every path here comes from coercion
    /// — source attrs and `$(source ...)` arg macros alike — which resolves
    /// files against the package listing and hands out the listing's own
    /// `ArcS`. The same file referenced from either form therefore shares
    /// one allocation, and pointer equality implies path equality. Each
    /// entry retains its `ArcS`, so a keyed address can never be freed — or
    /// reused — while the map lives. Files absent from the listing
    /// (missing-file soft error) carry fresh allocations and never hit.
    /// Directories are excluded: their value includes the expanded file
    /// list, so the dir pointer alone does not imply value equality.
    pub(super) path_by_ptr: hashbrown::HashTable<(ArcS<PackageRelativePath>, u32, u64)>,
    /// Whole-value tables' content dedup: value hash to table index.
    pub(super) visibility_dedup: hashbrown::HashTable<(u64, u32)>,
    pub(super) within_view_dedup: hashbrown::HashTable<(u64, u32)>,
    pub(super) modifiers_dedup: hashbrown::HashTable<(u64, u32)>,
    pub(super) metadata_dedup: hashbrown::HashTable<(u64, u32)>,
    /// Words interned as roots (top-level attribute values); the sealed
    /// arena retains an identity-grade strong hash for exactly these.
    pub(super) roots: Vec<AttrWord>,
}

pub(super) fn content_hash(tag: AttrKind, f: impl FnOnce(&mut BuckHasher)) -> CoercedAttrHash {
    let mut h = BuckHasher::default();
    Hash::hash(&(tag as u8), &mut h);
    f(&mut h);
    CoercedAttrHash(h.finish())
}

/// Bucket hash for the source-file lookaside: mixes the `ArcS` data
/// address (identity, not content).
pub(super) fn path_addr_hash(f: &ArcS<PackageRelativePath>) -> u64 {
    mix_addr(f.as_str().as_ptr().addr())
}

/// Fold a precomputed component hash under a kind tag: one multiply-rotate
/// mix (splitmix-style) instead of a full hasher instantiation.
#[inline]
pub(super) fn mix_kind(kind: AttrKind, h: u64) -> CoercedAttrHash {
    let x = h ^ (kind as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);
    let x = (x ^ (x >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    let x = (x ^ (x >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    CoercedAttrHash(x ^ (x >> 31))
}

/// Head fields of one record while encoding; `SelectorKeys` and
/// `QueryExpr` heads can run long, everything else is one to three fields.
pub(super) type EncodeScratchFields = SmallVec<[u32; 32]>;

/// Child refs gathered alongside [`EncodeScratchFields`]: covers values with up
/// to 32 children on the stack — beyond typical select/list/dict fan-out.
pub(super) type EncodeScratchRefs = SmallVec<[AttrWord; 32]>;

impl CoercedAttrArenaBuilder {
    /// View an already-encoded value while still building (reads between
    /// appends; the borrow prevents concurrent mutation).
    pub fn view(&self, r: AttrWord) -> CoercedAttrRef<'_> {
        self.arena.view(r)
    }

    pub fn finish(self) -> CoercedAttrArena {
        let mut arena = self.arena;

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
        arena.shrink_to_fit();
        arena
    }

    /// Content-keyed dedup, for non-default providers names only; the
    /// default-providers path dedups by interned-target pointer.
    pub(super) fn providers_label_idx(&mut self, l: &ProvidersLabel) -> u32 {
        if let Some(&i) = self.providers_label_dedup.get(l) {
            return i;
        }
        let i = self.arena.push_providers_label(l);
        self.providers_label_dedup.insert(l.dupe(), i);
        i
    }

    pub(super) fn target_label_idx(&mut self, l: TargetLabel) -> u32 {
        let ptr = l.pointer_identity();
        if let Some(&i) = self.target_label_dedup.get(&ptr) {
            return i;
        }
        let i = self.arena.push_target_label(l);
        self.target_label_dedup.insert(ptr, i);
        i
    }

    /// Dedup-or-append a record: reuse an existing value when one with the
    /// same content hash has the same head and refs (children are deduped
    /// before parents, so equal content implies equal child refs within
    /// this arena).
    pub(super) fn encode_record(
        &mut self,
        kind: AttrKind,
        hash: CoercedAttrHash,
        fields: &[u32],
        refs: &[AttrWord],
    ) -> buck2_error::Result<AttrWord> {
        if let Some(&(_, r)) = self.value_dedup.find(hash.0, |&(h, r)| {
            h == hash.0 && self.arena.record_matches(r, kind, fields, &[], refs)
        }) {
            return Ok(r);
        }
        let r = self.arena.append_record(kind, fields, &[], refs)?;
        self.value_dedup
            .insert_unique(hash.0, (hash.0, r), |&(h, _)| h);
        Ok(r)
    }

    /// Mark an encoded value as a root: a top-level attribute value referenced
    /// from outside the arena, for which the sealed arena retains an
    /// identity-grade strong hash.
    pub fn root(&mut self, token: AttrToken) -> AttrWord {
        self.roots.push(token.0);
        token.0
    }

    /// Writes one gathered value per its kind's storage class.
    pub(super) fn emit(
        &mut self,
        storage: Storage,
        w: RecordWriter<'_>,
    ) -> buck2_error::Result<AttrToken> {
        match storage {
            Storage::Text => {
                let text = w
                    .text
                    .expect("should have text: text storage has a `Text` field");
                self.encode_str(w.kind, text)
            }
            Storage::Inline => {
                let payload = w.head.first().copied().unwrap_or(0);
                Ok(AttrToken(
                    AttrWord::inline(w.kind, payload)?,
                    CoercedAttrHash(w.h.finish()),
                ))
            }
            Storage::Record => {
                let hash = CoercedAttrHash(w.h.finish());
                let r = self.encode_record(w.kind, hash, &w.head, &w.refs)?;
                Ok(AttrToken(r, hash))
            }
        }
    }

    /// The shared `SelectorKeys` record of a selector's key set.
    pub(super) fn encode_selector_keys<'k>(
        &mut self,
        keys: impl ExactSizeIterator<Item = &'k ProvidersLabel>,
    ) -> buck2_error::Result<AttrToken> {
        let mut key_idxs = EncodeScratchFields::with_capacity(1 + keys.len());
        key_idxs.push(keys.len() as u32);
        let mut keys_hasher = BuckHasher::default();
        Hash::hash(&(AttrKind::SelectorKeys as u8), &mut keys_hasher);
        for key in keys {
            let (key_idx, key_hash) = self.providers_label_idx_and_hash(key);
            key_idxs.push(key_idx);
            key_hash.hash(&mut keys_hasher);
        }
        let keys_hash = CoercedAttrHash(keys_hasher.finish());
        let r = self.encode_record(AttrKind::SelectorKeys, keys_hash, &key_idxs, &[])?;
        Ok(AttrToken(r, keys_hash))
    }

    pub fn encode_int(&mut self, i: i64) -> buck2_error::Result<AttrToken> {
        // Inline and record ints share a hash tag: same value, same hash,
        // whichever encoding it lands in.
        let hash = mix_kind(AttrKind::Int, i as u64);
        if (INT_INLINE_MIN..=INT_INLINE_MAX).contains(&i) {
            return Ok(AttrToken(
                AttrWord::inline(AttrKind::Int, (i as u32) & INLINE_PAYLOAD_MASK)?,
                hash,
            ));
        }
        let z = zigzag(i);
        let r = self.encode_record(AttrKind::Int, hash, &[z as u32, (z >> 32) as u32], &[])?;
        Ok(AttrToken(r, hash))
    }

    /// Copy a value out of another arena into this one, node by node through
    /// the per-kind encoders: the token this returns has the same content and
    /// identity hash as `v`, deduped against what this arena already holds.
    pub fn encode_view(&mut self, v: CoercedAttrRef<'_>) -> buck2_error::Result<AttrToken> {
        dispatch(v.arena.kind_of(v.r), CopyParts { v, b: self })
    }

    /// Path table index and content hash, via the `ArcS` pointer lookaside
    /// for files (see `path_by_ptr`).
    pub(super) fn source_path_idx_and_hash(&mut self, p: &CoercedPath) -> (u32, u64) {
        let file = match p {
            CoercedPath::File(f) => {
                let bucket = path_addr_hash(f);
                if let Some((idx, hash)) = self
                    .path_by_ptr
                    .find(bucket, |(a, _, _)| {
                        std::ptr::eq(a.as_str().as_ptr(), f.as_str().as_ptr())
                    })
                    .map(|&(_, idx, hash)| (idx, hash))
                {
                    return (idx, hash);
                }
                Some((f.dupe(), bucket))
            }
            CoercedPath::Directory(_) => None,
        };
        let idx = self.arena.push_path(p.clone());
        let mut hasher = BuckHasher::default();
        p.hash(&mut hasher);
        let hash = hasher.finish();
        if let Some((f, bucket)) = file {
            self.path_by_ptr
                .insert_unique(bucket, (f, idx, hash), |(a, _, _)| path_addr_hash(a));
        }
        (idx, hash)
    }

    /// Table index and content hash for a label. Default-providers labels
    /// (the common case) key by the interned target label's address:
    /// pointer equality implies label equality, and the hash is computed
    /// once per unique label.
    pub(super) fn providers_label_idx_and_hash(&mut self, l: &ProvidersLabel) -> (u32, u64) {
        if matches!(
            l.name(),
            buck2_core::provider::label::ProvidersName::Default
        ) {
            let ptr = l.target().pointer_identity();
            if let Some(&(idx, label_hash)) = self.label_by_ptr.get(&ptr) {
                return (idx, label_hash);
            }
            let idx = self.arena.push_providers_label(l);
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

    pub(super) fn encode_str(&mut self, kind: AttrKind, s: &str) -> buck2_error::Result<AttrToken> {
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
            return Ok(AttrToken(r, hash));
        }
        let r = self
            .arena
            .append_record(kind, &[bytes.len() as u32], bytes, &[])?;
        self.str_dedup
            .insert_unique(byte_hash, (byte_hash, r), |&(h, _)| h);
        Ok(AttrToken(r, hash))
    }
}
