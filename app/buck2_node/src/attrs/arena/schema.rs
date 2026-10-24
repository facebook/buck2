/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The kinds table and the per-kind structure it derives: record shape,
//! decoding, copying, identity hashing and equality.

use std::hash::Hasher;

use buck2_core::provider::label::ProvidersLabel;
use strong_hash::StrongHash;

use crate::attrs::arena::builder::CoercedAttrArenaBuilder;
use crate::attrs::arena::fields::Cursor;
use crate::attrs::arena::fields::FieldList;
use crate::attrs::arena::fields::IdHasher;
use crate::attrs::arena::fields::RecordWriter;
use crate::attrs::arena::storage::AttrToken;
use crate::attrs::arena::storage::AttrWord;
use crate::attrs::arena::storage::CoercedAttrArena;
use crate::attrs::arena::storage::INLINE_KIND_MASK;
use crate::attrs::arena::storage::RefRun;
use crate::attrs::arena::storage::TailLen;
use crate::attrs::arena::view::CoercedAttrRef;
use crate::attrs::coerced::kinds;
use crate::attrs::coerced::kinds::CoercedAttrKind;
use crate::attrs::coerced::kinds::dispatch;

/// Defines everything that follows from the list of kinds as a whole. The
/// invocation names each item it defines, and the per-kind trait the value
/// visitor is bounded on; `unit` kinds have no view,
/// `value` kinds' view-enum cases hold the kind's [`Kind::View`], and
/// `internal` kinds are records inside other records, never attribute
/// values.
macro_rules! arena_kinds {
    (
        $(#[$tag_doc:meta])* enum $tag:ident;
        $(#[$view_doc:meta])* enum $view:ident<$lt:lifetime>;
        impl $kind_tag:ident;
        fn $dispatch:ident;
        fn $dispatch_value:ident;
        fn $decode:ident;
        trait $visitor:ident: $semantics:ident;
        unit { $( $(#[$udoc:meta])* $u:ident = $utag:literal, )* }
        value { $( $(#[$vdoc:meta])* $v:ident = $vtag:literal, )* }
        internal { $( $(#[$idoc:meta])* $i:ident = $itag:literal, )* }
    ) => {
        $(#[$tag_doc])*
        #[derive(Copy, Clone, PartialEq, Eq, Debug, ::strum::FromRepr)]
        #[repr(u8)]
        pub(crate) enum $tag {
            $( $(#[$udoc])* $u = $utag, )*
            $( $(#[$vdoc])* $v = $vtag, )*
            $( $(#[$idoc])* $i = $itag, )*
        }

        $( pub(crate) struct $u; )*
        $( pub(crate) struct $v; )*
        $( pub(crate) struct $i; )*

        $( impl $kind_tag for $u { const KIND: $tag = $tag::$u; } )*
        $( impl $kind_tag for $v { const KIND: $tag = $tag::$v; } )*
        $( impl $kind_tag for $i { const KIND: $tag = $tag::$i; } )*

        $(#[$view_doc])*
        // Each case's type is a kind's view, reached through the crate-private
        // kind type and trait; what a consumer gets is the concrete type.
        #[allow(private_interfaces)]
        pub enum $view<$lt> {
            $( $u, )*
            $( $v(<$v as $crate::attrs::arena::schema::Kind>::View<$lt>), )*
        }

        /// Runs `v` for the [`Kind`] type of `kind`.
        pub(crate) fn $dispatch<V: KindVisitor>(kind: $tag, v: V) -> V::Out {
            match kind {
                $( $tag::$u => v.visit::<$u>(), )*
                $( $tag::$v => v.visit::<$v>(), )*
                $( $tag::$i => v.visit::<$i>(), )*
            }
        }

        /// A visitor over the attribute value kinds, each of which implements
        /// `$semantics`; `visit` receives the value's decoded view.
        pub(crate) trait $visitor<$lt> {
            type Out;
            fn visit<K: $semantics + 'static>(
                self,
                view: <K as $crate::attrs::arena::schema::Kind>::View<$lt>,
            ) -> Self::Out;
        }

        /// Decodes `v` and runs `op` for its kind, which must be an attribute
        /// value kind.
        pub(crate) fn $dispatch_value<$lt, V: $visitor<$lt>>(
            v: $crate::attrs::arena::CoercedAttrRef<$lt>,
            op: V,
        ) -> V::Out {
            match v.arena.kind_of(v.r) {
                $( $tag::$u => op.visit::<$u>(<$u as $crate::attrs::arena::schema::Kind>::decode(v.arena, v.r)), )*
                $( $tag::$v => op.visit::<$v>(<$v as $crate::attrs::arena::schema::Kind>::decode(v.arena, v.r)), )*
                $( $tag::$i => unreachable!("not an attribute value: {:?}", $tag::$i), )*
            }
        }

        /// The decoded view of the value at `r`.
        pub(crate) fn $decode<$lt>(
            arena: &$lt $crate::attrs::arena::CoercedAttrArena,
            r: $crate::attrs::arena::AttrWord,
        ) -> $view<$lt> {
            match arena.kind_of(r) {
                $( $tag::$u => $view::$u, )*
                $( $tag::$v => $view::$v(<$v as $crate::attrs::arena::schema::Kind>::decode(arena, r)), )*
                $( $tag::$i => unreachable!("not an attribute value: {:?}", $tag::$i), )*
            }
        }
    };
}
pub(crate) use arena_kinds;

/// The tag of a kind, from its row in the kinds list.
pub(crate) trait KindTag {
    const KIND: CoercedAttrKind;
}

impl CoercedAttrKind {
    /// Whether values of this kind are inline leaves (payload in the
    /// word) rather than records in the buffer.
    pub(super) fn is_inline(self) -> bool {
        dispatch(self, IsInline)
    }

    /// Kinds whose values hold no deps, inputs, or labels: a traversal can
    /// pass over them without decoding. Select outcomes are scalar for the
    /// unconfigured walk only; the configured walk must report them.
    pub(crate) fn is_scalar(self) -> bool {
        dispatch(self, IsScalar)
    }
}

/// How a kind's values are stored.
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum Storage {
    /// No record: the word carries the kind and the single field's value.
    Inline,
    /// A record of head fields and child refs, deduped by content.
    Record,
    /// A text record, deduped by bytes.
    Text,
}

pub(crate) const fn storage_class(
    kind: CoercedAttrKind,
    has_text: bool,
    head: u32,
    has_refs: bool,
) -> Storage {
    if has_text {
        Storage::Text
    } else if !has_refs && head <= 1 && (kind as u32) <= INLINE_KIND_MASK {
        Storage::Inline
    } else {
        Storage::Record
    }
}

/// A kind whose values are a sequence of typed fields: everything about it
/// derives from the field list.
pub(crate) trait SchemaKind: KindTag + 'static {
    type Fields: FieldList;
    /// How values are stored follows from the fields and the tag: a text
    /// tail makes a text record; no tail and at most one head field fit the
    /// word, if the tag fits the word's kind bits; anything else is a
    /// record.
    const STORAGE: Storage = storage_class(
        Self::KIND,
        <Self::Fields as FieldList>::HAS_TEXT,
        <Self::Fields as FieldList>::HEAD,
        <Self::Fields as FieldList>::HAS_REFS,
    );
}

/// One record kind as a type. [`dispatch`] selects it from a value's tag
/// and monomorphizes a visitor per kind, so every operation that is uniform
/// over kinds is written once. Schema kinds get this from their fields;
/// the record-internal tagged unions implement it by hand.
pub(crate) trait Kind: KindTag {
    const STORAGE: Storage;
    /// Whether values hold no deps: no labels, paths or children, so
    /// traversals have nothing to visit.
    const SCALAR: bool;
    /// The decoded value: leaves as borrowed side-table values or
    /// immediates, children as refs.
    type View<'a>: Copy;
    /// Head field count and tail of a record, reading head fields through
    /// `peek`; `None` when a field the layout depends on is missing.
    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)>;
    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> Self::View<'a>;
    /// Re-encode into `b`, children first.
    fn copy_into(
        parts: Self::View<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken>;
    /// Identity hash of the parts; the kind byte is already written.
    fn identity(parts: Self::View<'_>, h: &mut IdHasher<'_>);
    fn same(a: Self::View<'_>, b: Self::View<'_>) -> bool;
}

impl<K: SchemaKind> Kind for K {
    const STORAGE: Storage = K::STORAGE;
    const SCALAR: bool = !<K::Fields as FieldList>::DEPS;
    type View<'a> = <K::Fields as FieldList>::View<'a>;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        K::Fields::shape(peek)
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> Self::View<'a> {
        let mut c = Cursor::new(arena, r);
        K::Fields::decode(&mut c)
    }

    fn copy_into(
        parts: Self::View<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken> {
        let mut w = RecordWriter::new(K::KIND);
        K::Fields::reemit(parts, b, &mut w)?;
        b.emit(K::STORAGE, w)
    }

    fn identity(parts: Self::View<'_>, h: &mut IdHasher<'_>) {
        K::Fields::identity(parts, h)
    }

    fn same(a: Self::View<'_>, b: Self::View<'_>) -> bool {
        K::Fields::same(a, b)
    }
}

/// Encode a schema kind's value from its field specs.
pub(crate) fn encode_kind<K: SchemaKind>(
    b: &mut CoercedAttrArenaBuilder,
    spec: <K::Fields as FieldList>::Spec<'_>,
) -> buck2_error::Result<AttrToken> {
    let mut w = RecordWriter::new(K::KIND);
    K::Fields::encode(spec, b, &mut w)?;
    b.emit(K::STORAGE, w)
}

/// Selects the [`Kind`] type for a tag at runtime, monomorphizing the
/// visitor per kind.
pub(crate) trait KindVisitor {
    type Out;
    fn visit<K: Kind + 'static>(self) -> Self::Out;
}

/// `Int`: inline when the value fits the payload, otherwise a record of the
/// zigzag value in 32-bit halves; one kind either way.
impl Kind for kinds::Int {
    const STORAGE: Storage = Storage::Record;
    const SCALAR: bool = true;
    type View<'a> = i64;

    fn shape(_peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        Some((2, TailLen::None))
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> i64 {
        arena.int_at(r)
    }

    fn copy_into(parts: i64, b: &mut CoercedAttrArenaBuilder) -> buck2_error::Result<AttrToken> {
        b.encode_int(parts)
    }

    fn identity(parts: i64, h: &mut IdHasher<'_>) {
        h.h.write_i64(parts);
    }

    fn same(a: i64, b: i64) -> bool {
        a == b
    }
}

/// The label set of a `SelectorKeys` record: `[n, idx...]`.
#[derive(Copy, Clone)]
pub(crate) struct KeysParts<'a> {
    pub(super) keys: RefRun<'a>,
}

impl<'a> KeysParts<'a> {
    pub(super) fn iter(&self) -> impl ExactSizeIterator<Item = &'a ProvidersLabel> + 'a {
        let arena = self.keys.arena;
        self.keys
            .iter_raw()
            .map(move |k| &arena.providers_labels[k as usize])
    }
}

impl Kind for kinds::SelectorKeys {
    const STORAGE: Storage = Storage::Record;
    const SCALAR: bool = false;
    type View<'a> = KeysParts<'a>;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        Some((1u32.checked_add(peek(0)?)?, TailLen::None))
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> KeysParts<'a> {
        KeysParts {
            keys: arena.open(r).head.skip(1),
        }
    }

    fn copy_into(
        parts: KeysParts<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken> {
        b.encode_selector_keys(parts.iter())
    }

    fn identity(parts: KeysParts<'_>, h: &mut IdHasher<'_>) {
        h.h.write_u32(parts.keys.len());
        for k in parts.iter() {
            k.strong_hash(&mut h.h);
        }
    }

    fn same(a: KeysParts<'_>, b: KeysParts<'_>) -> bool {
        a.keys.len() == b.keys.len() && a.iter().zip(b.iter()).all(|(x, y)| x == y)
    }
}

/// [`CoercedAttrKind::is_inline`].
pub(super) struct IsInline;

impl KindVisitor for IsInline {
    type Out = bool;
    fn visit<K: Kind + 'static>(self) -> bool {
        matches!(K::STORAGE, Storage::Inline)
    }
}

/// [`CoercedAttrKind::is_scalar`].
pub(super) struct IsScalar;

impl KindVisitor for IsScalar {
    type Out = bool;
    fn visit<K: Kind + 'static>(self) -> bool {
        K::SCALAR
    }
}

/// Record shape from a kind's fields: [`shape`].
pub(super) struct Shape<'f> {
    pub(super) peek: &'f dyn Fn(u32) -> Option<u32>,
}

impl KindVisitor for Shape<'_> {
    type Out = Option<(u32, TailLen)>;
    fn visit<K: Kind + 'static>(self) -> Self::Out {
        K::shape(self.peek)
    }
}

/// Copies a value into a builder: [`CoercedAttrArenaBuilder::encode_view`].
pub(super) struct CopyParts<'b, 'a> {
    pub(super) v: CoercedAttrRef<'a>,
    pub(super) b: &'b mut CoercedAttrArenaBuilder,
}

impl KindVisitor for CopyParts<'_, '_> {
    type Out = buck2_error::Result<AttrToken>;
    fn visit<K: Kind + 'static>(self) -> Self::Out {
        K::copy_into(K::decode(self.v.arena, self.v.r), self.b)
    }
}

/// Folds a value's parts into the identity hasher: [`CoercedAttrArena::strong_of`].
pub(super) struct HashParts<'h, 'm, 'a> {
    pub(super) v: CoercedAttrRef<'a>,
    pub(super) h: &'h mut IdHasher<'m>,
}

impl KindVisitor for HashParts<'_, '_, '_> {
    type Out = ();
    fn visit<K: Kind + 'static>(self) {
        K::identity(K::decode(self.v.arena, self.v.r), self.h)
    }
}

/// Compares two values of the same kind: [`CoercedAttrRef::content_eq`].
pub(super) struct EqParts<'a, 'b> {
    pub(super) a: CoercedAttrRef<'a>,
    pub(super) b: CoercedAttrRef<'b>,
}

impl KindVisitor for EqParts<'_, '_> {
    type Out = bool;
    fn visit<K: Kind + 'static>(self) -> bool {
        K::same(
            K::decode(self.a.arena, self.a.r),
            K::decode(self.b.arena, self.b.r),
        )
    }
}
