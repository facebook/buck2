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
use buck2_core::target::label::label::TargetLabel;
use strong_hash::StrongHash;
use strum::FromRepr;

use crate::attrs::arena::builder::CoercedAttrArenaBuilder;
use crate::attrs::arena::builder::token;
use crate::attrs::arena::fields::ArgChildren;
use crate::attrs::arena::fields::Child;
use crate::attrs::arena::fields::ChildPairs;
use crate::attrs::arena::fields::Children;
use crate::attrs::arena::fields::Cursor;
use crate::attrs::arena::fields::ExprChild;
use crate::attrs::arena::fields::FieldList;
use crate::attrs::arena::fields::IdHasher;
use crate::attrs::arena::fields::Idx;
use crate::attrs::arena::fields::Label;
use crate::attrs::arena::fields::OptChild;
use crate::attrs::arena::fields::OptLabel;
use crate::attrs::arena::fields::Path;
use crate::attrs::arena::fields::RecordWriter;
use crate::attrs::arena::fields::SelectorEntries;
use crate::attrs::arena::fields::Target;
use crate::attrs::arena::fields::Text;
use crate::attrs::arena::fields::U32;
use crate::attrs::arena::query_expr::QueryView;
use crate::attrs::arena::semantics::Semantics;
use crate::attrs::arena::storage::AttrToken;
use crate::attrs::arena::storage::AttrWord;
use crate::attrs::arena::storage::CoercedAttrArena;
use crate::attrs::arena::storage::CoercedAttrHash;
use crate::attrs::arena::storage::RefRun;
use crate::attrs::arena::storage::TailLen;
use crate::attrs::arena::view::CoercedAttrRef;
use crate::attrs::arena::view::CoercedAttrRefKind;
use crate::attrs::coerced_path::CoercedPath;
use crate::configuration::resolved::ConfigurationSettingKey;
use crate::metadata::map::MetadataMap;
use crate::modifiers::CfgModifiers;
use crate::visibility::VisibilitySpecification;
use crate::visibility::WithinViewSpecification;

/// Every kind once: its tag, its storage class, and its fields in order. A
/// field type says what a head field or the tail *means* (a providers-label
/// table index, a count plus that many child refs, UTF-8 text), so decoding,
/// copying between arenas, identity hashing, content equality and the
/// record's byte layout all derive from this row. `custom` kinds are the
/// tagged unions inside a record (`ArgMacro`, `QueryExpr`), plus `Int` and
/// `SelectorKeys`, which implement [`Kind`] by hand.
macro_rules! arena_kinds {
    (
        $(
            $(#[$doc:meta])*
            $name:ident = $tag:literal : $body:tt ;
        )*
    ) => {
        /// Record kinds.
        #[derive(Copy, Clone, PartialEq, Eq, Debug, FromRepr)]
        #[repr(u8)]
        pub(crate) enum AttrKind {
            $( $(#[$doc])* $name = $tag, )*
        }

        impl AttrKind {
            /// Whether values of this kind are inline leaves (payload in the
            /// word) rather than records in the buffer.
            pub(super) fn is_inline(self) -> bool {
                match self {
                    $( AttrKind::$name => arena_kinds!(@inline $body), )*
                }
            }
        }

        /// Each kind as a type, for [`Kind`] impls and [`dispatch`].
        pub(crate) mod kinds {
            $( pub(crate) struct $name; )*
        }

        /// Runs `v` for the [`Kind`] type of `kind`.
        pub(super) fn dispatch<V: KindVisitor>(kind: AttrKind, v: V) -> V::Out {
            match kind {
                $( AttrKind::$name => v.visit::<kinds::$name>(), )*
            }
        }

        $( arena_kinds!(@impl $name $body); )*
    };
    (@inline { inline $($rest:tt)* }) => { true };
    (@inline { record $($rest:tt)* }) => { false };
    (@inline { text $($rest:tt)* }) => { false };
    (@inline { custom inline }) => { true };
    (@inline { custom record }) => { false };
    (@impl $name:ident { custom $storage:ident }) => {};
    (@impl $name:ident {
        $storage:ident ($($fname:ident : $f:ty = $spec:ty),* $(,)?) $(as $enc:ident)? => |$v:ident| $wrap:expr
    }) => {
        impl SchemaKind for kinds::$name {
            const KIND: AttrKind = AttrKind::$name;
            const STORAGE: Storage = arena_kinds!(@storage $storage);
            type Fields = ($($f,)*);
            #[allow(unused_variables)]
            fn wrap<'a>(
                $v: CoercedAttrRef<'a>,
                ($($fname,)*): <($($f,)*) as FieldList>::View<'a>,
            ) -> CoercedAttrRefKind<'a> {
                $wrap
            }
        }

        arena_kinds!(@encoder $name ($($fname : $spec),*) $($enc)?);
    };
    (@encoder $name:ident ($($fname:ident : $spec:ty),*)) => {};
    (@encoder $name:ident ($($fname:ident : $spec:ty),*) $enc:ident) => {
        impl CoercedAttrArenaBuilder {
            /// Encode one value of this kind from its parts; children are
            /// tokens this builder minted.
            pub fn $enc(
                &mut self,
                $($fname: $spec),*
            ) -> buck2_error::Result<AttrToken> {
                encode_kind::<kinds::$name>(self, ($($fname,)*))
            }
        }
    };
    (@storage inline) => { Storage::Inline };
    (@storage record) => { Storage::Record };
    (@storage text) => { Storage::Text };
}

arena_kinds! {
    None = 0: { inline () as encode_none => |_v| CoercedAttrRefKind::None };
    /// `encode_bool` picks the kind.
    BoolFalse = 1: { inline () => |_v| CoercedAttrRefKind::Bool(false) };
    BoolTrue = 2: { inline () => |_v| CoercedAttrRefKind::Bool(true) };
    /// Inline as a 26-bit sign-extended payload when it fits; otherwise a
    /// record with head `[lo, hi]`: the zigzag-encoded value in 32-bit halves.
    Int = 3: { custom record };
    WithinView = 4: {
        inline (
            v: Idx<WithinViewSpecification> = &WithinViewSpecification,
        ) as encode_within_view
            => |_v| CoercedAttrRefKind::WithinView(v)
    };
    String = 5: { text (s: Text = &str) as encode_string => |_v| CoercedAttrRefKind::String(s) };
    EnumVariant = 6: {
        text (s: Text = &str) as encode_enum_variant
            => |_v| CoercedAttrRefKind::EnumVariant(s)
    };
    List = 7: {
        record (items: Children = &[AttrToken]) as encode_list
            => |_v| CoercedAttrRefKind::List(items)
    };
    Tuple = 8: {
        record (items: Children = &[AttrToken]) as encode_tuple
            => |_v| CoercedAttrRefKind::Tuple(items)
    };
    Dict = 9: {
        record (pairs: ChildPairs = &[(AttrToken, AttrToken)]) as encode_dict
            => |_v| CoercedAttrRefKind::Dict(pairs)
    };
    Selector = 10: {
        record (
            entries: SelectorEntries = &[(ConfigurationSettingKey, AttrToken)],
            default: OptChild = Option<AttrToken>,
        ) as encode_selector
            => |_v| CoercedAttrRefKind::Selector { entries, default }
    };
    Concat = 11: {
        record (items: Children = &[AttrToken]) as encode_concat
            => |_v| CoercedAttrRefKind::Concat(items)
    };
    OneOf = 12: {
        record (
            idx: U32 = u32,
            inner: Child = AttrToken,
        ) as encode_one_of
            => |_v| CoercedAttrRefKind::OneOf(inner, idx)
    };
    Dep = 13: {
        inline (l: Label = &ProvidersLabel) as encode_dep
            => |_v| CoercedAttrRefKind::Dep(l)
    };
    Label = 14: {
        inline (l: Label = &ProvidersLabel) as encode_label
            => |_v| CoercedAttrRefKind::Label(l)
    };
    SourceLabel = 15: {
        inline (l: Label = &ProvidersLabel) as encode_source_label
            => |_v| CoercedAttrRefKind::SourceLabel(l)
    };
    ConfigurationDep = 16: {
        inline (l: Label = &ProvidersLabel) as encode_configuration_dep
            => |_v| CoercedAttrRefKind::ConfigurationDep(l)
    };
    SplitTransitionDep = 17: {
        inline (l: Label = &ProvidersLabel) as encode_split_transition_dep
            => |_v| CoercedAttrRefKind::SplitTransitionDep(l)
    };
    PluginDep = 18: {
        inline (l: Target = &TargetLabel) as encode_plugin_dep
            => |_v| CoercedAttrRefKind::PluginDep(l)
    };
    SourceFile = 19: {
        inline (p: Path = &CoercedPath) as encode_source_file
            => |_v| CoercedAttrRefKind::SourceFile(p)
    };
    Visibility = 20: {
        inline (
            v: Idx<VisibilitySpecification> = &VisibilitySpecification,
        ) as encode_visibility
            => |_v| CoercedAttrRefKind::Visibility(v)
    };
    /// `Arg` with no macros (`StringWithMacros::StringPart`) -- most `Arg`
    /// values in real graphs contain no macros.
    ArgSimple = 21: {
        text (s: Text = &str) as encode_arg_simple
            => |_v| CoercedAttrRefKind::ArgSimple(s)
    };
    /// Head: `[n, idx...]`: `n` providers-label table indices, a selector's
    /// key set. Never appears as an attribute value ref.
    SelectorKeys = 22: { custom record };
    /// Parts are `ArgSimple` or `ArgMacro` records.
    ArgParts = 23: {
        record (parts: ArgChildren = &[AttrToken]) as encode_arg_parts
            => |_v| CoercedAttrRefKind::Arg(parts)
    };
    /// Head: `[header, payload index, n]`; refs: `n` per the header's
    /// variant tag. Never appears as an attribute value ref.
    ArgMacro = 24: { custom record };
    /// Head: `[subtag, nv, nv values, n]`; refs: `n` per `Q_EXPR_*`. Encodes
    /// the parsed form of a query macro's expression; target literals are
    /// label table indices, text is shared text records. Never appears as
    /// an attribute value ref.
    QueryExpr = 25: { custom record };
    /// The parsed form of an `attrs.query()` value.
    Query = 26: {
        record (expr: ExprChild = (AttrWord, CoercedAttrHash)) as encode_parsed_query
            => |v| CoercedAttrRefKind::Query(QueryView { arena: v.arena, r: v.r })
    };
    /// A `select_fail()` / `select_incompatible()` leaf holding the message.
    SelectFail = 27: {
        text (message: Text = &str) as encode_select_fail
            => |_v| CoercedAttrRefKind::SelectFail(message)
    };
    SelectIncompatible = 28: {
        text (message: Text = &str) as encode_select_incompatible
            => |_v| CoercedAttrRefKind::SelectIncompatible(message)
    };
    /// An `attrs.configured_dep()` value.
    ExplicitConfiguredDep = 29: {
        record (
            label: Label = &ProvidersLabel,
            platform: Target = &TargetLabel,
        ) as encode_explicit_configured_dep
            => |_v| CoercedAttrRefKind::ExplicitConfiguredDep { label, platform }
    };
    /// The dynamic transition target is absent when the attr type fixes the
    /// transition.
    TransitionDep = 30: {
        record (
            dep: Label = &ProvidersLabel,
            transition: OptLabel = Option<&ProvidersLabel>,
        ) as encode_transition_dep
            => |_v| CoercedAttrRefKind::TransitionDep { dep, transition }
    };
    Modifiers = 31: {
        inline (m: Idx<CfgModifiers> = &CfgModifiers) as encode_target_modifiers
            => |_v| CoercedAttrRefKind::Modifiers(m)
    };
    /// A record, not an inline leaf: the tag does not fit the inline kind bits.
    Metadata = 32: {
        record (m: Idx<MetadataMap> = &MetadataMap) as encode_metadata
            => |_v| CoercedAttrRefKind::Metadata(m)
    };
}

impl AttrKind {
    /// Kinds whose values hold no deps, inputs, or labels: a traversal can
    /// pass over them without decoding. Select outcomes are scalar for the
    /// unconfigured walk only; the configured walk must report them.
    pub(super) fn is_scalar(self) -> bool {
        dispatch(self, IsScalar)
    }
}

/// How a kind's values are stored.
#[derive(Copy, Clone, PartialEq, Eq)]
pub(super) enum Storage {
    /// No record: the word carries the kind and the single field's value.
    Inline,
    /// A record of head fields and child refs, deduped by content.
    Record,
    /// A text record, deduped by bytes.
    Text,
}

/// A kind whose values are a sequence of typed fields: everything about it
/// derives from the field list.
pub(super) trait SchemaKind: 'static {
    const KIND: AttrKind;
    const STORAGE: Storage;
    type Fields: FieldList;
    /// The public view case for decoded fields.
    fn wrap<'a>(
        v: CoercedAttrRef<'a>,
        parts: <Self::Fields as FieldList>::View<'a>,
    ) -> CoercedAttrRefKind<'a>;
}

/// One record kind as a type. [`dispatch`] selects it from a value's tag
/// and monomorphizes a visitor per kind, so every operation that is uniform
/// over kinds is written once. Schema kinds get this from their fields;
/// the record-internal tagged unions implement it by hand.
pub(super) trait Kind {
    const KIND: AttrKind;
    /// Whether values hold no deps: no labels, paths or children, so
    /// traversals have nothing to visit.
    const SCALAR: bool;
    /// The decoded value: leaves as borrowed side-table values or
    /// immediates, children as refs.
    type Parts<'a>: Copy;
    /// Head field count and tail of a record, reading head fields through
    /// `peek`; `None` when a field the layout depends on is missing.
    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)>;
    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> Self::Parts<'a>;
    /// The public view case. Record kinds that are not attribute values
    /// (`ArgMacro`, `QueryExpr`, `SelectorKeys`) have none.
    fn wrap<'a>(v: CoercedAttrRef<'a>, parts: Self::Parts<'a>) -> CoercedAttrRefKind<'a>
    where
        Self: 'a;
    /// Re-encode into `b`, children first.
    fn copy_into(
        parts: Self::Parts<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken>;
    /// Identity hash of the parts; the kind byte is already written.
    fn identity(parts: Self::Parts<'_>, h: &mut IdHasher<'_>);
    fn same(a: Self::Parts<'_>, b: Self::Parts<'_>) -> bool;
}

impl<K: SchemaKind> Kind for K {
    const KIND: AttrKind = K::KIND;
    const SCALAR: bool = !<K::Fields as FieldList>::DEPS;
    type Parts<'a> = <K::Fields as FieldList>::View<'a>;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        K::Fields::shape(peek)
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> Self::Parts<'a> {
        let mut c = Cursor::new(arena, r);
        K::Fields::decode(&mut c)
    }

    fn wrap<'a>(v: CoercedAttrRef<'a>, parts: Self::Parts<'a>) -> CoercedAttrRefKind<'a>
    where
        Self: 'a,
    {
        K::wrap(v, parts)
    }

    fn copy_into(
        parts: Self::Parts<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken> {
        let mut w = RecordWriter::new(K::KIND);
        K::Fields::reemit(parts, b, &mut w)?;
        b.emit(K::STORAGE, w)
    }

    fn identity(parts: Self::Parts<'_>, h: &mut IdHasher<'_>) {
        K::Fields::identity(parts, h)
    }

    fn same(a: Self::Parts<'_>, b: Self::Parts<'_>) -> bool {
        K::Fields::same(a, b)
    }
}

/// Encode a schema kind's value from its field specs.
pub(super) fn encode_kind<K: SchemaKind>(
    b: &mut CoercedAttrArenaBuilder,
    spec: <K::Fields as FieldList>::Spec<'_>,
) -> buck2_error::Result<AttrToken> {
    let mut w = RecordWriter::new(K::KIND);
    K::Fields::encode(spec, b, &mut w)?;
    b.emit(K::STORAGE, w)
}

/// Selects the [`Kind`] type for a tag at runtime, monomorphizing the
/// visitor per kind.
pub(super) trait KindVisitor {
    type Out;
    fn visit<K: Semantics + 'static>(self) -> Self::Out;
}

/// `Int`: inline when the value fits the payload, otherwise a record of the
/// zigzag value in 32-bit halves; one kind either way.
impl Kind for kinds::Int {
    const KIND: AttrKind = AttrKind::Int;
    const SCALAR: bool = true;
    type Parts<'a> = i64;

    fn shape(_peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        Some((2, TailLen::None))
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> i64 {
        arena.int_at(r)
    }

    fn wrap<'a>(_v: CoercedAttrRef<'a>, parts: i64) -> CoercedAttrRefKind<'a>
    where
        Self: 'a,
    {
        CoercedAttrRefKind::Int(parts)
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
pub(super) struct KeysParts<'a> {
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
    const KIND: AttrKind = AttrKind::SelectorKeys;
    const SCALAR: bool = false;
    type Parts<'a> = KeysParts<'a>;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        Some((1u32.checked_add(peek(0)?)?, TailLen::None))
    }

    fn decode<'a>(arena: &'a CoercedAttrArena, r: AttrWord) -> KeysParts<'a> {
        KeysParts {
            keys: arena.open(r).head.skip(1),
        }
    }

    fn wrap<'a>(_v: CoercedAttrRef<'a>, _parts: KeysParts<'a>) -> CoercedAttrRefKind<'a>
    where
        Self: 'a,
    {
        unreachable!("selector keys records are not attribute values")
    }

    fn copy_into(
        parts: KeysParts<'_>,
        b: &mut CoercedAttrArenaBuilder,
    ) -> buck2_error::Result<AttrToken> {
        b.encode_selector_keys(parts.iter()).map(token)
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

/// [`AttrKind::is_scalar`].
pub(super) struct IsScalar;

impl KindVisitor for IsScalar {
    type Out = bool;
    fn visit<K: Semantics + 'static>(self) -> bool {
        K::SCALAR
    }
}

/// Record shape from a kind's fields: [`shape`].
pub(super) struct Shape<'f> {
    pub(super) peek: &'f dyn Fn(u32) -> Option<u32>,
}

impl KindVisitor for Shape<'_> {
    type Out = Option<(u32, TailLen)>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
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
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
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
    fn visit<K: Semantics + 'static>(self) {
        K::identity(K::decode(self.v.arena, self.v.r), self.h)
    }
}

/// Decodes a value to its public view: [`CoercedAttrRef::kind`].
pub(super) struct ToRefKind<'a> {
    pub(super) v: CoercedAttrRef<'a>,
}

impl<'a> KindVisitor for ToRefKind<'a> {
    type Out = CoercedAttrRefKind<'a>;
    fn visit<K: Semantics + 'static>(self) -> Self::Out {
        K::wrap(self.v, K::decode(self.v.arena, self.v.r))
    }
}

/// Compares two values of the same kind: [`CoercedAttrRef::content_eq`].
pub(super) struct EqParts<'a, 'b> {
    pub(super) a: CoercedAttrRef<'a>,
    pub(super) b: CoercedAttrRef<'b>,
}

impl KindVisitor for EqParts<'_, '_> {
    type Out = bool;
    fn visit<K: Semantics + 'static>(self) -> bool {
        K::same(
            K::decode(self.a.arena, self.a.r),
            K::decode(self.b.arena, self.b.r),
        )
    }
}
