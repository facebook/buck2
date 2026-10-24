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
//! labels and of the package listing's paths, and tables of the whole-value
//! leaves (visibility, within_view, metadata, modifiers), one entry per
//! distinct value. An arena is *sealed* when its package finishes
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

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::fmt::Write as _;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;
use std::sync::OnceLock;

use allocative::Allocative;
use buck2_core::configuration::compatibility::IncompatiblePlatformReasonCause;
use buck2_core::configuration::compatibility::ResultMaybeCompatible;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::package::PackageLabel;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::soft_error;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_hash::BuckHasher;
use buck2_hash::BuckMutMap;
use buck2_query_parser::BinaryOp;
use buck2_query_parser::Expr;
use buck2_util::arc_str::ArcS;
use buck2_util::arc_str::ArcStr;
use buck2_util::pointer_identity::StaticPointerIdentity;
use buck2_util::pointer_identity::mix_addr;
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
use serde::Serialize;
use serde::Serializer;
use serde::ser::SerializeMap;
use serde::ser::SerializeSeq;
use serde_json::to_value;
use smallvec::SmallVec;
use smallvec::smallvec;
use strong_hash::StrongHash;
use strum::FromRepr;

use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::AttrTypeInner;
use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::attrs::attr_type::arg::ConfiguredMacro;
use crate::attrs::attr_type::arg::ConfiguredStringWithMacros;
use crate::attrs::attr_type::arg::ConfiguredStringWithMacrosPart;
use crate::attrs::attr_type::arg::MacroBase;
use crate::attrs::attr_type::arg::MacroDepKind;
use crate::attrs::attr_type::arg::QueryExpansion;
use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::arg::StringWithMacrosPart;
use crate::attrs::attr_type::attr_config::source_file_display;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepAttrType;
use crate::attrs::attr_type::configuration_dep::ConfigurationDepKind;
use crate::attrs::attr_type::dep::DepAttr;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::label::LabelAttrType;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::query::QueryAttr;
use crate::attrs::attr_type::query::QueryAttrBase;
use crate::attrs::attr_type::query::QueryMacroBase;
use crate::attrs::attr_type::query::ResolvedQueryLiterals;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::coerced_attr::CoercedSelector;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::configuration_context::AttrConfigurationContext;
use crate::attrs::configured_attr::ConfiguredAttr;
use crate::attrs::configured_traversal::ConfiguredAttrTraversal;
use crate::attrs::display::AttrDisplayWithContext;
use crate::attrs::display::AttrDisplayWithContextExt;
use crate::attrs::fmt_context::AttrFmtContext;
use crate::attrs::serialize::AttrSerializeWithContext;
use crate::attrs::special_attr::SpecialAttr;
use crate::attrs::traversal::CoercedAttrTraversal;
use crate::configuration::resolved::ConfigurationSettingKey;
use crate::metadata::map::MetadataMap;
use crate::modifiers::CfgModifiers;
use crate::provider_id_set::ProviderIdSet;
use crate::visibility::VisibilitySpecification;
use crate::visibility::WithinViewSpecification;

/// Tag byte of a record: `[ width:2 | kind:6 ]`. `width` is the byte width
/// of every head field and ref in the record, minus one.
const TAG_KIND_MASK: u8 = 0x3F;
const TAG_WIDTH_SHIFT: u8 = 6;

/// Arg macro record header (its first head field):
/// - bits 0-3: variant tag (`ARG_MACRO_TAG_*`)
/// - bits 4-11: variant flag byte — location: `MacroDepKind`; exe:
///   `exec_dep`; keyed placeholder: has-arg; query: expansion kind in
///   flag bits 0-1, has-separator in flag bit 2
/// - bit 12: `write_to_file`
const ARG_MACRO_TAG_MASK: u32 = 0xF;
const ARG_MACRO_FLAG_MASK: u32 = 0xFF;
const ARG_MACRO_TAG_LOCATION: u32 = 0;
const ARG_MACRO_TAG_EXE: u32 = 1;
const ARG_MACRO_TAG_UNKEYED: u32 = 2;
const ARG_MACRO_TAG_KEYED: u32 = 3;
const ARG_MACRO_TAG_SOURCE: u32 = 4;
/// Refs: `[expr, sep]`; `expr` is the parsed expression as a `QueryExpr`
/// record, `sep` a text record present when the has-separator flag bit is
/// set.
const ARG_MACRO_TAG_QUERY: u32 = 5;
const ARG_MACRO_FLAG_SHIFT: u32 = 4;
const ARG_MACRO_WTF_BIT: u32 = 1 << 12;

// Query-expression node subtags (first head field of a `QueryExpr` record).
const Q_EXPR_NONE: u32 = 0;
const Q_EXPR_WORD: u32 = 1;
const Q_EXPR_STRING: u32 = 2;
const Q_EXPR_INT: u32 = 3;
const Q_EXPR_LABEL: u32 = 4;
const Q_EXPR_FUNCTION: u32 = 5;
const Q_EXPR_BINOP: u32 = 6;
const Q_EXPR_SET: u32 = 7;
const Q_EXPR_FILESET: u32 = 8;

const ARG_QUERY_EXPANSION_MASK: u32 = 0x3;
const ARG_QUERY_EXPANSION_OUTPUT: u32 = 0;
const ARG_QUERY_EXPANSION_TARGET: u32 = 1;
const ARG_QUERY_EXPANSION_TARGET_AND_OUTPUT: u32 = 2;
const ARG_QUERY_HAS_SEP: u32 = 4;

fn dep_kind_bits(k: MacroDepKind) -> u32 {
    match k {
        MacroDepKind::Regular => 0,
        MacroDepKind::Exec => 1,
        MacroDepKind::Toolchain => 2,
    }
}

fn dep_kind_from_bits(bits: u32) -> MacroDepKind {
    match bits {
        0 => MacroDepKind::Regular,
        1 => MacroDepKind::Exec,
        2 => MacroDepKind::Toolchain,
        _ => unreachable!("should be valid: records are only written by the encoder"),
    }
}

fn binary_op_bits(op: BinaryOp) -> u32 {
    match op {
        BinaryOp::Intersect => 0,
        BinaryOp::Except => 1,
        BinaryOp::Union => 2,
    }
}

fn binary_op_from_bits(bits: u32) -> BinaryOp {
    match bits {
        0 => BinaryOp::Intersect,
        1 => BinaryOp::Except,
        2 => BinaryOp::Union,
        _ => unreachable!("should be valid: records are only written by the encoder"),
    }
}

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
        enum AttrKind {
            $( $(#[$doc])* $name = $tag, )*
        }

        impl AttrKind {
            /// Whether values of this kind are inline leaves (payload in the
            /// word) rather than records in the buffer.
            fn is_inline(self) -> bool {
                match self {
                    $( AttrKind::$name => arena_kinds!(@inline $body), )*
                }
            }
        }

        /// Each kind as a type, for [`Kind`] impls and [`dispatch`].
        mod kinds {
            $( pub(super) struct $name; )*
        }

        /// Runs `v` for the [`Kind`] type of `kind`.
        fn dispatch<V: KindVisitor>(kind: AttrKind, v: V) -> V::Out {
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
            v: Whole<WithinViewSpecification> = &WithinViewSpecification,
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
            v: Whole<VisibilitySpecification> = &VisibilitySpecification,
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
        inline (m: Whole<CfgModifiers> = &CfgModifiers) as encode_target_modifiers
            => |_v| CoercedAttrRefKind::Modifiers(m)
    };
    /// A record, not an inline leaf: the tag does not fit the inline kind bits.
    Metadata = 32: {
        record (m: Whole<MetadataMap> = &MetadataMap) as encode_metadata
            => |_v| CoercedAttrRefKind::Metadata(m)
    };
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
    dispatch(kind, Shape { peek: &field })
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

impl AttrKind {
    /// Kinds encoded in the word itself, never as a record.
    /// A select branch that is an outcome rather than a value: an error or
    /// an incompatibility once the select resolves to it.
    fn is_select_outcome(self) -> bool {
        matches!(self, AttrKind::SelectFail | AttrKind::SelectIncompatible)
    }

    /// Kinds whose values hold no deps, inputs, or labels: a traversal can
    /// pass over them without decoding. Select outcomes are scalar for the
    /// unconfigured walk only; the configured walk must report them.
    fn is_scalar(self) -> bool {
        dispatch(self, IsScalar)
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
    /// `SourceLabel`, `ConfigurationDep`, `SplitTransitionDep`, and
    /// `SelectorKeys` records.
    providers_labels: Vec<ProvidersLabel>,
    /// Globally interned `TargetLabel`s, indexed by `PluginDep` records.
    target_labels: Vec<TargetLabel>,
    /// Source paths, indexed by `SourceFile` records.
    paths: Vec<CoercedPath>,
    /// Whole-value leaves by table index, one entry per distinct value in
    /// the package.
    visibility: Vec<VisibilitySpecification>,
    within_view: Vec<WithinViewSpecification>,
    metadata: Vec<MetadataMap>,
    /// `modifiers` values, indexed by `Modifiers` records; each shared with
    /// every holder that outlives the arena.
    modifiers: Vec<CfgModifiers>,
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
/// so it needs no package. Equality and hashing are
/// content-based, so two `OwnedCoercedAttr`s encoding equal values compare
/// equal regardless of which storage holds them.
#[derive(Clone, Dupe, Debug, Allocative, Pagable)]
pub struct OwnedCoercedAttr {
    storage: Arc<PackageAttrStorage>,
    word: AttrWord,
}

impl OwnedCoercedAttr {
    /// A value `f` encodes into storage of its own.
    pub fn new_with(
        f: impl FnOnce(&mut CoercedAttrArenaBuilder) -> buck2_error::Result<AttrToken>,
    ) -> buck2_error::Result<OwnedCoercedAttr> {
        let mut builder = CoercedAttrArenaBuilder::default();
        let value = f(&mut builder)?;
        Self::seal_from_builder(builder, value)
    }

    /// Seal `builder` into this value's own storage; `token` must have been
    /// minted by `builder`.
    pub fn seal_from_builder(
        mut builder: CoercedAttrArenaBuilder,
        token: AttrToken,
    ) -> buck2_error::Result<OwnedCoercedAttr> {
        let word = builder.root(token);
        let storage = Arc::new(PackageAttrStorage::default());
        storage.seal(builder.finish());
        Ok(OwnedCoercedAttr { storage, word })
    }

    /// A handle to the storage `word` points into, without re-encoding.
    pub(crate) fn from_parts(storage: Arc<PackageAttrStorage>, word: AttrWord) -> OwnedCoercedAttr {
        OwnedCoercedAttr { storage, word }
    }

    /// A handle to `child`, which must be a view into this value's storage.
    pub fn sub(&self, child: CoercedAttrRef<'_>) -> OwnedCoercedAttr {
        debug_assert!(
            std::ptr::eq(self.storage.arena(), child.arena),
            "child view is not backed by this value's storage"
        );
        OwnedCoercedAttr {
            storage: self.storage.dupe(),
            word: child.r,
        }
    }

    pub fn as_ref(&self) -> CoercedAttrRef<'_> {
        self.storage.arena().view(self.word)
    }

    /// The identity-grade strong hash of the value. Roots read the retained
    /// hash; interior values (`sub` handles) compute it on demand.
    pub fn strong_hash(&self) -> StrongHash128 {
        let arena = self.storage.arena();
        arena
            .root_strong_hash(self.word)
            .unwrap_or_else(|| arena.strong_of(self.word, &mut BuckMutMap::default()))
    }
}

impl PartialEq for OwnedCoercedAttr {
    fn eq(&self, other: &OwnedCoercedAttr) -> bool {
        // The 128-bit blake3 content hash IS equality (`CoercedAttrArena::
        // strong_of`); debug builds keep verifying that structurally.
        let eq = self.strong_hash() == other.strong_hash();
        if eq {
            debug_assert!(
                self.as_ref().content_eq(&other.as_ref()),
                "identity hashes matched but content differs"
            );
        }
        eq
    }
}

impl Eq for OwnedCoercedAttr {}

impl Hash for OwnedCoercedAttr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.strong_hash().hash(state);
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
    /// as a root (`CoercedAttrArenaBuilder::root`).
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

    /// Identity-grade content hash of one value: blake3 over the value's kind
    /// and its parts, children by their own identity hashes. Equal content
    /// yields equal hashes across arenas and processes.
    fn strong_of(&self, r: AttrWord, memo: &mut BuckMutMap<u32, StrongHash128>) -> StrongHash128 {
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
    fn int_at(&self, r: AttrWord) -> i64 {
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

    /// Header, payload index, and refs of an `ArgMacro` record.
    fn arg_macro_parts(&self, r: AttrWord) -> (u32, u32, RefRun<'_>) {
        let rec = self.open(r);
        (rec.head.raw(0), rec.head.raw(1), rec.refs())
    }

    fn arg_part_at(&self, r: AttrWord) -> ArgPartView<'_> {
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

/// A run of fixed-width values inside one record: ref fields (see
/// [`AttrWord::to_field`]) or raw indices (`SelectorKeys`).
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
    fn iter_raw(&self) -> impl ExactSizeIterator<Item = u32> + use<'a> {
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

    fn iter(&self) -> impl ExactSizeIterator<Item = AttrWord> + use<'a> {
        let rec = self.rec;
        self.iter_raw().map(move |f| {
            AttrWord::from_field(f, rec)
                .expect("should be present: only a selector default may be absent")
        })
    }
}

/// How a kind's values are stored.
#[derive(Copy, Clone, PartialEq, Eq)]
enum Storage {
    /// No record: the word carries the kind and the single field's value.
    Inline,
    /// A record of head fields and child refs, deduped by content.
    Record,
    /// A text record, deduped by bytes.
    Text,
}

/// A kind whose values are a sequence of typed fields: everything about it
/// derives from the field list.
trait SchemaKind: 'static {
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
trait Kind {
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
fn encode_kind<K: SchemaKind>(
    b: &mut CoercedAttrArenaBuilder,
    spec: <K::Fields as FieldList>::Spec<'_>,
) -> buck2_error::Result<AttrToken> {
    let mut w = RecordWriter::new(K::KIND);
    K::Fields::encode(spec, b, &mut w)?;
    b.emit(K::STORAGE, w)
}

/// Selects the [`Kind`] type for a tag at runtime, monomorphizing the
/// visitor per kind.
trait KindVisitor {
    type Out;
    fn visit<K: Kind + 'static>(self) -> Self::Out;
}

/// The blake3 identity hasher plus the per-arena memo child hashes use.
struct IdHasher<'m> {
    h: Blake3StrongHasher,
    memo: &'m mut BuckMutMap<u32, StrongHash128>,
}

impl IdHasher<'_> {
    /// A child's whole-value identity hash, memoized per record.
    fn child(&mut self, c: CoercedAttrRef<'_>) {
        c.arena.strong_of(c.r, self.memo).strong_hash(&mut self.h);
    }
}

/// Content equality: the same kind with equal parts, across arenas.
impl PartialEq for CoercedAttrRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.content_eq(other)
    }
}

/// Reads one value's fields in order: the inline payload, or a record's
/// head fields and tail.
struct Cursor<'a> {
    arena: &'a CoercedAttrArena,
    r: AttrWord,
    /// Head fields of a record; `None` for an inline word.
    head: Option<RefRun<'a>>,
    tail: Tail<'a>,
    head_i: u32,
    ref_i: u32,
}

impl<'a> Cursor<'a> {
    fn new(arena: &'a CoercedAttrArena, r: AttrWord) -> Cursor<'a> {
        let (head, tail) = if r.inline_parts().is_some() {
            (None, Tail::None)
        } else {
            let rec = arena.open(r);
            (Some(rec.head), rec.tail)
        };
        Cursor {
            arena,
            r,
            head,
            tail,
            head_i: 0,
            ref_i: 0,
        }
    }

    /// The next head field, or the inline payload.
    fn raw(&mut self) -> u32 {
        match self.head {
            None => {
                self.r
                    .inline_parts()
                    .expect("should be inline: no record head")
                    .1
            }
            Some(head) => {
                let v = head.raw(self.head_i);
                self.head_i += 1;
                v
            }
        }
    }

    fn text(&self) -> &'a str {
        self.arena.str_at(self.r)
    }

    /// The next `n` refs of the tail.
    fn refs(&mut self, n: u32) -> RefRun<'a> {
        let Tail::Refs(run) = self.tail else {
            unreachable!("should have refs: the kind's fields say so")
        };
        let out = RefRun {
            len: n,
            ..run.skip(self.ref_i)
        };
        self.ref_i += n;
        out
    }

    /// The next ref of the tail; `None` for an absent slot.
    fn child(&mut self) -> Option<AttrWord> {
        self.refs(1).get_opt(0)
    }
}

/// Head fields, child refs and weak content hash of one value being
/// encoded, gathered field by field.
struct RecordWriter<'s> {
    kind: AttrKind,
    head: EncodeScratchFields,
    refs: EncodeScratchRefs,
    text: Option<&'s str>,
    h: BuckHasher,
}

impl<'s> RecordWriter<'s> {
    fn new(kind: AttrKind) -> RecordWriter<'s> {
        let mut h = BuckHasher::default();
        Hash::hash(&(kind as u8), &mut h);
        RecordWriter {
            kind,
            head: EncodeScratchFields::new(),
            refs: EncodeScratchRefs::new(),
            text: None,
            h,
        }
    }

    fn head(&mut self, v: u32) {
        self.head.push(v);
    }

    fn r(&mut self, w: AttrWord) {
        self.refs.push(w);
    }

    fn hash<T: Hash + ?Sized>(&mut self, t: &T) {
        t.hash(&mut self.h);
    }
}

/// What a field occupies in a record.
struct Part {
    head: u32,
    refs: Option<u32>,
    text: Option<u32>,
}

impl Part {
    const fn head(n: u32) -> Part {
        Part {
            head: n,
            refs: None,
            text: None,
        }
    }
}

/// One typed component of a record: what a head field, a run of refs or
/// the text tail means. Each field knows how to read itself, how to encode
/// its input form, how to re-encode its decoded form into another arena,
/// and how it hashes and compares.
trait Field {
    /// Whether the field can hold a dep: a label, a path or a child.
    const DEPS: bool;
    /// Decoded form, borrowed from the arena.
    type View<'a>: Copy;
    /// Input form for encoding: leaf values by reference, children as
    /// tokens.
    type Spec<'s>: Copy;
    /// What the field occupies, reading head fields through `peek`; `at` is
    /// the field's first head index.
    fn shape(peek: &dyn Fn(u32) -> Option<u32>, at: u32) -> Option<Part>;
    fn decode<'a>(c: &mut Cursor<'a>) -> Self::View<'a>;
    fn encode<'s>(
        spec: Self::Spec<'s>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()>;
    fn reemit<'a>(
        view: Self::View<'a>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()>;
    fn identity(view: Self::View<'_>, h: &mut IdHasher<'_>);
    fn same(a: Self::View<'_>, b: Self::View<'_>) -> bool;
}

/// A kind's fields as a tuple.
trait FieldList {
    const DEPS: bool;
    type View<'a>: Copy;
    type Spec<'s>: Copy;
    fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)>;
    fn decode<'a>(c: &mut Cursor<'a>) -> Self::View<'a>;
    fn encode<'s>(
        spec: Self::Spec<'s>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()>;
    fn reemit<'a>(
        view: Self::View<'a>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()>;
    fn identity(view: Self::View<'_>, h: &mut IdHasher<'_>);
    fn same(a: Self::View<'_>, b: Self::View<'_>) -> bool;
}

fn tail_len(refs: Option<u32>, text: Option<u32>) -> TailLen {
    match (text, refs) {
        (Some(t), _) => TailLen::Text(t),
        (None, Some(n)) => TailLen::Refs(n),
        (None, None) => TailLen::None,
    }
}

impl FieldList for () {
    const DEPS: bool = false;
    type View<'a> = ();
    type Spec<'s> = ();

    fn shape(_peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
        Some((0, TailLen::None))
    }

    fn decode<'a>(_c: &mut Cursor<'a>) {}

    fn encode<'s>(
        (): (),
        _b: &mut CoercedAttrArenaBuilder,
        _w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        Ok(())
    }

    fn reemit<'a>(
        (): (),
        _b: &mut CoercedAttrArenaBuilder,
        _w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        Ok(())
    }

    fn identity((): (), _h: &mut IdHasher<'_>) {}

    fn same((): (), (): ()) -> bool {
        true
    }
}

macro_rules! field_tuples {
    ($( ($($F:ident $a:ident $b:ident),+) ;)*) => { $(
        impl<$($F: Field),+> FieldList for ($($F,)+) {
            const DEPS: bool = $($F::DEPS)||+;
            type View<'a> = ($($F::View<'a>,)+);
            type Spec<'s> = ($($F::Spec<'s>,)+);

            fn shape(peek: &dyn Fn(u32) -> Option<u32>) -> Option<(u32, TailLen)> {
                let mut at = 0u32;
                let mut refs: Option<u32> = None;
                let mut text = None;
                $(
                    let p = $F::shape(peek, at)?;
                    at = at.checked_add(p.head)?;
                    if let Some(n) = p.refs {
                        refs = Some(refs.unwrap_or(0).checked_add(n)?);
                    }
                    text = text.or(p.text);
                )+
                Some((at, tail_len(refs, text)))
            }

            fn decode<'a>(c: &mut Cursor<'a>) -> Self::View<'a> {
                ($($F::decode(c),)+)
            }

            fn encode<'s>(
                ($($a,)+): Self::Spec<'s>,
                b: &mut CoercedAttrArenaBuilder,
                w: &mut RecordWriter<'s>,
            ) -> buck2_error::Result<()> {
                $( $F::encode($a, b, w)?; )+
                Ok(())
            }

            fn reemit<'a>(
                ($($a,)+): Self::View<'a>,
                b: &mut CoercedAttrArenaBuilder,
                w: &mut RecordWriter<'a>,
            ) -> buck2_error::Result<()> {
                $( $F::reemit($a, b, w)?; )+
                Ok(())
            }

            fn identity(($($a,)+): Self::View<'_>, h: &mut IdHasher<'_>) {
                $( $F::identity($a, h); )+
            }

            fn same(($($a,)+): Self::View<'_>, ($($b,)+): Self::View<'_>) -> bool {
                $( $F::same($a, $b) )&&+
            }
        }
    )* };
}

field_tuples! {
    (A a0 b0);
    (A a0 b0, B a1 b1);
}

/// A leaf field whose decoded and input forms are both `&T`: it hashes as
/// `T`'s `StrongHash` and compares as `T`'s equality.
macro_rules! leaf_field {
    ($(
        $(#[$doc:meta])*
        $name:ident : $t:ty, deps = $deps:literal,
        |$c:ident| $decode:expr,
        |$spec:ident, $b:ident, $w:ident| $encode:expr;
    )*) => { $(
        $(#[$doc])*
        struct $name;

        impl Field for $name {
            const DEPS: bool = $deps;
            type View<'a> = &'a $t;
            type Spec<'s> = &'s $t;

            fn shape(_peek: &dyn Fn(u32) -> Option<u32>, _at: u32) -> Option<Part> {
                Some(Part::head(1))
            }

            fn decode<'a>($c: &mut Cursor<'a>) -> &'a $t {
                $decode
            }

            fn encode<'s>(
                $spec: &'s $t,
                $b: &mut CoercedAttrArenaBuilder,
                $w: &mut RecordWriter<'s>,
            ) -> buck2_error::Result<()> {
                $encode
            }

            fn reemit<'a>(
                view: &'a $t,
                b: &mut CoercedAttrArenaBuilder,
                w: &mut RecordWriter<'a>,
            ) -> buck2_error::Result<()> {
                Self::encode(view, b, w)
            }

            fn identity(view: &$t, h: &mut IdHasher<'_>) {
                view.strong_hash(&mut h.h);
            }

            fn same(a: &$t, b: &$t) -> bool {
                a == b
            }
        }
    )* };
}

leaf_field! {
    /// A providers-label table index.
    Label: ProvidersLabel, deps = true,
    |c| &c.arena.providers_labels[c.raw() as usize],
    |l, b, w| {
        let (idx, hash) = b.providers_label_idx_and_hash(l);
        w.head(idx);
        w.hash(&hash);
        Ok(())
    };
    /// A target-label table index.
    Target: TargetLabel, deps = true,
    |c| &c.arena.target_labels[c.raw() as usize],
    |t, b, w| {
        w.head(b.target_label_idx(t.dupe()));
        w.hash(t);
        Ok(())
    };
    /// A path table index.
    Path: CoercedPath, deps = true,
    |c| &c.arena.paths[c.raw() as usize],
    |p, b, w| {
        let (idx, hash) = b.source_path_idx_and_hash(p);
        w.head(idx);
        w.hash(&hash);
        Ok(())
    };
}

/// A whole value interned into its own table, one entry per distinct
/// value; the field is the table index.
struct Whole<T>(std::marker::PhantomData<T>);

/// A side table of whole values and its content dedup.
trait WholeTable: Hash + PartialEq + Clone + StrongHash + 'static {
    fn table(arena: &CoercedAttrArena) -> &Vec<Self>;
    fn table_mut(
        b: &mut CoercedAttrArenaBuilder,
    ) -> (&mut hashbrown::HashTable<(u64, u32)>, &mut Vec<Self>);
}

macro_rules! whole_tables {
    ($($t:ty => $table:ident, $dedup:ident;)*) => { $(
        impl WholeTable for $t {
            fn table(arena: &CoercedAttrArena) -> &Vec<Self> {
                &arena.$table
            }
            fn table_mut(
                b: &mut CoercedAttrArenaBuilder,
            ) -> (&mut hashbrown::HashTable<(u64, u32)>, &mut Vec<Self>) {
                (&mut b.$dedup, &mut b.arena.$table)
            }
        }
    )* };
}

whole_tables! {
    VisibilitySpecification => visibility, visibility_dedup;
    WithinViewSpecification => within_view, within_view_dedup;
    CfgModifiers => modifiers, modifiers_dedup;
    MetadataMap => metadata, metadata_dedup;
}

impl<T: WholeTable> Field for Whole<T> {
    const DEPS: bool = false;
    type View<'a> = &'a T;
    type Spec<'s> = &'s T;

    fn shape(_peek: &dyn Fn(u32) -> Option<u32>, _at: u32) -> Option<Part> {
        Some(Part::head(1))
    }

    fn decode<'a>(c: &mut Cursor<'a>) -> &'a T {
        &T::table(c.arena)[c.raw() as usize]
    }

    fn encode<'s>(
        v: &'s T,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        let (idx, hash) = b.intern_whole::<T>(v);
        w.head(idx);
        w.hash(&hash);
        Ok(())
    }

    fn reemit<'a>(
        view: &'a T,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        Self::encode(view, b, w)
    }

    fn identity(view: &T, h: &mut IdHasher<'_>) {
        view.strong_hash(&mut h.h);
    }

    fn same(a: &T, b: &T) -> bool {
        a == b
    }
}

/// A providers-label table index plus one, or zero for absent.
struct OptLabel;

impl Field for OptLabel {
    const DEPS: bool = true;
    type View<'a> = Option<&'a ProvidersLabel>;
    type Spec<'s> = Option<&'s ProvidersLabel>;

    fn shape(_peek: &dyn Fn(u32) -> Option<u32>, _at: u32) -> Option<Part> {
        Some(Part::head(1))
    }

    fn decode<'a>(c: &mut Cursor<'a>) -> Option<&'a ProvidersLabel> {
        match c.raw() {
            0 => None,
            i => Some(&c.arena.providers_labels[(i - 1) as usize]),
        }
    }

    fn encode<'s>(
        l: Option<&'s ProvidersLabel>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        let (field, hash) = match l {
            Some(l) => {
                let (idx, hash) = b.providers_label_idx_and_hash(l);
                (idx + 1, Some(hash))
            }
            None => (0, None),
        };
        w.head(field);
        w.hash(&hash);
        Ok(())
    }

    fn reemit<'a>(
        view: Option<&'a ProvidersLabel>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        Self::encode(view, b, w)
    }

    fn identity(view: Option<&ProvidersLabel>, h: &mut IdHasher<'_>) {
        view.strong_hash(&mut h.h);
    }

    fn same(a: Option<&ProvidersLabel>, b: Option<&ProvidersLabel>) -> bool {
        a == b
    }
}

/// An immediate head field.
struct U32;

impl Field for U32 {
    const DEPS: bool = false;
    type View<'a> = u32;
    type Spec<'s> = u32;

    fn shape(_peek: &dyn Fn(u32) -> Option<u32>, _at: u32) -> Option<Part> {
        Some(Part::head(1))
    }

    fn decode<'a>(c: &mut Cursor<'a>) -> u32 {
        c.raw()
    }

    fn encode<'s>(
        v: u32,
        _b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        w.head(v);
        w.hash(&v);
        Ok(())
    }

    fn reemit<'a>(
        view: u32,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        Self::encode(view, b, w)
    }

    fn identity(view: u32, h: &mut IdHasher<'_>) {
        h.h.write_u32(view);
    }

    fn same(a: u32, b: u32) -> bool {
        a == b
    }
}

/// The UTF-8 text tail, with its byte length as the head field. Text
/// storage writes the record; the field only carries the text.
struct Text;

impl Field for Text {
    const DEPS: bool = false;
    type View<'a> = &'a str;
    type Spec<'s> = &'s str;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>, at: u32) -> Option<Part> {
        Some(Part {
            head: 1,
            refs: None,
            text: Some(peek(at)?),
        })
    }

    fn decode<'a>(c: &mut Cursor<'a>) -> &'a str {
        c.text()
    }

    fn encode<'s>(
        s: &'s str,
        _b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        w.text = Some(s);
        Ok(())
    }

    fn reemit<'a>(
        view: &'a str,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        Self::encode(view, b, w)
    }

    fn identity(view: &str, h: &mut IdHasher<'_>) {
        view.strong_hash(&mut h.h);
    }

    fn same(a: &str, b: &str) -> bool {
        a == b
    }
}

/// One child ref.
struct Child;

impl Field for Child {
    const DEPS: bool = true;
    type View<'a> = CoercedAttrRef<'a>;
    type Spec<'s> = AttrToken;

    fn shape(_peek: &dyn Fn(u32) -> Option<u32>, _at: u32) -> Option<Part> {
        Some(Part {
            head: 0,
            refs: Some(1),
            text: None,
        })
    }

    fn decode<'a>(c: &mut Cursor<'a>) -> CoercedAttrRef<'a> {
        c.arena.view(
            c.child()
                .expect("should be present: only a selector default may be absent"),
        )
    }

    fn encode<'s>(
        t: AttrToken,
        _b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        w.r(t.0);
        w.hash(&t.1);
        Ok(())
    }

    fn reemit<'a>(
        view: CoercedAttrRef<'a>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        Self::encode(b.encode_view(view)?, b, w)
    }

    fn identity(view: CoercedAttrRef<'_>, h: &mut IdHasher<'_>) {
        h.child(view);
    }

    fn same(a: CoercedAttrRef<'_>, b: CoercedAttrRef<'_>) -> bool {
        a.content_eq(&b)
    }
}

/// One child ref that may be absent (a selector's default): the identity
/// hash writes a presence flag, then the child.
struct OptChild;

impl Field for OptChild {
    const DEPS: bool = true;
    type View<'a> = Option<CoercedAttrRef<'a>>;
    type Spec<'s> = Option<AttrToken>;

    fn shape(_peek: &dyn Fn(u32) -> Option<u32>, _at: u32) -> Option<Part> {
        Some(Part {
            head: 0,
            refs: Some(1),
            text: None,
        })
    }

    fn decode<'a>(c: &mut Cursor<'a>) -> Option<CoercedAttrRef<'a>> {
        c.child().map(|w| c.arena.view(w))
    }

    fn encode<'s>(
        t: Option<AttrToken>,
        _b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        w.r(t.map_or(ABSENT_REF, |t| t.0));
        w.hash(&t.map(|t| t.1));
        Ok(())
    }

    fn reemit<'a>(
        view: Option<CoercedAttrRef<'a>>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        let t = view.map(|v| b.encode_view(v)).transpose()?;
        Self::encode(t, b, w)
    }

    fn identity(view: Option<CoercedAttrRef<'_>>, h: &mut IdHasher<'_>) {
        match view {
            Some(c) => {
                h.h.write_u8(1);
                h.child(c);
            }
            None => h.h.write_u8(0),
        }
    }

    fn same(a: Option<CoercedAttrRef<'_>>, b: Option<CoercedAttrRef<'_>>) -> bool {
        a == b
    }
}

/// A count head field, then that many items of `T`, each `T::STRIDE`
/// child refs.
struct ChildrenOf<T>(std::marker::PhantomData<T>);

/// Child values.
type Children = ChildrenOf<Values>;
/// (key, value) child pairs.
type ChildPairs = ChildrenOf<Pairs>;
/// Arg parts.
type ArgChildren = ChildrenOf<ArgParts>;

impl<T: RefItem + 'static> Field for ChildrenOf<T> {
    const DEPS: bool = true;
    type View<'a> = Refs<'a, T>;
    type Spec<'s> = &'s T::Spec;

    fn shape(peek: &dyn Fn(u32) -> Option<u32>, at: u32) -> Option<Part> {
        Some(Part {
            head: 1,
            refs: Some(peek(at)?.checked_mul(T::STRIDE)?),
            text: None,
        })
    }

    fn decode<'a>(c: &mut Cursor<'a>) -> Refs<'a, T> {
        let n = c.raw();
        Refs {
            run: c.refs(n * T::STRIDE),
            _item: std::marker::PhantomData,
        }
    }

    fn encode<'s>(
        items: &'s T::Spec,
        _b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        let mut n = 0;
        let head_at = w.head.len();
        w.head(0);
        for (i, t) in T::tokens(items).enumerate() {
            w.r(t.0);
            w.hash(&t.1);
            n = i + 1;
        }
        w.head[head_at] = (n as u32) / T::STRIDE;
        Ok(())
    }

    fn reemit<'a>(
        view: Refs<'a, T>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        w.head(view.len() as u32);
        for c in view.children() {
            let t = b.encode_view(c)?;
            w.r(t.0);
            w.hash(&t.1);
        }
        Ok(())
    }

    fn identity(view: Refs<'_, T>, h: &mut IdHasher<'_>) {
        h.h.write_usize(view.len());
        for c in view.children() {
            h.child(c);
        }
    }

    fn same(a: Refs<'_, T>, b: Refs<'_, T>) -> bool {
        a.len() == b.len() && a.children().zip(b.children()).all(|(x, y)| x == y)
    }
}

/// A selector's entries: a count head field, a ref to the shared
/// `SelectorKeys` record, then that many value refs.
struct SelectorEntries;

impl Field for SelectorEntries {
    const DEPS: bool = true;
    type View<'a> = AttrRefSelectorEntries<'a>;
    type Spec<'s> = &'s [(ConfigurationSettingKey, AttrToken)];

    fn shape(peek: &dyn Fn(u32) -> Option<u32>, at: u32) -> Option<Part> {
        Some(Part {
            head: 1,
            refs: Some(peek(at)?.checked_add(1)?),
            text: None,
        })
    }

    fn decode<'a>(c: &mut Cursor<'a>) -> AttrRefSelectorEntries<'a> {
        let n = c.raw();
        let keys = c
            .child()
            .expect("should be present: a selector always has a keys record");
        AttrRefSelectorEntries {
            keys: c.arena.open(keys).head.skip(1),
            values: c.refs(n),
        }
    }

    fn encode<'s>(
        entries: &'s [(ConfigurationSettingKey, AttrToken)],
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        CoercedSelector::check_all_keys_unique(entries)?;
        let (keys, keys_hash) = b.encode_selector_keys(entries.iter().map(|(k, _)| &k.0))?;
        w.head(entries.len() as u32);
        w.r(keys);
        w.hash(&keys_hash);
        for (_, v) in entries {
            w.r(v.0);
            w.hash(&v.1);
        }
        Ok(())
    }

    fn reemit<'a>(
        view: AttrRefSelectorEntries<'a>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        let (keys, keys_hash) = b.encode_selector_keys(view.iter().map(|(k, _)| k))?;
        w.head(view.len() as u32);
        w.r(keys);
        w.hash(&keys_hash);
        for (_, v) in view.iter() {
            let t = b.encode_view(v)?;
            w.r(t.0);
            w.hash(&t.1);
        }
        Ok(())
    }

    fn identity(view: AttrRefSelectorEntries<'_>, h: &mut IdHasher<'_>) {
        h.h.write_usize(view.len());
        for (k, v) in view.iter() {
            k.strong_hash(&mut h.h);
            h.child(v);
        }
    }

    fn same(a: AttrRefSelectorEntries<'_>, b: AttrRefSelectorEntries<'_>) -> bool {
        a.len() == b.len()
            && a.iter()
                .zip(b.iter())
                .all(|((ka, va), (kb, vb))| ka == kb && va == vb)
    }
}

/// A ref to a `QueryExpr` record. Its identity hash is the expression's,
/// inline; equality compares reprints.
struct ExprChild;

impl Field for ExprChild {
    const DEPS: bool = true;
    type View<'a> = QueryExprRef<'a>;
    type Spec<'s> = (AttrWord, CoercedAttrHash);

    fn shape(_peek: &dyn Fn(u32) -> Option<u32>, _at: u32) -> Option<Part> {
        Some(Part {
            head: 0,
            refs: Some(1),
            text: None,
        })
    }

    fn decode<'a>(c: &mut Cursor<'a>) -> QueryExprRef<'a> {
        QueryExprRef {
            arena: c.arena,
            r: c.child()
                .expect("should be present: a query always has an expression"),
        }
    }

    fn encode<'s>(
        (r, hash): (AttrWord, CoercedAttrHash),
        _b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'s>,
    ) -> buck2_error::Result<()> {
        w.r(r);
        w.hash(&hash);
        Ok(())
    }

    fn reemit<'a>(
        view: QueryExprRef<'a>,
        b: &mut CoercedAttrArenaBuilder,
        w: &mut RecordWriter<'a>,
    ) -> buck2_error::Result<()> {
        Self::encode(b.encode_query_expr_view(view)?, b, w)
    }

    fn identity(view: QueryExprRef<'_>, h: &mut IdHasher<'_>) {
        view.strong_hash(&mut h.h);
    }

    fn same(a: QueryExprRef<'_>, b: QueryExprRef<'_>) -> bool {
        a.same_expr(&b)
    }
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
struct KeysParts<'a> {
    keys: RefRun<'a>,
}

impl<'a> KeysParts<'a> {
    fn iter(&self) -> impl ExactSizeIterator<Item = &'a ProvidersLabel> + 'a {
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

/// [`AttrKind::is_scalar`].
struct IsScalar;

impl KindVisitor for IsScalar {
    type Out = bool;
    fn visit<K: Kind + 'static>(self) -> bool {
        K::SCALAR
    }
}

/// Record shape from a kind's fields: [`shape`].
struct Shape<'f> {
    peek: &'f dyn Fn(u32) -> Option<u32>,
}

impl KindVisitor for Shape<'_> {
    type Out = Option<(u32, TailLen)>;
    fn visit<K: Kind + 'static>(self) -> Self::Out {
        K::shape(self.peek)
    }
}

/// Copies a value into a builder: [`CoercedAttrArenaBuilder::encode_view`].
struct CopyParts<'b, 'a> {
    v: CoercedAttrRef<'a>,
    b: &'b mut CoercedAttrArenaBuilder,
}

impl KindVisitor for CopyParts<'_, '_> {
    type Out = buck2_error::Result<AttrToken>;
    fn visit<K: Kind + 'static>(self) -> Self::Out {
        K::copy_into(K::decode(self.v.arena, self.v.r), self.b)
    }
}

/// Folds a value's parts into the identity hasher: [`CoercedAttrArena::strong_of`].
struct HashParts<'h, 'm, 'a> {
    v: CoercedAttrRef<'a>,
    h: &'h mut IdHasher<'m>,
}

impl KindVisitor for HashParts<'_, '_, '_> {
    type Out = ();
    fn visit<K: Kind + 'static>(self) {
        K::identity(K::decode(self.v.arena, self.v.r), self.h)
    }
}

/// Decodes a value to its public view: [`CoercedAttrRef::kind`].
struct ToRefKind<'a> {
    v: CoercedAttrRef<'a>,
}

impl<'a> KindVisitor for ToRefKind<'a> {
    type Out = CoercedAttrRefKind<'a>;
    fn visit<K: Kind + 'static>(self) -> Self::Out {
        K::wrap(self.v, K::decode(self.v.arena, self.v.r))
    }
}

/// Compares two values of the same kind: [`CoercedAttrRef::content_eq`].
struct EqParts<'a, 'b> {
    a: CoercedAttrRef<'a>,
    b: CoercedAttrRef<'b>,
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

/// A decoded (shallow) view of one attribute value. Children decode lazily via
/// further `view` calls.
#[derive(Copy, Clone)]
pub struct CoercedAttrRef<'a> {
    arena: &'a CoercedAttrArena,
    r: AttrWord,
}

/// Shallow decode of a [`CoercedAttrRef`]: one case per value kind, with
/// children left as refs.
pub enum CoercedAttrRefKind<'a> {
    None,
    Bool(bool),
    Int(i64),
    String(&'a str),
    EnumVariant(&'a str),
    ArgSimple(&'a str),
    Arg(Refs<'a, ArgParts>),
    List(Refs<'a, Values>),
    Tuple(Refs<'a, Values>),
    Dict(Refs<'a, Pairs>),
    Selector {
        entries: AttrRefSelectorEntries<'a>,
        default: Option<CoercedAttrRef<'a>>,
    },
    Concat(Refs<'a, Values>),
    OneOf(CoercedAttrRef<'a>, u32),
    Dep(&'a ProvidersLabel),
    Label(&'a ProvidersLabel),
    SourceLabel(&'a ProvidersLabel),
    ConfigurationDep(&'a ProvidersLabel),
    SplitTransitionDep(&'a ProvidersLabel),
    PluginDep(&'a TargetLabel),
    SourceFile(&'a CoercedPath),
    Query(QueryView<'a>),
    /// The message of a `select_fail()` leaf.
    SelectFail(&'a str),
    /// The message of a `select_incompatible()` leaf.
    SelectIncompatible(&'a str),
    ExplicitConfiguredDep {
        label: &'a ProvidersLabel,
        platform: &'a TargetLabel,
    },
    /// `transition` is the dynamic transition target; `None` when the attr
    /// type fixes the transition.
    TransitionDep {
        dep: &'a ProvidersLabel,
        transition: Option<&'a ProvidersLabel>,
    },
    Visibility(&'a VisibilitySpecification),
    WithinView(&'a WithinViewSpecification),
    Modifiers(&'a CfgModifiers),
    Metadata(&'a MetadataMap),
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

/// Decoded view of an `attrs.query()` value record.
#[derive(Copy, Clone)]
pub struct QueryView<'a> {
    arena: &'a CoercedAttrArena,
    r: AttrWord,
}

impl<'a> QueryView<'a> {
    fn expr(&self) -> QueryExprRef<'a> {
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

    /// The stored expression's literal labels, in expression order.
    fn literal_labels(&self, out: &mut Vec<&'a ProvidersLabel>) {
        self.expr().labels(out);
    }

    fn configure(&self, ctx: &dyn AttrConfigurationContext) -> QueryAttr<ConfiguredProvidersLabel> {
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

/// Decoded view of a query macro record (`$(query_targets ...)` etc.).
#[derive(Copy, Clone)]
pub struct ArgQueryView<'a> {
    arena: &'a CoercedAttrArena,
    r: AttrWord,
}

impl<'a> ArgQueryView<'a> {
    fn flags(&self) -> u32 {
        let (header, _, _) = self.arena.arg_macro_parts(self.r);
        (header >> ARG_MACRO_FLAG_SHIFT) & ARG_MACRO_FLAG_MASK
    }

    fn expr(&self) -> QueryExprRef<'a> {
        let (_, _, refs) = self.arena.arg_macro_parts(self.r);
        QueryExprRef {
            arena: self.arena,
            r: refs.get(0),
        }
    }

    fn separator(&self) -> Option<&'a str> {
        let (_, _, refs) = self.arena.arg_macro_parts(self.r);
        (self.flags() & ARG_QUERY_HAS_SEP != 0).then(|| self.arena.str_at(refs.get(1)))
    }

    /// Expansion kind bits and the borrowed separator, for readers that must
    /// not allocate; the owned [`QueryExpansion`] clones the separator.
    fn expansion_parts(&self) -> (u32, Option<&'a str>) {
        (self.flags() & ARG_QUERY_EXPANSION_MASK, self.separator())
    }

    fn expansion_type(&self) -> QueryExpansion {
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

    fn configure(
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

/// Handle to one stored query-expression node ([`AttrKind::QueryExpr`]);
/// `view` decodes it shallowly.
#[derive(Copy, Clone)]
struct QueryExprRef<'a> {
    arena: &'a CoercedAttrArena,
    r: AttrWord,
}

/// Shallow decode of a [`QueryExprRef`]. Children decode lazily via further
/// `view` calls.
enum QueryExprView<'a> {
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
struct QueryExprSeq<'a> {
    run: RefRun<'a>,
}

impl<'a> QueryExprSeq<'a> {
    fn len(&self) -> u32 {
        self.run.len()
    }

    fn iter(&self) -> impl Iterator<Item = QueryExprRef<'a>> + 'a {
        let arena = self.run.arena;
        self.run.iter().map(move |r| QueryExprRef { arena, r })
    }
}

/// View of a binop node's tail: `n` operators in the head, paired with `n`
/// operand refs.
#[derive(Copy, Clone)]
struct QueryExprOpSeq<'a> {
    ops: RefRun<'a>,
    run: RefRun<'a>,
}

impl<'a> QueryExprOpSeq<'a> {
    fn len(&self) -> u32 {
        self.run.len()
    }

    fn iter(&self) -> impl Iterator<Item = (BinaryOp, QueryExprRef<'a>)> + 'a {
        let arena = self.run.arena;
        self.ops
            .iter_raw()
            .zip(self.run.iter())
            .map(move |(op, r)| (binary_op_from_bits(op), QueryExprRef { arena, r }))
    }
}

impl<'a> QueryExprRef<'a> {
    fn view(&self) -> QueryExprView<'a> {
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
    fn reprint(&self, out: &mut String, lit: &mut dyn FnMut((usize, usize), &'a ProvidersLabel)) {
        match self.view() {
            QueryExprView::None => out.push_str("None"),
            QueryExprView::Word(t) => out.push_str(t),
            QueryExprView::String(t) => {
                // The query grammar has no escapes, so a string literal
                // holds at most one quote kind.
                let quote = if t.contains('\'') { '"' } else { '\'' };
                out.push(quote);
                out.push_str(t);
                out.push(quote);
            }
            QueryExprView::Int(v) => write!(out, "{v}").expect("should write: String"),
            QueryExprView::Label(label) => {
                let start = out.len();
                write!(out, "{label}").expect("should write: String");
                lit((start, out.len() - start), label);
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

    fn reprint_set(
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
    fn labels(&self, out: &mut Vec<&'a ProvidersLabel>) {
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
    fn same_expr(&self, other: &QueryExprRef<'_>) -> bool {
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

impl<'a> ArgPartView<'a> {
    /// Mirror of `MacroBase::<ProvidersLabel>::configure` off the view.
    fn configure(
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

/// A run of child refs viewed as items of `T`: a sequence's values, a
/// dict's (key, value) pairs, or an arg's parts. `len`/`iter` only; the
/// items decode lazily.
pub struct Refs<'a, T> {
    run: RefRun<'a>,
    _item: std::marker::PhantomData<T>,
}

impl<T> Copy for Refs<'_, T> {}
impl<T> Clone for Refs<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}

/// How a [`Refs`] run decodes: `STRIDE` refs per item.
pub trait RefItem: Sized {
    const STRIDE: u32;
    type Item<'a>;
    /// Input form of the items for encoding, borrowed.
    type Spec: ?Sized;
    fn item<'a>(refs: Refs<'a, Self>, i: u32) -> Self::Item<'a>;
    fn tokens(spec: &Self::Spec) -> impl Iterator<Item = AttrToken> + '_;
}

/// One child value per ref.
pub struct Values;

impl RefItem for Values {
    const STRIDE: u32 = 1;
    type Item<'a> = CoercedAttrRef<'a>;
    type Spec = [AttrToken];
    fn item<'a>(refs: Refs<'a, Self>, i: u32) -> CoercedAttrRef<'a> {
        refs.run.arena.view(refs.run.get(i))
    }
    fn tokens(spec: &[AttrToken]) -> impl Iterator<Item = AttrToken> + '_ {
        spec.iter().copied()
    }
}

/// A (key, value) pair per two refs.
pub struct Pairs;

impl RefItem for Pairs {
    const STRIDE: u32 = 2;
    type Item<'a> = (CoercedAttrRef<'a>, CoercedAttrRef<'a>);
    type Spec = [(AttrToken, AttrToken)];
    fn item<'a>(refs: Refs<'a, Self>, i: u32) -> Self::Item<'a> {
        let run = refs.run;
        (
            run.arena.view(run.get(2 * i)),
            run.arena.view(run.get(2 * i + 1)),
        )
    }
    fn tokens(spec: &[(AttrToken, AttrToken)]) -> impl Iterator<Item = AttrToken> + '_ {
        spec.iter().flat_map(|(k, v)| [*k, *v])
    }
}

/// One arg part (`ArgSimple` or `ArgMacro` record) per ref.
pub struct ArgParts;

impl RefItem for ArgParts {
    const STRIDE: u32 = 1;
    type Item<'a> = ArgPartView<'a>;
    type Spec = [AttrToken];
    fn item<'a>(refs: Refs<'a, Self>, i: u32) -> ArgPartView<'a> {
        refs.run.arena.arg_part_at(refs.run.get(i))
    }
    fn tokens(spec: &[AttrToken]) -> impl Iterator<Item = AttrToken> + '_ {
        spec.iter().copied()
    }
}

impl<'a, T: RefItem> Refs<'a, T> {
    pub fn len(&self) -> usize {
        (self.run.len() / T::STRIDE) as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = T::Item<'a>> + use<'a, T> {
        let refs = *self;
        (0..self.len() as u32).map(move |i| T::item(refs, i))
    }

    /// Every child ref in storage order, whatever the item shape.
    fn children(&self) -> impl ExactSizeIterator<Item = CoercedAttrRef<'a>> + use<'a, T> {
        let arena = self.run.arena;
        self.run.iter().map(move |r| arena.view(r))
    }
}

/// View of a selector value's (configuration setting key, value) entries.
#[derive(Copy, Clone)]
pub struct AttrRefSelectorEntries<'a> {
    /// Providers-label table indices, from the shared `SelectorKeys` record.
    keys: RefRun<'a>,
    /// Value refs, one per key, stored in the selector record.
    values: RefRun<'a>,
}

impl<'a> AttrRefSelectorEntries<'a> {
    pub fn len(&self) -> usize {
        self.values.len() as usize
    }

    pub fn iter(
        &self,
    ) -> impl ExactSizeIterator<Item = (&'a ProvidersLabel, CoercedAttrRef<'a>)> + 'a {
        let arena = self.values.arena;
        self.keys
            .iter_raw()
            .zip(self.values.iter())
            .map(move |(k, v)| (&arena.providers_labels[k as usize], arena.view(v)))
    }
}

impl<'a> CoercedAttrRef<'a> {
    pub fn kind(&self) -> CoercedAttrRefKind<'a> {
        dispatch(self.arena.kind_of(self.r), ToRefKind { v: *self })
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// The decoded parts if this value is of kind `K`: the typed downcast,
    /// for readers that want one kind rather than the sum `kind()` gives.
    fn parts<K: Kind>(&self) -> Option<K::Parts<'a>> {
        (self.arena.kind_of(self.r) == K::KIND).then(|| K::decode(self.arena, self.r))
    }

    pub fn unpack_visibility(&self) -> Option<&'a VisibilitySpecification> {
        self.parts::<kinds::Visibility>().map(|(v,)| v)
    }

    pub fn unpack_within_view(&self) -> Option<&'a WithinViewSpecification> {
        self.parts::<kinds::WithinView>().map(|(v,)| v)
    }

    pub fn unpack_metadata(&self) -> Option<&'a MetadataMap> {
        self.parts::<kinds::Metadata>().map(|(m,)| m)
    }

    pub fn unpack_target_modifiers(&self) -> Option<&'a CfgModifiers> {
        self.parts::<kinds::Modifiers>().map(|(m,)| m)
    }

    /// Whether evaluating this value can ever yield `None`, directly or
    /// through a `select()` branch.
    pub fn may_return_none(&self) -> bool {
        self.parts::<kinds::None>().is_some()
            || self
                .parts::<kinds::Selector>()
                .is_some_and(|(entries, default)| {
                    entries.iter().any(|(_, v)| v.may_return_none())
                        || default.is_some_and(|d| d.may_return_none())
                })
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
        let kind = self.arena.kind_of(self.r);
        kind == other.arena.kind_of(other.r)
            && dispatch(
                kind,
                EqParts {
                    a: *self,
                    b: *other,
                },
            )
    }

    /// Type-threaded traversal over the value's deps. Callback borrows come
    /// from the arena's side tables, so they carry the arena's lifetime.
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
            // Selects are type-transparent here.
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
            CoercedAttrRefKind::Visibility(_)
            | CoercedAttrRefKind::WithinView(_)
            | CoercedAttrRefKind::Modifiers(_)
            | CoercedAttrRefKind::Metadata(_) => Ok(()),
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
                // Mirror of `StringWithMacros::<ProvidersLabel>::traverse`, with
                // `MacroBase::<ProvidersLabel>::traverse`'s dispatch inlined.
                (CoercedAttrRefKind::Arg(parts), AttrTypeInner::Arg(_)) => {
                    for part in parts.iter() {
                        let ArgPartView::Macro { mac, .. } = part else {
                            continue;
                        };
                        match mac {
                            ArgMacroView::UserKeyedPlaceholder { label, .. } => {
                                traversal.dep(label)?;
                            }
                            ArgMacroView::Location {
                                label,
                                dep_kind: MacroDepKind::Exec,
                            } => traversal.exec_dep(label)?,
                            ArgMacroView::Location {
                                label,
                                dep_kind: MacroDepKind::Toolchain,
                            } => traversal.toolchain_dep(label)?,
                            ArgMacroView::Location {
                                label,
                                dep_kind: MacroDepKind::Regular,
                            } => traversal.dep(label)?,
                            ArgMacroView::Exe {
                                label,
                                exec_dep: true,
                            } => traversal.exec_dep(label)?,
                            ArgMacroView::Exe {
                                label,
                                exec_dep: false,
                            } => traversal.dep(label)?,
                            ArgMacroView::Source(path) => {
                                let pkg = pkg.ok_or_else(|| {
                                    internal_error!(
                                        "Expected a package when traversing arg source macro"
                                    )
                                })?;
                                for x in path.inputs() {
                                    traversal.input(SourcePathRef::new(pkg, x))?;
                                }
                            }
                            // Mirror of `QueryAttrBase::<ProvidersLabel>::traverse`:
                            // literal labels only. Expression order equals
                            // the map's offset-sorted order.
                            ArgMacroView::Query(q) => {
                                let mut labels = Vec::new();
                                q.literal_labels(&mut labels);
                                for label in labels {
                                    traversal.dep(label)?;
                                }
                            }
                            ArgMacroView::UserUnkeyedPlaceholder(_) => {}
                        }
                    }
                    Ok(())
                }
                // Queries have no inputs or configuration deps: literal
                // labels only, in expression order.
                (CoercedAttrRefKind::Query(q), AttrTypeInner::Query(_)) => {
                    let mut labels = Vec::new();
                    q.literal_labels(&mut labels);
                    for label in labels {
                        traversal.dep(label)?;
                    }
                    Ok(())
                }
                (
                    CoercedAttrRefKind::ExplicitConfiguredDep { label, platform },
                    AttrTypeInner::ConfiguredDep(_),
                ) => {
                    traversal.dep(label)?;
                    traversal.configuration_dep(
                        &ProvidersLabel::default_for(platform.dupe()),
                        ConfigurationDepKind::ConfiguredDepPlatform,
                    )
                }
                (
                    CoercedAttrRefKind::TransitionDep { dep, transition },
                    AttrTypeInner::TransitionDep(ty),
                ) => ty.traverse(dep, transition, traversal),
                _ => Err(internal_error!(
                    "attr arena traverse: kind and type mismatch"
                )),
            },
        }
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// Builds the `ConfiguredAttr` straight off the arena: selects resolve on
    /// the view, and only the configured form is materialized.
    ///
    /// TODO(jtbraun): tag errors with `ErrorTag::ConfigureAttr` and emit the
    /// `select_first_match_differs` soft error.
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
                    None => return Self::select_no_match_error(ctx, entries).into(),
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
            CoercedAttrRefKind::Visibility(v) => ConfiguredAttr::Visibility(v.clone()),
            CoercedAttrRefKind::WithinView(v) => ConfiguredAttr::WithinView(v.clone()),
            CoercedAttrRefKind::Modifiers(m) => ConfiguredAttr::TargetModifiers(m.dupe()),
            CoercedAttrRefKind::Metadata(m) => ConfiguredAttr::Metadata(m.clone()),
            kind => return Self::configure_typed(kind, t, ctx, attr_name),
        };
        ResultMaybeCompatible::Compatible(configured)
    }

    /// The error for a select where no condition matched and no default
    /// was set.
    fn select_no_match_error(
        ctx: &dyn AttrConfigurationContext,
        entries: AttrRefSelectorEntries<'a>,
    ) -> buck2_error::Error {
        buck2_error!(
            buck2_error::ErrorTag::Input,
            "None of {} conditions matched configuration `{}` and no default was set:\n{}",
            entries.len(),
            ctx.cfg().cfg(),
            entries.iter().map(|(k, _)| format!("  {k}")).join("\n"),
        )
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

    /// The result of a select resolving to `select_fail()` or
    /// `select_incompatible()`: type-transparent, so it is the same whatever
    /// the attribute's type and whether the value is configured or only
    /// traversed. `None` for any other kind.
    fn select_outcome<T>(
        kind: &CoercedAttrRefKind<'a>,
        ctx: &dyn AttrConfigurationContext,
    ) -> Option<ResultMaybeCompatible<T>> {
        Some(match kind {
            CoercedAttrRefKind::SelectFail(message) => buck2_error!(
                buck2_error::ErrorTag::Input,
                "select resolved to select_fail(): {message}"
            )
            .into(),
            CoercedAttrRefKind::SelectIncompatible(message) => {
                ResultMaybeCompatible::Incompatible(ctx.incompatible_platform_reason(
                    IncompatiblePlatformReasonCause::SelectIncompatible(ArcStr::from(*message)),
                ))
            }
            _ => return None,
        })
    }

    fn configure_typed(
        kind: CoercedAttrRefKind<'a>,
        t: &AttrType,
        ctx: &dyn AttrConfigurationContext,
        attr_name: Option<&str>,
    ) -> ResultMaybeCompatible<ConfiguredAttr> {
        if let Some(outcome) = Self::select_outcome(&kind, ctx) {
            return outcome;
        }
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
                (CoercedAttrRefKind::Query(q), AttrTypeInner::Query(_)) => {
                    ConfiguredAttr::Query(Box::new(q.configure(ctx)))
                }
                (CoercedAttrRefKind::Modifiers(m), AttrTypeInner::TargetModifiers(_)) => {
                    ConfiguredAttr::TargetModifiers(*m)
                }
                (
                    CoercedAttrRefKind::ExplicitConfiguredDep { label, platform },
                    AttrTypeInner::ConfiguredDep(ty),
                ) => ty.configure(ctx, label, platform.dupe())?,
                (
                    CoercedAttrRefKind::TransitionDep { dep, transition },
                    AttrTypeInner::TransitionDep(ty),
                ) => ty.configure(dep, transition, ctx)?,
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
                (CoercedAttrRefKind::Arg(parts), AttrTypeInner::Arg(ty)) => {
                    let configured = parts
                        .iter()
                        .map(|p| p.configure(ctx))
                        .collect::<buck2_error::Result<Box<[_]>>>()?;
                    ConfiguredAttr::Arg(ConfiguredStringWithMacros {
                        string_with_macros: StringWithMacros::ManyParts(configured),
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

    /// Fused [`Self::configure`] + `ConfiguredAttr::traverse`: visit the deps
    /// the configured form of this value would contain, without
    /// materializing `ConfiguredAttr` trees. Containers, selects, and concat
    /// recurse natively (scalars cost nothing); dep-like leaves configure
    /// just the leaf value and traverse it, reusing the per-dep-kind
    /// dispatch of the owned path.
    ///
    /// Concat items are validated as `ConfiguredAttr::concat` would (one
    /// concat-able kind, equal oneof variants, no duplicate dict keys):
    /// configure-time errors must surface here, before the node exists,
    /// because later attr reads assume configuration succeeds.
    pub fn traverse_configured(
        &self,
        t: &AttrType,
        pkg: PackageLabel,
        ctx: &dyn AttrConfigurationContext,
        traversal: &mut dyn ConfiguredAttrTraversal,
        attr_name: Option<&str>,
    ) -> ResultMaybeCompatible<()> {
        let kind = self.arena.kind_of(self.r);
        if kind.is_scalar() && !kind.is_select_outcome() {
            return ResultMaybeCompatible::Compatible(());
        }
        match self.kind() {
            kind @ (CoercedAttrRefKind::SelectFail(_)
            | CoercedAttrRefKind::SelectIncompatible(_)) => Self::select_outcome(&kind, ctx)
                .expect("should be a select outcome: the arm matched one"),
            CoercedAttrRefKind::Selector { entries, default } => {
                match Self::select_view(ctx, entries, default) {
                    Ok(Some(selected)) => {
                        selected.traverse_configured(t, pkg, ctx, traversal, attr_name)
                    }
                    Ok(None) => Self::select_no_match_error(ctx, entries).into(),
                    Err(e) => e.into(),
                }
            }
            CoercedAttrRefKind::Concat(items) => {
                let mut leaves: SmallVec<[CoercedAttrRef<'a>; 4]> = SmallVec::new();
                for item in items.iter() {
                    item.concat_leaves(ctx, &mut leaves)?;
                }
                if leaves.len() > 1
                    && let Err(e) = Self::validate_concat(t, &leaves)
                {
                    return e.into();
                }
                for leaf in leaves {
                    leaf.traverse_configured(t, pkg, ctx, traversal, attr_name)?;
                }
                ResultMaybeCompatible::Compatible(())
            }
            CoercedAttrRefKind::Visibility(_)
            | CoercedAttrRefKind::WithinView(_)
            | CoercedAttrRefKind::Modifiers(_)
            | CoercedAttrRefKind::Metadata(_) => ResultMaybeCompatible::Compatible(()),
            kind => Self::traverse_configured_typed(kind, t, pkg, ctx, traversal, attr_name),
        }
    }

    fn traverse_configured_typed(
        kind: CoercedAttrRefKind<'a>,
        t: &AttrType,
        pkg: PackageLabel,
        ctx: &dyn AttrConfigurationContext,
        traversal: &mut dyn ConfiguredAttrTraversal,
        attr_name: Option<&str>,
    ) -> ResultMaybeCompatible<()> {
        match &t.0.inner {
            AttrTypeInner::Option(opt) => {
                return Self::traverse_configured_typed(
                    kind, &opt.inner, pkg, ctx, traversal, attr_name,
                );
            }
            // `Any`-typed values cannot contain deps (they coerce to
            // scalars/containers of strings), matching `configure_any`.
            AttrTypeInner::Any(_) => return ResultMaybeCompatible::Compatible(()),
            inner => match (kind, inner) {
                // Scalars configure to themselves: nothing to visit, and the
                // owned path's per-string `ArcStr` materialization is skipped.
                (
                    CoercedAttrRefKind::None
                    | CoercedAttrRefKind::Bool(_)
                    | CoercedAttrRefKind::Int(_)
                    | CoercedAttrRefKind::String(_)
                    | CoercedAttrRefKind::EnumVariant(_)
                    | CoercedAttrRefKind::ArgSimple(_),
                    _,
                ) => {}
                (CoercedAttrRefKind::List(items), AttrTypeInner::List(ty)) => {
                    for v in items.iter() {
                        v.traverse_configured(&ty.inner, pkg, ctx, traversal, attr_name)?;
                    }
                }
                (CoercedAttrRefKind::Tuple(items), AttrTypeInner::Tuple(ty)) => {
                    if items.len() != ty.xs.len() {
                        return internal_error!("Inconsistent number of elements in tuple").into();
                    }
                    for (v, vt) in items.iter().zip(&ty.xs) {
                        v.traverse_configured(vt, pkg, ctx, traversal, attr_name)?;
                    }
                }
                (CoercedAttrRefKind::Dict(pairs), AttrTypeInner::Dict(ty)) => {
                    for (k, v) in pairs.iter() {
                        k.traverse_configured(&ty.key, pkg, ctx, traversal, attr_name)?;
                        v.traverse_configured(&ty.value, pkg, ctx, traversal, attr_name)?;
                    }
                }
                (CoercedAttrRefKind::OneOf(v, i), AttrTypeInner::OneOf(ty)) => {
                    let item_ty = match ty.xs.get(i as usize) {
                        Some(ty) => ty,
                        None => return internal_error!("invalid oneof index").into(),
                    };
                    v.traverse_configured(item_ty, pkg, ctx, traversal, attr_name)?;
                }
                (CoercedAttrRefKind::SourceFile(s), AttrTypeInner::Source(_)) => {
                    for x in s.inputs() {
                        if let Err(e) = traversal.input(SourcePathRef::new(pkg, x)) {
                            return e.into();
                        }
                    }
                }
                (CoercedAttrRefKind::SourceLabel(s), AttrTypeInner::Source(_)) => {
                    let configured = s.configure_pair(ctx.cfg().cfg_pair().dupe());
                    if let Err(e) = traversal.dep(&configured) {
                        return e.into();
                    }
                }
                (CoercedAttrRefKind::PluginDep(d), AttrTypeInner::PluginDep(ty)) => {
                    if let Err(e) = traversal.plugin_dep(d, ty.kind()) {
                        return e.into();
                    }
                }
                // Remaining dep-like leaves: configure the leaf only (one
                // small owned value) and reuse its traverse dispatch. Also
                // the error path for mismatched kind/type combinations.
                (leaf, _) => {
                    let configured = Self::configure_typed(leaf, t, ctx, attr_name)?;
                    if let Err(e) = configured.traverse(pkg, traversal) {
                        return e.into();
                    }
                }
            },
        }
        ResultMaybeCompatible::Compatible(())
    }

    /// Flattens one concat item into the leaves `ConfiguredAttr::concat`
    /// would see: selects resolved, nested concats inlined.
    fn concat_leaves(
        &self,
        ctx: &dyn AttrConfigurationContext,
        out: &mut SmallVec<[CoercedAttrRef<'a>; 4]>,
    ) -> ResultMaybeCompatible<()> {
        match self.kind() {
            kind @ (CoercedAttrRefKind::SelectFail(_)
            | CoercedAttrRefKind::SelectIncompatible(_)) => Self::select_outcome(&kind, ctx)
                .expect("should be a select outcome: the arm matched one"),
            CoercedAttrRefKind::Selector { entries, default } => {
                match Self::select_view(ctx, entries, default) {
                    Ok(Some(selected)) => selected.concat_leaves(ctx, out),
                    Ok(None) => Self::select_no_match_error(ctx, entries).into(),
                    Err(e) => e.into(),
                }
            }
            CoercedAttrRefKind::Concat(items) => {
                for item in items.iter() {
                    item.concat_leaves(ctx, out)?;
                }
                ResultMaybeCompatible::Compatible(())
            }
            _ => {
                out.push(*self);
                ResultMaybeCompatible::Compatible(())
            }
        }
    }

    /// The checks `ConfiguredAttr::concat` makes on its items, applied to
    /// the resolved leaves: one concat-able kind throughout, equal oneof
    /// variants, no dict key on both sides.
    fn validate_concat(t: &AttrType, leaves: &[CoercedAttrRef<'a>]) -> buck2_error::Result<()> {
        let Some((first, rest)) = leaves.split_first() else {
            return Ok(());
        };
        match &t.0.inner {
            AttrTypeInner::Option(opt) => return Self::validate_concat(&opt.inner, leaves),
            AttrTypeInner::OneOf(oneof) => {
                let mut inner: SmallVec<[CoercedAttrRef<'a>; 4]> =
                    SmallVec::with_capacity(leaves.len());
                let mut variant = None;
                for leaf in leaves {
                    let CoercedAttrRefKind::OneOf(v, i) = leaf.kind() else {
                        return Err(internal_error!(
                            "expecting oneof variant, got: {}",
                            leaf.as_display_no_ctx()
                        ));
                    };
                    match variant {
                        None => variant = Some(i),
                        Some(first_i) if first_i != i => {
                            return Err(buck2_error!(
                                buck2_error::ErrorTag::Input,
                                "Cannot concatenate values coerced/configured \
                                to different oneof variants: `{}` and `{}`",
                                oneof.get(first_i)?,
                                oneof.get(i)?
                            ));
                        }
                        Some(_) => {}
                    }
                    inner.push(v);
                }
                return match variant {
                    Some(i) => Self::validate_concat(oneof.get(i)?, &inner),
                    None => Ok(()),
                };
            }
            _ => {}
        }
        match first.kind() {
            CoercedAttrRefKind::List(_) => {
                for leaf in rest {
                    if !matches!(leaf.kind(), CoercedAttrRefKind::List(_)) {
                        return Err(leaf.concat_not_supported("list"));
                    }
                }
            }
            CoercedAttrRefKind::Dict(first_pairs) => {
                let mut seen: Vec<CoercedAttrRef<'a>> =
                    first_pairs.iter().map(|(k, _)| k).collect();
                for leaf in rest {
                    let CoercedAttrRefKind::Dict(pairs) = leaf.kind() else {
                        return Err(leaf.concat_not_supported("dict"));
                    };
                    for (k, _) in pairs.iter() {
                        if seen.iter().any(|s| s.r == k.r || s.content_eq(&k)) {
                            return Err(buck2_error!(
                                buck2_error::ErrorTag::Input,
                                "got same key in both sides of dictionary concat (key `{}`)",
                                k.as_display_no_ctx()
                            ));
                        }
                        seen.push(k);
                    }
                }
            }
            CoercedAttrRefKind::String(_) => {
                for leaf in rest {
                    if !matches!(leaf.kind(), CoercedAttrRefKind::String(_)) {
                        return Err(leaf.concat_not_supported("string"));
                    }
                }
            }
            CoercedAttrRefKind::ArgSimple(_) | CoercedAttrRefKind::Arg(_) => {
                for leaf in rest {
                    if !matches!(
                        leaf.kind(),
                        CoercedAttrRefKind::ArgSimple(_) | CoercedAttrRefKind::Arg(_)
                    ) {
                        return Err(leaf.concat_not_supported("arg"));
                    }
                }
            }
            CoercedAttrRefKind::OneOf(..) => {
                return Err(internal_error!(
                    "Inconsistent attr value (`{}`) and attr type (`{}`)",
                    first.as_display_no_ctx(),
                    t
                ));
            }
            _ => {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "addition not supported for this attribute type `{}`",
                    first.as_display_no_ctx()
                ));
            }
        }
        Ok(())
    }

    fn concat_not_supported(&self, attr_ty: &'static str) -> buck2_error::Error {
        buck2_error!(
            buck2_error::ErrorTag::Input,
            "addition not supported for these attribute type `{}` and value `{}`",
            attr_ty,
            self.as_display_no_ctx()
        )
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// The value's JSON rendering.
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
            CoercedAttrRefKind::Arg(parts) => Ok(to_value(parts.to_string())?),
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
            CoercedAttrRefKind::Query(q) => Ok(to_value(q.query().0)?),
            CoercedAttrRefKind::SelectFail(s) => CoercedSelector::fail_to_json(s),
            CoercedAttrRefKind::SelectIncompatible(s) => CoercedSelector::incompatible_to_json(s),
            CoercedAttrRefKind::ExplicitConfiguredDep { label, platform } => {
                Ok(to_value([label.to_string(), platform.to_string()])?)
            }
            CoercedAttrRefKind::TransitionDep { dep, transition } => match transition {
                Some(tr) => Ok(to_value([dep.to_string(), tr.to_string()])?),
                None => Ok(to_value(dep.to_string())?),
            },
            CoercedAttrRefKind::Visibility(v) => Ok(v.to_json()),
            CoercedAttrRefKind::WithinView(v) => Ok(v.to_json()),
            CoercedAttrRefKind::Modifiers(m) => Ok(m.to_json()),
            CoercedAttrRefKind::Metadata(m) => Ok(m.to_value()),
        }
    }

    /// Whether any leaf's string form matches the filter.
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
            CoercedAttrRefKind::Arg(parts) => filter(&parts.to_string()),
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
            CoercedAttrRefKind::Query(q) => filter(&q.query().0),
            CoercedAttrRefKind::SelectFail(_) | CoercedAttrRefKind::SelectIncompatible(_) => {
                Ok(false)
            }
            CoercedAttrRefKind::ExplicitConfiguredDep { label, platform } => {
                filter(&format!("({label}, {platform})"))
            }
            CoercedAttrRefKind::TransitionDep { dep, .. } => filter(&dep.to_string()),
            CoercedAttrRefKind::Visibility(v) => v.any_matches(filter),
            CoercedAttrRefKind::WithinView(v) => v.any_matches(filter),
            CoercedAttrRefKind::Modifiers(m) => m.any_matches(filter),
            CoercedAttrRefKind::Metadata(m) => m.any_matches(filter),
        }
    }

    /// The configuration-dep label, or `None` for an explicit `None` value.
    ///
    /// Errors on any other kind: only for attributes the coercer has already
    /// verified to be a (non-configurable) configuration dep.
    pub fn unpack_configuration_dep(&self) -> buck2_error::Result<Option<&'a ProvidersLabel>> {
        if self.parts::<kinds::None>().is_some() {
            return Ok(None);
        }
        self.parts::<kinds::ConfigurationDep>()
            .map(|(l,)| Some(l))
            .ok_or_else(|| {
                internal_error!("should be a configuration dep: verified by the attr coercer")
            })
    }

    /// The list elements, or `None` when the value is not a list.
    pub fn unpack_list(&self) -> Option<impl Iterator<Item = CoercedAttrRef<'a>> + use<'a>> {
        self.parts::<kinds::List>().map(|(items,)| items.iter())
    }

    /// The string content, or `None` when the value is not a string.
    pub fn unpack_str(&self) -> Option<&'a str> {
        self.parts::<kinds::String>().map(|(s,)| s)
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
            CoercedAttrRefKind::Arg(parts) => write!(f, "\"{parts}\""),
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
            CoercedAttrRefKind::Query(q) => write!(f, "\"{}\"", q.query().0),
            CoercedAttrRefKind::SelectFail(s) => write!(f, "select_fail(\"{s}\")"),
            CoercedAttrRefKind::SelectIncompatible(s) => write!(f, "select_incompatible(\"{s}\")"),
            CoercedAttrRefKind::ExplicitConfiguredDep { label, platform } => {
                write!(f, "({label}, {platform})")
            }
            CoercedAttrRefKind::TransitionDep { dep, .. } => std::fmt::Display::fmt(dep, f),
            CoercedAttrRefKind::Visibility(v) => std::fmt::Display::fmt(v, f),
            CoercedAttrRefKind::WithinView(v) => std::fmt::Display::fmt(v, f),
            CoercedAttrRefKind::Modifiers(m) => write!(f, "{m}"),
            CoercedAttrRefKind::Metadata(m) => write!(f, "{m}"),
        }
    }
}

impl std::fmt::Debug for CoercedAttrRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CoercedAttrRef({})", self.as_display_no_ctx())
    }
}

/// An attribute value handed to a callback: a normal attr, which the package
/// arena stores, or a special attr, which is built per call and served as
/// built — nothing reads a special attr's identity, so nothing is retained.
#[derive(Debug)]
pub enum AttrView<'a> {
    Normal(CoercedAttrRef<'a>),
    Special(SpecialAttr),
}

impl AttrView<'_> {
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        match self {
            AttrView::Normal(r) => r.any_matches(filter),
            AttrView::Special(a) => a.any_matches(filter),
        }
    }
}

impl crate::attrs::display::AttrDisplayWithContext for AttrView<'_> {
    fn fmt(&self, ctx: &AttrFmtContext, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttrView::Normal(r) => AttrDisplayWithContext::fmt(r, ctx, f),
            AttrView::Special(a) => AttrDisplayWithContext::fmt(a, ctx, f),
        }
    }
}

impl AttrSerializeWithContext for AttrView<'_> {
    fn serialize_with_ctx<S>(&self, ctx: &AttrFmtContext, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            AttrView::Normal(r) => r.serialize_with_ctx(ctx, s),
            AttrView::Special(a) => a.to_json().map_err(serde::ser::Error::custom)?.serialize(s),
        }
    }
}

/// A value rendered in place of its parent, so nested values serialize without
/// an intermediate tree.
struct AttrRefSerialize<'a> {
    attr: CoercedAttrRef<'a>,
    ctx: &'a AttrFmtContext,
}

impl Serialize for AttrRefSerialize<'_> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.attr.serialize_with_ctx(self.ctx, s)
    }
}

impl<'a> CoercedAttrRef<'a> {
    /// The value's rendering when it is used as a JSON object key.
    ///
    /// Errors for values that render as anything but a string.
    fn json_key(&self, ctx: &AttrFmtContext) -> buck2_error::Result<Cow<'a, str>> {
        Ok(match self.kind() {
            CoercedAttrRefKind::String(s)
            | CoercedAttrRefKind::EnumVariant(s)
            | CoercedAttrRefKind::ArgSimple(s) => Cow::Borrowed(s),
            CoercedAttrRefKind::Arg(parts) => Cow::Owned(parts.to_string()),
            CoercedAttrRefKind::OneOf(l, _) => l.json_key(ctx)?,
            CoercedAttrRefKind::Dep(l)
            | CoercedAttrRefKind::Label(l)
            | CoercedAttrRefKind::SourceLabel(l)
            | CoercedAttrRefKind::ConfigurationDep(l)
            | CoercedAttrRefKind::SplitTransitionDep(l) => Cow::Owned(l.to_string()),
            CoercedAttrRefKind::PluginDep(t) => Cow::Owned(t.to_string()),
            CoercedAttrRefKind::SourceFile(p) => {
                Cow::Owned(source_file_display(ctx, p).to_string())
            }
            CoercedAttrRefKind::Visibility(_)
            | CoercedAttrRefKind::WithinView(_)
            | CoercedAttrRefKind::Modifiers(_)
            | CoercedAttrRefKind::Metadata(_) => {
                return Err(internal_error!("dict keys must render to JSON strings"));
            }
            _ => return Err(internal_error!("dict keys must render to JSON strings")),
        })
    }
}

impl AttrSerializeWithContext for CoercedAttrRef<'_> {
    fn serialize_with_ctx<S>(&self, ctx: &AttrFmtContext, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        fn err<S: Serializer>(e: buck2_error::Error) -> S::Error {
            serde::ser::Error::custom(format!("{e}"))
        }
        // Keyed values collect into a `BTreeMap` to match `serde_json::Map`:
        // keys emit in sorted order, and a repeated key keeps its last value.
        fn keyed<'a, S: Serializer>(
            entries: impl Iterator<Item = (Cow<'a, str>, CoercedAttrRef<'a>)>,
            ctx: &'a AttrFmtContext,
            s: S,
        ) -> Result<S::Ok, S::Error> {
            let sorted: BTreeMap<Cow<'a, str>, CoercedAttrRef<'a>> = entries.collect();
            let mut map = s.serialize_map(Some(sorted.len()))?;
            for (k, v) in sorted {
                map.serialize_entry(&k, &AttrRefSerialize { attr: v, ctx })?;
            }
            map.end()
        }
        fn seq<'a, S: Serializer>(
            items: Refs<'a, Values>,
            ctx: &'a AttrFmtContext,
            s: S,
        ) -> Result<S::Ok, S::Error> {
            let mut seq = s.serialize_seq(Some(items.len()))?;
            for v in items.iter() {
                seq.serialize_element(&AttrRefSerialize { attr: v, ctx })?;
            }
            seq.end()
        }

        match self.kind() {
            CoercedAttrRefKind::None => s.serialize_unit(),
            CoercedAttrRefKind::Bool(b) => s.serialize_bool(b),
            CoercedAttrRefKind::Int(i) => s.serialize_i64(i),
            CoercedAttrRefKind::String(v)
            | CoercedAttrRefKind::EnumVariant(v)
            | CoercedAttrRefKind::ArgSimple(v) => s.serialize_str(v),
            CoercedAttrRefKind::Arg(parts) => s.serialize_str(&parts.to_string()),
            CoercedAttrRefKind::List(items) | CoercedAttrRefKind::Tuple(items) => {
                seq(items, ctx, s)
            }
            CoercedAttrRefKind::Dict(pairs) => keyed(
                pairs
                    .iter()
                    .map(|(k, v)| Ok((k.json_key(ctx)?, v)))
                    .collect::<buck2_error::Result<Vec<_>>>()
                    .map_err(err::<S>)?
                    .into_iter(),
                ctx,
                s,
            ),
            CoercedAttrRefKind::Selector { entries, default } => {
                let mut map = s.serialize_map(Some(2))?;
                map.serialize_entry("__type", "selector")?;
                struct Entries<'a> {
                    entries: AttrRefSelectorEntries<'a>,
                    default: Option<CoercedAttrRef<'a>>,
                    ctx: &'a AttrFmtContext,
                }
                impl Serialize for Entries<'_> {
                    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
                    where
                        S: Serializer,
                    {
                        keyed(
                            self.entries
                                .iter()
                                .map(|(k, v)| (Cow::Owned(k.to_string()), v))
                                .chain(self.default.map(|d| (Cow::Borrowed("DEFAULT"), d))),
                            self.ctx,
                            s,
                        )
                    }
                }
                map.serialize_entry(
                    "entries",
                    &Entries {
                        entries,
                        default,
                        ctx,
                    },
                )?;
                map.end()
            }
            CoercedAttrRefKind::Concat(items) => {
                let mut map = s.serialize_map(Some(2))?;
                map.serialize_entry("__type", "concat")?;
                struct Items<'a> {
                    items: Refs<'a, Values>,
                    ctx: &'a AttrFmtContext,
                }
                impl Serialize for Items<'_> {
                    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
                    where
                        S: Serializer,
                    {
                        seq(self.items, self.ctx, s)
                    }
                }
                map.serialize_entry("items", &Items { items, ctx })?;
                map.end()
            }
            CoercedAttrRefKind::OneOf(l, _) => l.serialize_with_ctx(ctx, s),
            CoercedAttrRefKind::Dep(l)
            | CoercedAttrRefKind::Label(l)
            | CoercedAttrRefKind::SourceLabel(l)
            | CoercedAttrRefKind::ConfigurationDep(l)
            | CoercedAttrRefKind::SplitTransitionDep(l) => s.serialize_str(&l.to_string()),
            CoercedAttrRefKind::PluginDep(t) => s.serialize_str(&t.to_string()),
            CoercedAttrRefKind::SourceFile(p) => {
                s.serialize_str(&source_file_display(ctx, p).to_string())
            }
            CoercedAttrRefKind::SelectFail(m) => CoercedSelector::fail_to_json(m)
                .map_err(err::<S>)?
                .serialize(s),
            CoercedAttrRefKind::SelectIncompatible(m) => CoercedSelector::incompatible_to_json(m)
                .map_err(err::<S>)?
                .serialize(s),
            CoercedAttrRefKind::ExplicitConfiguredDep { label, platform } => {
                [label.to_string(), platform.to_string()].serialize(s)
            }
            CoercedAttrRefKind::TransitionDep { dep, transition } => match transition {
                Some(tr) => [dep.to_string(), tr.to_string()].serialize(s),
                None => s.serialize_str(&dep.to_string()),
            },
            CoercedAttrRefKind::Query(q) => s.serialize_str(&q.query().0),
            CoercedAttrRefKind::Visibility(v) => v.to_json().serialize(s),
            CoercedAttrRefKind::WithinView(v) => v.to_json().serialize(s),
            CoercedAttrRefKind::Modifiers(m) => m.to_json().serialize(s),
            CoercedAttrRefKind::Metadata(m) => m.to_value().serialize(s),
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
    /// except the whole-value kinds, whose table index is what the probe
    /// finds.
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
    path_by_ptr: hashbrown::HashTable<(ArcS<PackageRelativePath>, u32, u64)>,
    /// Whole-value tables' content dedup: value hash to table index.
    visibility_dedup: hashbrown::HashTable<(u64, u32)>,
    within_view_dedup: hashbrown::HashTable<(u64, u32)>,
    modifiers_dedup: hashbrown::HashTable<(u64, u32)>,
    metadata_dedup: hashbrown::HashTable<(u64, u32)>,
    /// Words interned as roots (top-level attribute values); the sealed
    /// arena retains an identity-grade strong hash for exactly these.
    roots: Vec<AttrWord>,
}

/// The builder's result for one value: a ref plus the content hash a parent
/// folds into its own.
fn token((r, h): (AttrWord, CoercedAttrHash)) -> AttrToken {
    AttrToken(r, h)
}

fn content_hash(tag: AttrKind, f: impl FnOnce(&mut BuckHasher)) -> CoercedAttrHash {
    let mut h = BuckHasher::default();
    Hash::hash(&(tag as u8), &mut h);
    f(&mut h);
    CoercedAttrHash(h.finish())
}

/// Bucket hash for the source-file lookaside: mixes the `ArcS` data
/// address (identity, not content).
fn path_addr_hash(f: &ArcS<PackageRelativePath>) -> u64 {
    mix_addr(f.as_str().as_ptr().addr())
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

/// Head fields of one record while encoding; `SelectorKeys` and
/// `QueryExpr` heads can run long, everything else is one to three fields.
type EncodeScratchFields = SmallVec<[u32; 32]>;

/// Child refs gathered alongside [`EncodeScratchFields`]: covers values with up
/// to 32 children on the stack — beyond typical select/list/dict fan-out.
type EncodeScratchRefs = SmallVec<[AttrWord; 32]>;

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
        arena.visibility.shrink_to_fit();
        arena.within_view.shrink_to_fit();
        arena.metadata.shrink_to_fit();
        arena.modifiers.shrink_to_fit();

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

    /// Mark an encoded value as a root: a top-level attribute value referenced
    /// from outside the arena, for which the sealed arena retains an
    /// identity-grade strong hash.
    pub fn root(&mut self, token: AttrToken) -> AttrWord {
        self.roots.push(token.0);
        token.0
    }

    /// Interns a whole value into its table, one entry per distinct value:
    /// the table index and the value's content hash.
    fn intern_whole<T: WholeTable>(&mut self, v: &T) -> (u32, u64) {
        let mut h = BuckHasher::default();
        v.hash(&mut h);
        let hash = h.finish();
        let (dedup, table) = T::table_mut(self);
        if let Some(&(_, i)) = dedup.find(hash, |&(h, i)| h == hash && table[i as usize] == *v) {
            return (i, hash);
        }
        let i = table.len() as u32;
        table.push(v.clone());
        dedup.insert_unique(hash, (hash, i), |&(h, _)| h);
        (i, hash)
    }

    /// Writes one gathered value per its kind's storage class.
    fn emit(&mut self, storage: Storage, w: RecordWriter<'_>) -> buck2_error::Result<AttrToken> {
        match storage {
            Storage::Text => {
                let text = w
                    .text
                    .expect("should have text: text storage has a `Text` field");
                self.encode_str(w.kind, text).map(token)
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
    fn encode_selector_keys<'k>(
        &mut self,
        keys: impl ExactSizeIterator<Item = &'k ProvidersLabel>,
    ) -> buck2_error::Result<(AttrWord, CoercedAttrHash)> {
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
        Ok((r, keys_hash))
    }

    pub fn encode_bool(&mut self, b: bool) -> buck2_error::Result<AttrToken> {
        if b {
            encode_kind::<kinds::BoolTrue>(self, ())
        } else {
            encode_kind::<kinds::BoolFalse>(self, ())
        }
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

    /// Encode an `attrs.query()` value as its parsed expression.
    pub fn encode_query(
        &mut self,
        q: &QueryAttrBase<ProvidersLabel>,
    ) -> buck2_error::Result<AttrToken> {
        let parsed = buck2_query_parser::parse_expr(&q.query)?;
        let expr = self.encode_query_expr(&parsed, &q.query, &q.resolved_literals)?;
        self.encode_parsed_query(expr)
    }

    /// Copy a value out of another arena into this one, node by node through
    /// the per-kind encoders: the token this returns has the same content and
    /// identity hash as `v`, deduped against what this arena already holds.
    pub fn encode_view(&mut self, v: CoercedAttrRef<'_>) -> buck2_error::Result<AttrToken> {
        dispatch(v.arena.kind_of(v.r), CopyParts { v, b: self })
    }

    /// [`Self::encode_arg_part`] from a decoded part: the same records and
    /// the same hashes, so copies dedup against parsed originals.
    fn encode_arg_part_view(
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

    /// [`Self::encode_query_expr_inner`] from a decoded expression: node for
    /// node and hash for hash.
    fn encode_query_expr_view(
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

    /// Path table index and content hash, via the `ArcS` pointer lookaside
    /// for files (see `path_by_ptr`).
    fn source_path_idx_and_hash(&mut self, p: &CoercedPath) -> (u32, u64) {
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
        let idx = self.arena.paths.len() as u32;
        self.arena.paths.push(p.clone());
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

    /// Encode one arg part: string parts are `ArgSimple` text records;
    /// macro parts dedup as `ArgMacro` records with labels in the label
    /// table.
    fn encode_arg_part(
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

    fn arg_macro_head(
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

    /// Encode one parsed query expression node. Literal text (from
    /// `resolved_literals`) becomes providers-label refs; every other text
    /// becomes a shared text record.
    ///
    /// Coercion records one literal per distinct text, keyed by the span of
    /// its first occurrence, and evaluation resolves literals by text; so a
    /// token is a literal whenever its text matches a recorded one, repeated
    /// occurrences included.
    fn encode_query_expr(
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
    fn encode_query_node(
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

    fn encode_query_word<'s>(
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

    fn encode_query_expr_inner<'s>(
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
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use buck2_core::provider::label::testing::ProvidersLabelTestExt;

    use super::*;
    use crate::attrs::attr_type::arg::UnconfiguredMacro;
    use crate::attrs::display::AttrDisplayWithContextExt;
    use crate::visibility::VisibilitySpecification;

    fn label(pkg: &str, name: &str) -> ProvidersLabel {
        ProvidersLabel::testing_new("root", pkg, name, None)
    }

    /// A test value: the recipe that encodes it into a builder, so one
    /// value can be encoded into any number of arenas.
    type V = Rc<dyn Fn(&mut CoercedAttrArenaBuilder) -> buck2_error::Result<AttrToken>>;

    fn v(
        f: impl Fn(&mut CoercedAttrArenaBuilder) -> buck2_error::Result<AttrToken> + 'static,
    ) -> V {
        Rc::new(f)
    }

    fn put(b: &mut CoercedAttrArenaBuilder, xs: &[V]) -> buck2_error::Result<Vec<AttrToken>> {
        xs.iter().map(|x| x(b)).collect()
    }

    fn none() -> V {
        v(|b| b.encode_none())
    }
    fn boolean(x: bool) -> V {
        v(move |b| b.encode_bool(x))
    }
    fn int(x: i64) -> V {
        v(move |b| b.encode_int(x))
    }
    fn s(x: &'static str) -> V {
        v(move |b| b.encode_string(x))
    }
    fn enum_variant(x: &'static str) -> V {
        v(move |b| b.encode_enum_variant(x))
    }
    fn select_fail(x: &'static str) -> V {
        v(move |b| b.encode_select_fail(x))
    }
    fn select_incompatible(x: &'static str) -> V {
        v(move |b| b.encode_select_incompatible(x))
    }
    fn arg_simple(x: &'static str) -> V {
        arg_value(StringWithMacros::StringPart(ArcStr::from(x)))
    }
    fn arg(parts: Vec<StringWithMacrosPart<ProvidersLabel>>) -> V {
        arg_value(StringWithMacros::ManyParts(parts.into_boxed_slice()))
    }
    fn arg_value(x: StringWithMacros<ProvidersLabel>) -> V {
        v(move |b| b.encode_arg(&x))
    }
    fn query(x: QueryAttrBase<ProvidersLabel>) -> V {
        v(move |b| b.encode_query(&x))
    }
    fn list(xs: Vec<V>) -> V {
        v(move |b| {
            let t = put(b, &xs)?;
            b.encode_list(&t)
        })
    }
    fn tuple(xs: Vec<V>) -> V {
        v(move |b| {
            let t = put(b, &xs)?;
            b.encode_tuple(&t)
        })
    }
    fn concat(xs: Vec<V>) -> V {
        v(move |b| {
            let t = put(b, &xs)?;
            b.encode_concat(&t)
        })
    }
    fn dict(pairs: Vec<(V, V)>) -> V {
        v(move |b| {
            let mut t = Vec::with_capacity(pairs.len());
            for (k, x) in &pairs {
                t.push((k(b)?, x(b)?));
            }
            b.encode_dict(&t)
        })
    }
    fn one_of(x: V, idx: u32) -> V {
        v(move |b| {
            let t = x(b)?;
            b.encode_one_of(idx, t)
        })
    }
    /// A selector keyed by `root//cfg:<key>` labels.
    fn sel(entries: Vec<(&'static str, V)>, default: Option<V>) -> V {
        v(move |b| {
            let mut t = Vec::with_capacity(entries.len());
            for (k, x) in &entries {
                t.push((ConfigurationSettingKey(label("cfg", k)), x(b)?));
            }
            let d = default.as_ref().map(|d| d(b)).transpose()?;
            b.encode_selector(&t, d)
        })
    }
    fn dep(l: ProvidersLabel) -> V {
        v(move |b| b.encode_dep(&l))
    }
    fn label_value(l: ProvidersLabel) -> V {
        v(move |b| b.encode_label(&l))
    }
    fn source_label(l: ProvidersLabel) -> V {
        v(move |b| b.encode_source_label(&l))
    }
    fn configuration_dep(l: ProvidersLabel) -> V {
        v(move |b| b.encode_configuration_dep(&l))
    }
    fn split_transition_dep(l: ProvidersLabel) -> V {
        v(move |b| b.encode_split_transition_dep(&l))
    }
    fn explicit_configured_dep(l: ProvidersLabel, platform: TargetLabel) -> V {
        v(move |b| b.encode_explicit_configured_dep(&l, &platform))
    }
    fn transition_dep(l: ProvidersLabel, transition: Option<ProvidersLabel>) -> V {
        v(move |b| b.encode_transition_dep(&l, transition.as_ref()))
    }
    fn source_file(p: CoercedPath) -> V {
        v(move |b| b.encode_source_file(&p))
    }
    fn visibility(x: VisibilitySpecification) -> V {
        v(move |b| b.encode_visibility(&x))
    }
    fn metadata(x: MetadataMap) -> V {
        v(move |b| b.encode_metadata(&x))
    }
    fn target_modifiers(x: CfgModifiers) -> V {
        v(move |b| b.encode_target_modifiers(&x))
    }

    /// Encode each value and check what every value must satisfy:
    /// re-encoding it into the same arena dedups to the same ref, and a
    /// copy into a fresh arena is content-equal with the same identity hash
    /// and rendering.
    fn roundtrip(values: &[V]) -> CoercedAttrArena {
        let (arena, refs) = encode_all(values);
        assert_copies(&arena, &refs);
        arena
    }

    /// Encode `values` into one arena, in order.
    fn encode_all(values: &[V]) -> (CoercedAttrArena, Vec<AttrWord>) {
        let mut b = CoercedAttrArena::builder();
        let refs: Vec<AttrWord> = values.iter().map(|x| x(&mut b).unwrap().0).collect();
        for (x, &r) in values.iter().zip(&refs) {
            assert_eq!(
                x(&mut b).unwrap().0,
                r,
                "re-encoding should dedup to the same ref: {}",
                b.view(r).as_display_no_ctx()
            );
        }
        (b.finish(), refs)
    }

    /// Encode one value into a fresh arena.
    fn encode_one(value: &V) -> (CoercedAttrArena, AttrWord) {
        let (arena, refs) = encode_all(std::slice::from_ref(value));
        (arena, refs[0])
    }

    /// The query expressions a value holds, as `attrs.query()` values or
    /// query macros: each as its reprint and literal bindings.
    fn queries_of(
        view: CoercedAttrRef<'_>,
    ) -> Vec<(String, ResolvedQueryLiterals<ProvidersLabel>)> {
        match view.kind() {
            CoercedAttrRefKind::Query(q) => vec![q.query()],
            CoercedAttrRefKind::Arg(parts) => parts
                .iter()
                .filter_map(|p| match p {
                    ArgPartView::Macro {
                        mac: ArgMacroView::Query(m),
                        ..
                    } => Some(m.query()),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Round-trip for values whose decode is a canonical reprint (query
    /// expressions): bytes may differ from the source, but every literal
    /// label must survive with a span that slices the reprint to that
    /// label's rendering, and the reprint must be a fixed point.
    fn roundtrip_canonical(values: &[V]) -> CoercedAttrArena {
        let (arena, refs) = encode_all(values);
        assert_copies(&arena, &refs);
        for &r in &refs {
            for (reprint, lits) in queries_of(arena.view(r)) {
                for ((off, len), l) in lits.0.iter() {
                    assert_eq!(
                        &reprint[*off..*off + *len],
                        l.to_string(),
                        "literal span should slice the reprint to its label"
                    );
                }
                let again = QueryAttrBase {
                    query: reprint.clone(),
                    resolved_literals: lits.clone(),
                };
                let mut b2 = CoercedAttrArena::builder();
                let r2 = b2.encode_query(&again).unwrap().0;
                let arena2 = b2.finish();
                assert_eq!(
                    queries_of(arena2.view(r2)),
                    vec![(reprint, lits)],
                    "canonical form should be a fixed point"
                );
            }
        }
        arena
    }

    /// `encode_view` into a fresh arena reproduces each value: same content,
    /// same identity hash, same rendering.
    fn assert_copies(arena: &CoercedAttrArena, refs: &[AttrWord]) {
        for &r in refs {
            let mut b = CoercedAttrArena::builder();
            let AttrToken(c, _) = b.encode_view(arena.view(r)).unwrap();
            let copy = b.finish();
            assert!(
                copy.view(c).content_eq(&arena.view(r)),
                "copy should have the same content: {}",
                arena.view(r).as_display_no_ctx()
            );
            assert_eq!(
                copy.strong_of(c, &mut BuckMutMap::default()),
                arena.strong_of(r, &mut BuckMutMap::default()),
                "copy should have the same identity hash: {}",
                arena.view(r).as_display_no_ctx()
            );
            assert_eq!(
                copy.view(c).as_display_no_ctx().to_string(),
                arena.view(r).as_display_no_ctx().to_string()
            );
        }
    }

    /// Near-miss values must stay distinct under both the identity hash and
    /// content equality: the shared decomposition may not coarsen either.
    #[test]
    fn test_near_miss_values_are_distinct() {
        let l = label("pkg", "t");
        let values = vec![
            none(),
            boolean(true),
            boolean(false),
            int(1),
            int(2),
            s("a"),
            s(""),
            enum_variant("a"),
            arg_simple("a"),
            select_fail("a"),
            select_incompatible("a"),
            list(vec![s("a")]),
            list(vec![s("a"), s("a")]),
            tuple(vec![s("a")]),
            concat(vec![s("a")]),
            dict(vec![(s("a"), int(1))]),
            dep(l.dupe()),
            label_value(l.dupe()),
            source_label(l.dupe()),
            configuration_dep(l.dupe()),
            split_transition_dep(l.dupe()),
            one_of(s("a"), 0),
            one_of(s("a"), 1),
            sel(vec![("a", s("x"))], None),
            sel(vec![("a", s("x"))], Some(s("d"))),
        ];
        roundtrip(&values);
        let mut b = CoercedAttrArena::builder();
        let refs: Vec<AttrWord> = values.iter().map(|x| x(&mut b).unwrap().0).collect();
        let arena = b.finish();
        let mut memo = BuckMutMap::default();
        for (i, &ri) in refs.iter().enumerate() {
            for &rj in &refs[i + 1..] {
                let (vi, vj) = (arena.view(ri), arena.view(rj));
                assert!(
                    !vi.content_eq(&vj),
                    "{} and {} must differ",
                    vi.as_display_no_ctx(),
                    vj.as_display_no_ctx()
                );
                assert_ne!(
                    arena.strong_of(ri, &mut memo),
                    arena.strong_of(rj, &mut memo),
                    "{} and {} must hash apart",
                    vi.as_display_no_ctx(),
                    vj.as_display_no_ctx()
                );
            }
        }
    }

    #[test]
    fn test_page_in_validates_text_records() {
        use pagable::testing::TestingDeserializer;
        use pagable::testing::TestingSerializer;

        let mut b = CoercedAttrArena::builder();
        let r = b.encode_string("héllo").unwrap().0;
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
        let r = list(vec![s("héllo")])(&mut b).unwrap().0;
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
            none(),
            boolean(true),
            boolean(false),
            int(0),
            int(-1),
            int(INT_INLINE_MAX),
            int(INT_INLINE_MIN),
            dep(label("foo", "bar")),
        ]);
        assert_eq!(
            arena.byte_len(),
            0,
            "inline leaves should use no arena bytes"
        );
    }

    #[test]
    fn test_root_strong_hash_identity() {
        let value = list(vec![s("x"), int(3), dep(label("foo", "bar"))]);
        let other = list(vec![s("y")]);

        // Same value in two arenas, interleaved with different neighbors:
        // strong hashes must agree (identity is content, not placement).
        let mut b1 = CoercedAttrArena::builder();
        let t = value(&mut b1).unwrap();
        let r1 = b1.root(t);
        let a1 = b1.finish();

        let mut b2 = CoercedAttrArena::builder();
        let t = other(&mut b2).unwrap();
        let o2 = b2.root(t);
        let t = value(&mut b2).unwrap();
        let r2 = b2.root(t);
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
        let interior = value(&mut b3).unwrap().0;
        let a3 = b3.finish();
        assert_eq!(
            a3.root_strong_hash(interior),
            None,
            "non-root values should not retain strong hashes"
        );

        // OwnedCoercedAttr agrees with a package arena holding the same value.
        let sealed = OwnedCoercedAttr::new_with(|b| value(b)).unwrap();
        assert_eq!(
            sealed.strong_hash(),
            a1.root_strong_hash(r1).unwrap(),
            "sealed storage should produce the same identity hash"
        );
    }

    #[test]
    fn test_big_ints() {
        let arena = roundtrip(&[
            int(INT_INLINE_MAX + 1),
            int(INT_INLINE_MIN - 1),
            int(i64::MAX),
            int(i64::MIN),
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
        let list_v = list(vec![s("x"), int(3), none()]);
        let tuple_v = tuple(vec![list_v.clone(), boolean(false)]);
        let dict_v = dict(vec![(s("k1"), list_v.clone()), (s("k2"), tuple_v.clone())]);
        let oneof_v = one_of(dict_v.clone(), 7);
        roundtrip(&[list_v, tuple_v, dict_v.clone(), oneof_v]);
        let (arena, r) = encode_one(&dict_v);
        assert_eq!(
            arena.view(r).as_display_no_ctx().to_string(),
            "{\"k1\": [\"x\", 3, None], \"k2\": ([\"x\", 3, None], False)}"
        );
    }

    #[test]
    fn test_labels_and_selector() {
        let dep_v = dep(label("foo", "bar"));
        let sel_full = sel(
            vec![("a", s("on-a")), ("b", dep_v.clone())],
            Some(s("dflt")),
        );
        let sel_no_default = sel(vec![("a", s("v"))], None);
        let concat_v = concat(vec![sel_full.clone(), s("suffix")]);
        roundtrip(&[dep_v, sel_full, sel_no_default, concat_v.clone()]);
        let (arena, r) = encode_one(&concat_v);
        assert_eq!(
            arena.view(r).as_display_no_ctx().to_string(),
            "select({\"root//cfg:a\": \"on-a\", \"root//cfg:b\": \"root//foo:bar\", \"DEFAULT\": \"dflt\"})+\"suffix\""
        );
    }

    /// Whole-value leaves intern into their own tables, one entry per
    /// distinct value.
    #[test]
    fn test_whole_value_leaves_intern_into_tables() {
        let vis = visibility(VisibilitySpecification::testing_parse(&["root//a/..."]));
        let meta = metadata(Default::default());
        let arena = roundtrip(&[
            vis.clone(),
            visibility(VisibilitySpecification::DEFAULT),
            vis.clone(),
            meta.clone(),
            meta.clone(),
        ]);
        assert_eq!(arena.visibility.len(), 2);
        assert_eq!(arena.metadata.len(), 1);
    }

    #[test]
    fn test_dep_like_leaves_encode_natively() {
        let configured_dep = explicit_configured_dep(
            label("pkg", "dep"),
            label("platforms", "linux").target().dupe(),
        );
        let fixed_transition = transition_dep(label("pkg", "dep"), None);
        let dynamic_transition =
            transition_dep(label("pkg", "dep"), Some(label("transitions", "t")));
        roundtrip(&[
            configured_dep,
            fixed_transition.clone(),
            dynamic_transition.clone(),
        ]);

        // Same dep with and without a dynamic transition: distinct records.
        let mut b = CoercedAttrArena::builder();
        let AttrToken(r1, h1) = fixed_transition(&mut b).unwrap();
        let AttrToken(r2, h2) = dynamic_transition(&mut b).unwrap();
        assert_ne!(r1, r2);
        assert_ne!(h1, h2);
    }

    fn modifiers(json: serde_json::Value) -> CfgModifiers {
        use buck2_core::cells::CellResolver;
        use buck2_core::cells::cell_root_path::CellRootPathBuf;
        use buck2_core::cells::name::CellName;

        use crate::modifiers::ModifierLabelParser;
        let cells = CellResolver::testing_with_names_and_paths(&[
            (
                CellName::testing_new("root"),
                CellRootPathBuf::testing_new(""),
            ),
            (
                CellName::testing_new("cfg"),
                CellRootPathBuf::testing_new("cfg"),
            ),
        ]);
        CfgModifiers::from_target(&json, &ModifierLabelParser::new(&cells))
    }

    /// `modifiers` values intern into the modifiers table: equal values
    /// share one entry, and the view hands out the shared value.
    #[test]
    fn test_modifiers_intern_into_table() {
        use serde_json::json;
        let json = json!([
            "root//os:linux",
            null,
            "not-a-label",
            42,
            {
                "_type": "ModifiersMatch",
                "DEFAULT": "cfg//compiler:gcc",
                "cfg//os:linux": {"_type": "ModifiersMatch", "cfg//arch:arm": null},
                "cfg//os:windows": "cfg//compiler:msvc[sub]",
            },
        ]);
        let value = target_modifiers(modifiers(json.clone()));
        let non_list = target_modifiers(modifiers(json!({"a": 1})));
        let empty = target_modifiers(CfgModifiers::empty());
        let arena = roundtrip(&[value.clone(), non_list.clone(), empty]);
        assert_eq!(arena.modifiers.len(), 3);

        let mut b = CoercedAttrArena::builder();
        let AttrToken(r, _) = value(&mut b).unwrap();
        let AttrToken(again, _) = target_modifiers(modifiers(json.clone()))(&mut b).unwrap();
        assert_eq!(r, again, "an equal value should intern to the same entry");
        let AttrToken(r2, _) = non_list(&mut b).unwrap();
        let arena = b.finish();
        assert_eq!(arena.modifiers.len(), 2);
        let CoercedAttrRefKind::Modifiers(m) = arena.view(r).kind() else {
            panic!("should decode as modifiers");
        };
        assert_eq!(m.to_json(), json);
        assert_eq!(
            arena.view(r2).to_json(&AttrFmtContext::NO_CONTEXT).unwrap(),
            json!({"a": 1})
        );
    }

    /// `PACKAGE` modifier groups served as `buck.package_cfg_modifiers`
    /// share the package's value.
    #[test]
    fn test_package_modifier_groups_intern_into_table() {
        use buck2_core::cells::CellResolver;
        use buck2_core::cells::cell_root_path::CellRootPathBuf;
        use buck2_core::cells::name::CellName;
        use serde_json::json;

        use crate::modifiers::ModifierLabelParser;
        let cells = CellResolver::testing_with_names_and_paths(&[(
            CellName::testing_new("root"),
            CellRootPathBuf::testing_new(""),
        )]);
        let json = json!([
            {
                "_type": "TaggedModifiers",
                "location": {"_type": "ModifierPackageLocation", "package_path": "root//foo/PACKAGE"},
                "modifiers": ["root//cfg:linux"],
                "rule_name": null,
            },
            {
                "_type": "TaggedModifiers",
                "location": {"_type": "ModifierTargetLocation"},
                "modifiers": [],
                "rule_name": "python_binary",
            },
        ]);
        let package = CfgModifiers::from_package(&json, &ModifierLabelParser::new(&cells), None);
        let value = target_modifiers(package);
        roundtrip(std::slice::from_ref(&value));
        let mut b = CoercedAttrArena::builder();
        let AttrToken(r, _) = value(&mut b).unwrap();
        let arena = b.finish();
        assert_eq!(
            arena.view(r).to_json(&AttrFmtContext::NO_CONTEXT).unwrap(),
            json
        );
    }

    #[test]
    fn test_select_outcomes_are_text_records() {
        let fail = select_fail("nope");
        let incompatible = select_incompatible("nope");
        roundtrip(&[fail.clone(), incompatible.clone()]);

        // Same message, different outcome: distinct values.
        let mut b = CoercedAttrArena::builder();
        let AttrToken(r1, h1) = fail(&mut b).unwrap();
        let AttrToken(r2, h2) = incompatible(&mut b).unwrap();
        assert_ne!(r1, r2);
        assert_ne!(h1, h2);
        let arena = b.finish();
        assert_eq!(
            arena.view(r1).as_display_no_ctx().to_string(),
            "select_fail(\"nope\")"
        );
        assert_eq!(
            arena.view(r2).as_display_no_ctx().to_string(),
            "select_incompatible(\"nope\")"
        );
    }

    #[test]
    fn test_selector_keys_shared_across_values() {
        let entries = |x: &'static str| vec![("a", s(x)), ("b", none())];
        let sel1 = sel(entries("one"), None);
        let sel2 = sel(entries("two"), None);
        roundtrip(&[sel1.clone(), sel2.clone()]);

        let mut b = CoercedAttrArena::builder();
        let AttrToken(r1, _) = sel1(&mut b).unwrap();
        let bytes_after_first = b.arena.bytes.len();
        let AttrToken(r2, _) = sel2(&mut b).unwrap();
        // Second selector shares the keys record and the None value: it
        // adds only its own record (tag, count, 4 one-byte refs: keys,
        // absent default, 2 values) plus the new "two" text record (tag,
        // length, 3 bytes) -- NOT a second keys record.
        let added = b.arena.bytes.len() - bytes_after_first;
        assert_eq!(
            added,
            (1 + 1 + 4) + (1 + 1 + 3),
            "expected keys record to be shared"
        );
        let keys_of = |b: &CoercedAttrArenaBuilder, r: AttrWord| b.arena.open(r).refs().get(0);
        assert_eq!(
            keys_of(&b, r1),
            keys_of(&b, r2),
            "selectors should share one keys record"
        );
    }

    #[test]
    fn test_arg_simple_and_many_parts_encode_natively() {
        roundtrip(&[arg_simple("-DFOO=1"), arg(vec![])]);
        let (arena, r) = encode_one(&arg_simple("-DFOO=1"));
        assert!(matches!(
            arena.view(r).kind(),
            CoercedAttrRefKind::ArgSimple("-DFOO=1")
        ));
    }

    /// An `attrs.query()` value and a query macro over the same expression
    /// share one expression record, so `queries()` sees one query.
    #[test]
    fn test_query_attr_shares_expression_with_query_macro() {
        let text = "deps(:dep)";
        let lits = || ResolvedQueryLiterals(BTreeMap::from([((5, 4), label("pkg", "dep"))]));
        let attr = query(QueryAttrBase {
            query: text.to_owned(),
            resolved_literals: lits(),
        });
        let arg = arg(vec![StringWithMacrosPart::Macro(
            false,
            UnconfiguredMacro::Query(Box::new(QueryMacroBase {
                expansion_type: QueryExpansion::Target,
                query: QueryAttrBase {
                    query: text.to_owned(),
                    resolved_literals: lits(),
                },
            })),
        )]);
        roundtrip_canonical(&[attr.clone(), arg.clone()]);

        let mut b = CoercedAttrArena::builder();
        let AttrToken(ra, _) = attr(&mut b).unwrap();
        let AttrToken(rg, _) = arg(&mut b).unwrap();
        let arena = b.finish();
        let CoercedAttrRefKind::Query(q) = arena.view(ra).kind() else {
            panic!("should decode as a query");
        };
        let CoercedAttrRefKind::Arg(parts) = arena.view(rg).kind() else {
            panic!("should decode as arg parts");
        };
        let ArgPartView::Macro {
            mac: ArgMacroView::Query(m),
            ..
        } = parts.iter().next().unwrap()
        else {
            panic!("should decode as a query macro");
        };
        assert_eq!(q.expr().r, m.expr().r, "expression record should be shared");
        let (reprint, lits) = q.query();
        assert_eq!(reprint, "deps(root//pkg:dep)");
        assert_eq!(lits.0.len(), 1);
        let ((off, len), l) = lits.0.iter().next().unwrap();
        assert_eq!(&reprint[*off..*off + *len], l.to_string());
    }

    #[test]
    fn test_arg_many_parts_roundtrip_native() {
        let parts = vec![
            StringWithMacrosPart::String(ArcStr::from("--flag=")),
            StringWithMacrosPart::Macro(
                false,
                UnconfiguredMacro::Location {
                    label: label("foo", "gen"),
                    dep_kind: MacroDepKind::Toolchain,
                },
            ),
            StringWithMacrosPart::Macro(
                true,
                UnconfiguredMacro::Exe {
                    label: label("tools", "compiler"),
                    exec_dep: true,
                },
            ),
            StringWithMacrosPart::Macro(
                false,
                UnconfiguredMacro::UserUnkeyedPlaceholder(Box::from("CXX")),
            ),
            StringWithMacrosPart::Macro(
                false,
                UnconfiguredMacro::UserKeyedPlaceholder(Box::new((
                    Box::from("cxxppflags"),
                    label("foo", "hdrs"),
                    None,
                ))),
            ),
            StringWithMacrosPart::Macro(
                false,
                UnconfiguredMacro::UserKeyedPlaceholder(Box::new((
                    Box::from("cxxflags"),
                    label("foo", "deps"),
                    Some(Box::from("--x")),
                ))),
            ),
        ];
        let value = arg(parts);
        roundtrip(std::slice::from_ref(&value));
        let (arena, r) = encode_one(&value);
        let CoercedAttrRefKind::Arg(decoded) = arena.view(r).kind() else {
            panic!("should decode as arg parts");
        };
        let decoded: Vec<ArgPartView<'_>> = decoded.iter().collect();
        assert_eq!(decoded.len(), 6);
        assert!(decoded[0] == ArgPartView::String("--flag="));
        assert!(
            decoded[1]
                == ArgPartView::Macro {
                    write_to_file: false,
                    mac: ArgMacroView::Location {
                        label: &label("foo", "gen"),
                        dep_kind: MacroDepKind::Toolchain,
                    },
                }
        );
        assert!(
            decoded[2]
                == ArgPartView::Macro {
                    write_to_file: true,
                    mac: ArgMacroView::Exe {
                        label: &label("tools", "compiler"),
                        exec_dep: true,
                    },
                }
        );
        assert!(
            decoded[5]
                == ArgPartView::Macro {
                    write_to_file: false,
                    mac: ArgMacroView::UserKeyedPlaceholder {
                        var: "cxxflags",
                        label: &label("foo", "deps"),
                        arg: Some("--x"),
                    },
                }
        );
    }

    #[test]
    fn test_arg_query_macro_native_roundtrip_and_shared_record() {
        let mk_query = |expansion: QueryExpansion| {
            let mut lits = BTreeMap::new();
            lits.insert((5usize, 10usize), label("foo", "dep1"));
            lits.insert((23usize, 10usize), label("bar", "dep2"));
            UnconfiguredMacro::Query(Box::new(QueryMacroBase {
                expansion_type: expansion,
                query: QueryAttrBase {
                    query: "deps(//foo:dep1) + set(//bar:dep2)".to_owned(),
                    resolved_literals: ResolvedQueryLiterals(lits),
                },
            }))
        };
        let query_arg = |prefix: &str, expansion| {
            arg(vec![
                StringWithMacrosPart::String(ArcStr::from(prefix)),
                StringWithMacrosPart::Macro(false, mk_query(expansion)),
            ])
        };
        let no_lits = arg(vec![StringWithMacrosPart::Macro(
            true,
            UnconfiguredMacro::Query(Box::new(QueryMacroBase {
                expansion_type: QueryExpansion::Output,
                query: QueryAttrBase {
                    query: "deps(:x)".to_owned(),
                    resolved_literals: ResolvedQueryLiterals(BTreeMap::new()),
                },
            })),
        )]);
        roundtrip_canonical(&[
            query_arg("--deps=", QueryExpansion::Target),
            query_arg(
                "--deps=",
                QueryExpansion::TargetAndOutput(Some("=".to_owned())),
            ),
            query_arg("--deps=", QueryExpansion::TargetAndOutput(None)),
            no_lits,
        ]);
        let (arena, r) = encode_one(&query_arg("--deps=", QueryExpansion::Target));
        let (reprint, lits) = queries_of(arena.view(r)).remove(0);
        assert_eq!(reprint, "( deps(root//foo:dep1) + set(root//bar:dep2))");
        assert_eq!(lits.0.len(), 2);

        // Same macro in a different arg: only the new arg record and the
        // new text record allocate; the query macro record is shared.
        let mut b = CoercedAttrArena::builder();
        query_arg("--deps=", QueryExpansion::Target)(&mut b).unwrap();
        let bytes_after = b.arena.bytes.len();
        let AttrToken(r2, _) = query_arg("--other=", QueryExpansion::Target)(&mut b).unwrap();
        let (_, width) = b.arena.tag(r2);
        assert_eq!(
            b.arena.bytes.len(),
            bytes_after + (1 + 1 + 8) + (1 + 1 + 2 * width as usize),
            "second arg should add one 8-byte text record and its own [tag, count, 2 refs] record"
        );
    }

    /// Coercion records a repeated literal once (first occurrence's span);
    /// every occurrence must still encode as that label, or the reprint
    /// would carry the raw spelling that analysis cannot resolve.
    #[test]
    fn test_arg_query_macro_repeated_literal_resolves_every_occurrence() {
        let query = "deps(set(//foo:dep1 //foo:dep1 //bar:dep2 //foo:dep1))".to_owned();
        let mut lits = BTreeMap::new();
        lits.insert((9usize, 10usize), label("foo", "dep1"));
        lits.insert((31usize, 10usize), label("bar", "dep2"));
        let value = arg(vec![StringWithMacrosPart::Macro(
            false,
            UnconfiguredMacro::Query(Box::new(QueryMacroBase {
                expansion_type: QueryExpansion::Target,
                query: QueryAttrBase {
                    query,
                    resolved_literals: ResolvedQueryLiterals(lits),
                },
            })),
        )]);
        let mut b = CoercedAttrArena::builder();
        let r = value(&mut b).unwrap().0;
        let arena = b.finish();
        let (reprint, lits) = queries_of(arena.view(r)).remove(0);
        assert_eq!(
            reprint, "deps(set(root//foo:dep1 root//foo:dep1 root//bar:dep2 root//foo:dep1))",
            "every occurrence should reprint as the resolved label"
        );
        assert_eq!(
            lits.0.len(),
            4,
            "each occurrence should carry its own literal span"
        );
    }

    #[test]
    fn test_source_file_pointer_dedup() {
        use buck2_core::package::package_relative_path::PackageRelativePath;
        let shared = PackageRelativePath::new("hdrs/common.h").unwrap().to_arc();
        let a = source_file(CoercedPath::File(shared.dupe()));
        let b = source_file(CoercedPath::File(shared));
        let other = source_file(CoercedPath::File(
            PackageRelativePath::new("other.h").unwrap().to_arc(),
        ));
        let mut bld = CoercedAttrArena::builder();
        let AttrToken(r1, h1) = a(&mut bld).unwrap();
        let AttrToken(r2, h2) = b(&mut bld).unwrap();
        let AttrToken(r3, _) = other(&mut bld).unwrap();
        assert_eq!(r1, r2, "same listing allocation should dedup by pointer");
        assert_eq!(h1, h2);
        assert_ne!(r1, r3);
        assert_eq!(bld.arena.paths.len(), 2);
    }

    #[test]
    fn test_arg_parts_dedup_across_args() {
        // Same template, different exe label: the literal chunk is shared;
        // the second arg adds only its parent node and the new macro record.
        let exe_arg = |name: &str| {
            arg(vec![
                StringWithMacrosPart::String(ArcStr::from("-o $OUT --long-shared-literal-chunk")),
                StringWithMacrosPart::Macro(
                    false,
                    UnconfiguredMacro::Exe {
                        label: label("tools", name),
                        exec_dep: true,
                    },
                ),
            ])
        };
        let mut b = CoercedAttrArena::builder();
        let AttrToken(r1, h1) = exe_arg("gen1")(&mut b).unwrap();
        let bytes_after_first = b.arena.bytes.len();
        let AttrToken(r2, h2) = exe_arg("gen2")(&mut b).unwrap();
        assert_ne!(r1, r2);
        assert_ne!(h1, h2);
        let (_, width) = b.arena.tag(r2);
        // The macro record is [tag, header, label index, 0 refs] (4 bytes);
        // the arg record is [tag, count, 2 refs]; the text record is shared.
        assert_eq!(
            b.arena.bytes.len(),
            bytes_after_first + 4 + (1 + 1 + 2 * width as usize),
            "second arg should add its own record and one macro record; the text record is shared"
        );
        let AttrToken(r3, _) = exe_arg("gen1")(&mut b).unwrap();
        assert_eq!(r1, r3, "identical arg should dedup to one node");
    }

    #[test]
    fn test_dedup_shares_nodes_and_labels() {
        let shared = list(vec![s("shared"), dep(label("foo", "bar"))]);
        let mut b = CoercedAttrArena::builder();
        let r1 = shared(&mut b).unwrap().0;
        let bytes_after = b.arena.bytes.len();
        let r2 = shared(&mut b).unwrap().0;
        assert_eq!(r1, r2, "structurally equal values should dedup to one ref");
        assert_eq!(
            bytes_after,
            b.arena.bytes.len(),
            "dedup hit should allocate nothing"
        );
        let r3 = b.encode_label(&label("foo", "bar")).unwrap().0;
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
