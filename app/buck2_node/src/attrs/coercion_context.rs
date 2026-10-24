/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::provider::label::NonDefaultProvidersName;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::label::label::TargetLabel;
use buck2_query::query::syntax::simple::functions::QueryLiteralVisitor;
use buck2_query_parser::Expr;
use buck2_query_parser::spanned::Spanned;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use buck2_util::strong_hasher::StrongHash128;
use starlark::values::Value;
use starlark::values::ValueIdentity;

use super::coerced_attr::CoercedAttr;
use super::coerced_attr::CoercedSelector;
use crate::attrs::attr_type::AttrType;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::coerced_path::CoercedPath;
use crate::attrs::configurable::AttrIsConfigurable;
use crate::configuration::resolved::ConfigurationSettingKey;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum AttrCoercionContextError {
    #[error("Expected target label without name. Got `{0}`")]
    UnexpectedProvidersName(String),
}

/// Memo key identifying one coercion of a frozen starlark value.
///
/// The coercion result is not a function of the input value alone: the same
/// value can coerce to different results — or fail for one attribute and
/// succeed for another — so the key carries every input the result depends
/// on. Besides the value itself (identified by `ValueIdentity<'v>`, whose
/// soundness starlark owns), every component is an identity-grade content
/// hash, so keys stay valid regardless of what outlives what.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Allocative)]
pub struct CoerceMemoKey<'v> {
    /// The input, by identity. Only frozen values are keyed: they are
    /// immutable and pointer-stable for the lifetime of the evaluation.
    value: ValueIdentity<'v>,
    /// The coercer, by content fingerprint: `attr.string()` and `attr.int()`
    /// coerce the same input to different results (or reject it), while
    /// structurally equal types coerce identically and may share entries.
    coercer: StrongHash128,
    /// The attribute default, by identity-grade content hash (`None` when
    /// the attribute has no default): selector coercion resolves entries
    /// against it, so the same input under a different default can produce
    /// a different result.
    default: Option<StrongHash128>,
    /// Whether `select()` is permitted: affects both the result shape and
    /// whether coercion succeeds at all.
    configurable: bool,
}

impl<'v> CoerceMemoKey<'v> {
    /// `None` for non-frozen values (mutable, so not memoizable) and for
    /// defaults with no cheaply available content hash.
    pub fn new(
        value: Value<'v>,
        coercer: &AttrType,
        default_attr: Option<&CoercedAttr>,
        configurable: AttrIsConfigurable,
    ) -> Option<Self> {
        if !value.is_frozen() {
            return None;
        }
        let default = match default_attr {
            None => None,
            // Heap defaults carry no stored hash; hashing one per coercion
            // would cost more than the memo saves.
            Some(_) => return None,
        };
        Some(CoerceMemoKey {
            value: value.identity(),
            coercer: coercer.fingerprint(),
            default,
            configurable: configurable == AttrIsConfigurable::Yes,
        })
    }
}

/// The context for attribute coercion. Mostly just contains information about
/// the current package (to support things like parsing targets from strings).
pub trait AttrCoercionContext<'v> {
    fn coerce_target_label(&self, value: &str) -> buck2_error::Result<TargetLabel> {
        let label = self.coerce_providers_label(value)?;

        if let ProvidersName::NonDefault(flavor) = label.name()
            && matches!(flavor.as_ref(), NonDefaultProvidersName::Named(_))
        {
            return Err(AttrCoercionContextError::UnexpectedProvidersName(value.to_owned()).into());
        }

        Ok(label.into_parts().0)
    }

    /// Attempt to convert a string into a label
    fn coerce_providers_label(&self, value: &str) -> buck2_error::Result<ProvidersLabel>;

    /// Memo hooks for the package-arena pipeline: cache coercion results for
    /// frozen starlark values (see `CoerceMemoKey`). Contexts without a memo
    /// return `None` / ignore inserts.
    fn coerce_memo_lookup(&self, _key: CoerceMemoKey<'v>) -> Option<CoercedAttr> {
        None
    }

    fn coerce_memo_insert(&self, _key: CoerceMemoKey<'v>, _value: &CoercedAttr) {}

    /// Attr constructors: callers hand over the parts of a value and the
    /// context owns the result representation. The defaults assemble plain
    /// enum values; an arena context encodes the parts directly instead, so
    /// the enum form (and its allocations) never materializes for values
    /// headed into a package arena.
    fn build_string_attr(&self, value: &str) -> buck2_error::Result<CoercedAttr> {
        Ok(CoercedAttr::String(StringLiteral(self.intern_str(value))))
    }

    fn build_list_attr(&self, items: Vec<CoercedAttr>) -> buck2_error::Result<CoercedAttr> {
        Ok(CoercedAttr::List(ListLiteral(self.intern_list(items))))
    }

    fn build_tuple_attr(&self, items: Vec<CoercedAttr>) -> buck2_error::Result<CoercedAttr> {
        Ok(CoercedAttr::Tuple(TupleLiteral(self.intern_list(items))))
    }

    fn build_dict_attr(
        &self,
        pairs: Vec<(CoercedAttr, CoercedAttr)>,
    ) -> buck2_error::Result<CoercedAttr> {
        Ok(CoercedAttr::Dict(DictLiteral(self.intern_dict(pairs))))
    }

    fn build_one_of_attr(&self, inner: CoercedAttr, idx: u32) -> buck2_error::Result<CoercedAttr> {
        Ok(CoercedAttr::OneOf(Box::new(inner), idx))
    }

    fn build_selector_attr(
        &self,
        entries: Vec<(ConfigurationSettingKey, CoercedAttr)>,
        default: Option<CoercedAttr>,
    ) -> buck2_error::Result<CoercedAttr> {
        Ok(CoercedAttr::Selector(self.intern_select(entries, default)?))
    }

    /// Reuse previously allocated string if possible.
    fn intern_str(&self, value: &str) -> ArcStr;

    // Reuse previously allocated slices if possible.
    fn intern_list(&self, value: Vec<CoercedAttr>) -> ArcSlice<CoercedAttr>;

    // Reuse previously allocated selects if possible.
    fn intern_select(
        &self,
        value: Vec<(ConfigurationSettingKey, CoercedAttr)>,
        default: Option<CoercedAttr>,
    ) -> buck2_error::Result<Arc<CoercedSelector>> {
        let hash = CoercedSelector::entries_hash_for_interning(&value);
        let entries = self.intern_select_hashed(value, hash);
        let selector = CoercedSelector::new_with_entries_hash(entries, default, hash)?;
        Ok(self.intern_selector(selector))
    }

    /// Returns an owner with the same ordered entries and default. Contexts may override this
    /// to reuse an equal selector; the default implementation allocates a fresh owner.
    #[doc(hidden)]
    fn intern_selector(&self, value: CoercedSelector) -> Arc<CoercedSelector> {
        Arc::new(value)
    }

    /// Returns canonical entries structurally equal to `value`, using `hash` only for lookup.
    #[doc(hidden)]
    fn intern_select_hashed(
        &self,
        value: Vec<(ConfigurationSettingKey, CoercedAttr)>,
        hash: u64,
    ) -> ArcSlice<(ConfigurationSettingKey, CoercedAttr)>;

    // Reuse previously allocated dicts if possible.
    fn intern_dict(
        &self,
        value: Vec<(CoercedAttr, CoercedAttr)>,
    ) -> ArcSlice<(CoercedAttr, CoercedAttr)>;

    /// Attempt to convert a string into a BuckPath
    fn coerce_path(&self, value: &str, allow_directory: bool) -> buck2_error::Result<CoercedPath>;

    fn coerce_target_pattern(
        &self,
        pattern: &str,
    ) -> buck2_error::Result<ParsedPattern<TargetPatternExtra>>;

    fn visit_query_function_literals<'q>(
        &self,
        visitor: &mut dyn QueryLiteralVisitor<'q>,
        expr: &Spanned<Expr<'q>>,
        query: &'q str,
    ) -> buck2_error::Result<()>;
}
