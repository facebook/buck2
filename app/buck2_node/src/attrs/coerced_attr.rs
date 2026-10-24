/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use core::fmt;
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_hash::BuckHasherBuilder;
use buck2_hash::BuckMutSet;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use buck2_util::size_assert;
use pagable::Pagable;
use smallvec::SmallVec;
use strong_hash::StrongHash;

use crate::attrs::attr_type::arg::StringWithMacros;
use crate::attrs::attr_type::bool::BoolLiteral;
use crate::attrs::attr_type::configured_dep::UnconfiguredExplicitConfiguredDep;
use crate::attrs::attr_type::dict::DictLiteral;
use crate::attrs::attr_type::list::ListLiteral;
use crate::attrs::attr_type::query::QueryAttr;
use crate::attrs::attr_type::string::StringLiteral;
use crate::attrs::attr_type::transition_dep::CoercedTransitionDep;
use crate::attrs::attr_type::tuple::TupleLiteral;
use crate::attrs::coerced_path::CoercedPath;
use crate::configuration::resolved::ConfigurationSettingKey;
use crate::metadata::map::MetadataMap;
use crate::modifiers::CfgModifiers;
use crate::visibility::VisibilitySpecification;
use crate::visibility::WithinViewSpecification;

#[derive(Debug, Eq, PartialEq)]
pub enum CoercedSelectorKeyRef<'a> {
    Target(&'a ConfigurationSettingKey),
    Default,
}

impl CoercedSelectorKeyRef<'_> {
    pub const DEFAULT_KEY_STR: &'static str = "DEFAULT";
}

impl fmt::Display for CoercedSelectorKeyRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoercedSelectorKeyRef::Target(target) => write!(f, "{}", target.0),
            CoercedSelectorKeyRef::Default => write!(f, "{}", Self::DEFAULT_KEY_STR),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StrongHash, Allocative, Pagable)]
pub struct CoercedSelector {
    pub(crate) entries: ArcSlice<(ConfigurationSettingKey, CoercedAttr)>,
    pub(crate) default: Option<CoercedAttr>,
}

size_assert::words_of_type!(CoercedSelector, 5);

impl CoercedSelector {
    pub fn new(
        entries: ArcSlice<(ConfigurationSettingKey, CoercedAttr)>,
        default: Option<CoercedAttr>,
    ) -> buck2_error::Result<CoercedSelector> {
        Self::check_all_keys_unique(&entries)?;
        Ok(CoercedSelector { entries, default })
    }

    pub(crate) fn check_all_keys_unique(
        entries: &[(ConfigurationSettingKey, CoercedAttr)],
    ) -> buck2_error::Result<()> {
        fn duplicate_key(key: &ConfigurationSettingKey) -> buck2_error::Error {
            buck2_error!(
                buck2_error::ErrorTag::Input,
                "duplicate key `{key}` in `select()`"
            )
        }

        // This is possible when select keys are specified like:
        // ```
        // select({
        //   "cell//foo:bar": 2,
        //   "//foo:bar": 1,
        //   ":bar": 3,
        // })
        // ```
        // Keys are unique strings, but resolved to the same target.

        // Quadratic is cheaper than hashing for small `N`.
        // For 32 entries we do 496 comparisons, which is cheaper than 32 hashing operations.
        if entries.len() <= 32 {
            for i in 0..entries.len() {
                for j in i + 1..entries.len() {
                    if entries[i].0 == entries[j].0 {
                        return Err(duplicate_key(&entries[i].0));
                    }
                }
            }
        } else {
            let mut visited_keys: BuckMutSet<&ConfigurationSettingKey> =
                BuckMutSet::with_capacity_and_hasher(entries.len(), BuckHasherBuilder);
            for (k, _) in entries {
                if !visited_keys.insert(k) {
                    return Err(duplicate_key(k));
                }
            }
        }

        Ok(())
    }

    pub fn all_entries(&self) -> impl Iterator<Item = (CoercedSelectorKeyRef<'_>, &CoercedAttr)> {
        self.entries
            .iter()
            .map(|(k, v)| (CoercedSelectorKeyRef::Target(k), v))
            .chain(
                self.default
                    .iter()
                    .map(|default| (CoercedSelectorKeyRef::Default, default)),
            )
    }

    pub(crate) fn fail_to_json(message: &str) -> Result<serde_json::Value, buck2_error::Error> {
        Ok(serde_json::Value::Object(serde_json::Map::from_iter([
            (
                "__type".to_owned(),
                serde_json::Value::String("select_fail".to_owned()),
            ),
            (
                "message".to_owned(),
                serde_json::Value::String(message.to_owned()),
            ),
        ])))
    }

    pub(crate) fn incompatible_to_json(
        message: &str,
    ) -> Result<serde_json::Value, buck2_error::Error> {
        Ok(serde_json::Value::Object(serde_json::Map::from_iter([
            (
                "__type".to_owned(),
                serde_json::Value::String("select_incompatible".to_owned()),
            ),
            (
                "message".to_owned(),
                serde_json::Value::String(message.to_owned()),
            ),
        ])))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StrongHash, Allocative, Pagable)]
pub struct CoercedConcat(pub ArcSlice<CoercedAttr>);

impl Deref for CoercedConcat {
    type Target = [CoercedAttr];

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

/// CoercedAttr is the "coerced" representation of an attribute. It has been type-checked and converted to
/// specific types (for example, where we expect target-like things, it has been converted to something like
/// a TargetLabel or ProvidersLabel).
///
/// CoercedAttr  provides support for the `select()` function. All coerced data is
/// potentially represented by a select that represents possibly different
/// configured representations.
///
/// CoercedData::Concat supports a representation for when a selectable is added
/// to something. Not all types support this case and those will return an error
/// during coercion and not ever use the ::Concat case.
#[derive(Debug, Clone, PartialEq, Eq, Hash, StrongHash, Allocative, Pagable)]
pub enum CoercedAttr {
    Selector(Arc<CoercedSelector>),
    SelectFail(ArcStr),
    SelectIncompatible(ArcStr),
    Concat(CoercedConcat),

    Bool(BoolLiteral),
    Int(i64),
    String(StringLiteral),
    // Like String, but drawn from a set of variants, so doesn't support concat
    EnumVariant(StringLiteral),
    List(ListLiteral<CoercedAttr>),
    Tuple(TupleLiteral<CoercedAttr>),
    Dict(DictLiteral<CoercedAttr>),
    None,
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    OneOf(
        Box<Self>,
        // Index of matched oneof attr type variant.
        u32,
    ),
    Visibility(VisibilitySpecification),
    WithinView(WithinViewSpecification),
    ExplicitConfiguredDep(Box<UnconfiguredExplicitConfiguredDep>),
    TransitionDep(Box<CoercedTransitionDep>),
    SplitTransitionDep(ProvidersLabel),
    ConfigurationDep(ProvidersLabel),
    PluginDep(TargetLabel),
    Dep(ProvidersLabel),
    SourceLabel(ProvidersLabel),
    // NOTE: unlike deps, labels are not traversed, as they are typically used in lieu of deps in
    // cases that would cause cycles.
    Label(ProvidersLabel),
    Arg(StringWithMacros<ProvidersLabel>),
    Query(Box<QueryAttr<ProvidersLabel>>),
    SourceFile(CoercedPath),
    Metadata(MetadataMap),
    TargetModifiers(CfgModifiers),
    /// A value already encoded into the package arena during coercion.
    /// Never escapes coercion: `AttrValues::from_pairs` consumes it; every
    /// other consumer errors.
    Encoded(
        crate::attrs::arena::AttrWord,
        crate::attrs::arena::CoercedAttrHash,
    ),
}

// This is just to help understand any impact that changes have to the size of this.
// We store a lot of these, so we try to keep it to a reasonable size.
size_assert::words_of_type!(CoercedAttr, 3);

impl CoercedAttr {
    /// If more than one select key matches, select the most specific.
    pub fn select_the_most_specific<'a, 'x>(
        select_entries: impl IntoIterator<
            Item = (
                &'x ConfigurationSettingKey,
                &'x ConfigSettingData,
                &'a CoercedAttr,
            ),
        >,
    ) -> buck2_error::Result<Option<&'a CoercedAttr>> {
        let select_entries_vec = SmallVec::<[_; 17]>::from_iter(select_entries);

        let mut select_entries = select_entries_vec.iter().copied();
        let Some(mut matching): Option<(
            &ConfigurationSettingKey,
            &ConfigSettingData,
            &CoercedAttr,
        )> = select_entries.next() else {
            return Ok(None);
        };

        for (k, conf, v) in select_entries {
            let (prev_k, prev_conf, prev_v) = matching;
            matching = {
                {
                    if conf.refines(prev_conf) {
                        (k, conf, v)
                    } else if prev_conf.refines(conf) {
                        (prev_k, prev_conf, prev_v)
                    } else {
                        return Self::select_the_most_specific_slow(select_entries_vec);
                    }
                }
            }
        }
        Ok(Some(matching.2))
    }

    /// Select the first matching entry without considering specificity.
    pub fn select_the_first_match<'a, 'x>(
        select_entries: impl IntoIterator<
            Item = (
                &'x ConfigurationSettingKey,
                &'x ConfigSettingData,
                &'a CoercedAttr,
            ),
        >,
    ) -> Option<&'a CoercedAttr> {
        select_entries.into_iter().next().map(|(_, _, v)| v)
    }

    fn select_the_most_specific_slow<'a>(
        select_entries: SmallVec<
            [(
                &ConfigurationSettingKey,
                &ConfigSettingData,
                &'a CoercedAttr,
            ); 17],
        >,
    ) -> buck2_error::Result<Option<&'a CoercedAttr>> {
        let mut entries =
            SmallVec::<[(&ConfigurationSettingKey, &ConfigSettingData, &CoercedAttr); 17]>::new();

        for (k, d, v) in select_entries {
            // If there's entry for `linux-arm32` and current is `arm32`, skip current.
            if entries.iter().any(|(_, prev_d, _)| prev_d.refines(d)) {
                continue;
            }
            // If current is `linux-arm32`, remove `arm32` from `entries`.
            entries.retain(|(_, prev_d, _)| !d.refines(prev_d));
            entries.push((k, d, v));
        }
        match entries.as_slice() {
            [] => Err(internal_error!(
                "no entries after slow select the most specific"
            )),
            [(.., x)] => Ok(Some(x)),
            multiple_entries => {
                // Check if all entries have the same value
                let (first_key, _, first_value) = &multiple_entries[0];
                // Find the first entry with a different value, if any
                let different_value_entry = multiple_entries
                    .iter()
                    .skip(1)
                    .find(|(_, _, v)| v != first_value);
                if let Some((different_key, _, _)) = different_value_entry {
                    // Report the ambiguity error with the specific keys that have different values
                    Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Both select keys `{first_key}` and `{different_key}` match the configuration, but neither is more specific and they have different values"
                    ))
                } else {
                    // If all values are the same, return that value
                    Ok(Some(first_value))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_util::arc_str::ArcSlice;
    use dupe::Dupe;
    use pagable::PagableDeserialize;
    use pagable::PagableSerialize;
    use pagable::testing::TestingDeserializer;
    use pagable::testing::TestingSerializer;

    use crate::attrs::coerced_attr::CoercedAttr;
    use crate::attrs::coerced_attr::CoercedConcat;
    use crate::attrs::coerced_attr::CoercedSelector;
    use crate::configuration::resolved::ConfigurationSettingKey;

    #[test]
    fn test_check_all_keys_unique_small() {
        let a = ConfigurationSettingKey::testing_parse("foo//:a");
        let b = ConfigurationSettingKey::testing_parse("foo//:b");
        let c = ConfigurationSettingKey::testing_parse("foo//:c");
        let attr = CoercedAttr::None;
        let a = (a.dupe(), attr.clone());
        let b = (b.dupe(), attr.clone());
        let c = (c.dupe(), attr);
        assert!(
            CoercedSelector::check_all_keys_unique(&[a.clone(), b.clone(), a.clone()]).is_err()
        );
        assert!(
            CoercedSelector::check_all_keys_unique(&[a.clone(), b.clone(), b.clone()]).is_err()
        );
        assert!(
            CoercedSelector::check_all_keys_unique(&[a.clone(), a.clone(), b.clone()]).is_err()
        );
        assert!(CoercedSelector::check_all_keys_unique(&[a, b, c]).is_ok());
    }

    #[test]
    fn test_check_all_keys_unique_large() {
        let attr = CoercedAttr::None;
        let mut long = (0..100)
            .map(|i| {
                (
                    ConfigurationSettingKey::testing_parse(&format!("foo//:{i}")),
                    attr.clone(),
                )
            })
            .collect::<Vec<_>>();
        assert!(CoercedSelector::check_all_keys_unique(&long).is_ok());
        long[10].0 = long[0].0.dupe();
        assert!(CoercedSelector::check_all_keys_unique(&long).is_err());
    }

    #[test]
    fn test_selector_concat_sharing_survives_paging() -> buck2_error::Result<()> {
        let selector = Arc::new(CoercedSelector::new(
            ArcSlice::default(),
            Some(CoercedAttr::None),
        )?);
        let concat = CoercedConcat(ArcSlice::new([
            CoercedAttr::Selector(selector.dupe()),
            CoercedAttr::Selector(selector),
        ]));
        let value = vec![
            CoercedAttr::Concat(concat.clone()),
            CoercedAttr::Concat(concat),
        ];
        let mut serializer = TestingSerializer::new();
        value
            .pagable_serialize(&mut serializer)
            .expect("selector and concat serialization should succeed");
        let bytes = serializer.finish();
        let mut deserializer = TestingDeserializer::new(&bytes);
        let restored = Vec::<CoercedAttr>::pagable_deserialize(&mut deserializer)
            .expect("selector and concat deserialization should succeed");
        assert_eq!(value, restored);
        for values in [&value, &restored] {
            let [CoercedAttr::Concat(first), CoercedAttr::Concat(second)] = values.as_slice()
            else {
                panic!("concat values must survive serialization");
            };
            assert_eq!(first.as_ptr(), second.as_ptr());
            let [CoercedAttr::Selector(first), CoercedAttr::Selector(second)] = &**first else {
                panic!("nested selectors must survive serialization");
            };
            assert!(Arc::ptr_eq(first, second));
        }
        Ok(())
    }
}
