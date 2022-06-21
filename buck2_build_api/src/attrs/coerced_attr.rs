/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Contains the internal support within the attribute framework for `select()`.

use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use anyhow::anyhow;
use anyhow::Context;
use buck2_core::configuration::Configuration;
use buck2_core::configuration::ConfigurationData;
use buck2_core::target::TargetLabel;
use buck2_interpreter::selector::Selector;
use buck2_interpreter::selector::SelectorGen;
use gazebo::prelude::*;
use itertools::Itertools;
use serde::Serialize;
use serde::Serializer;
use starlark::collections::SmallMap;
use starlark::values::dict::Dict;
use starlark::values::Heap;
use starlark::values::Value;
use thiserror::Error;

use crate::attrs::attr_type::attr_literal::CoercionError;
use crate::attrs::attr_type::AttrType;
use crate::attrs::AttrCoercionContext;
use crate::attrs::AttrConfigurationContext;
use crate::attrs::AttrLiteral;
use crate::attrs::CoercedAttrTraversal;
use crate::attrs::ConfiguredAttr;
use crate::attrs::OrderedMap;
use crate::interpreter::rule_defs::attr::AttrIsConfigurable;

#[derive(Error, Debug)]
pub(crate) enum SelectError {
    #[error("None of {} conditions matched configuration `{}` and no default was set:\n{}",
        .1.len(),
        .0,
        .1.iter().map(|s| format!("  {}", s)).join("\n"),
    )]
    MissingDefault(Configuration, Vec<TargetLabel>),
    #[error("select() condition was not a string, got `{}`.", {0})]
    KeyNotString(String),
    #[error("select() value was not a dict, got `{}`.", {0})]
    ValueNotDict(String),
    #[error("addition not supported for this attribute type `{0}`.")]
    ConcatNotSupported(String),
    #[error("addition not supported for these attribute type `{0}` and value `{1}`.")]
    ConcatNotSupportedValues(&'static str, String),
    #[error("addition not supported for lists of different types, got `{0}` and `{1}`.")]
    ConcatListDifferentTypes(String, String),
    #[error("got same key in both sides of dictionary concat (key `{0}`).")]
    DictConcatDuplicateKeys(String),
    #[error(
        "Both select keys `{0}` and `{1}` match the configuration, but neither is more specific"
    )]
    TwoKeysDoNotRefineEachOther(String, String),
    #[error("select() cannot be used in non-configuable attribute")]
    SelectCannotBeUsedForNonConfigurableAttr,
    #[error("concat with no items (internal error)")]
    ConcatEmpty,
}

/// CoercedAttr is the "coerced" representation of an attribute. It has been type-checked and converted to
/// specific types (for example, where we expect target-like things, it has been converted to something like
/// a TargetLable or ProvidersLabel).
///
/// CoercedAttr  provides support for the `select()` function. All coerced data is
/// potentially represented by a select that represents possibly different
/// configured representations.
///
/// CoercedData::Concat supports a representation for when a selectable is added
/// to something. Not all types support this case and those will return an error
/// during coercion and not ever use the ::Concat case.
#[derive(Debug, Clone)]
pub enum CoercedAttr {
    Literal(AttrLiteral<Self>),
    Selector(Box<(OrderedMap<TargetLabel, Self>, Option<Self>)>),
    Concat(Vec<Self>),
}

// This is just to help understand any impact that changes have to the size of this.
// We store a lot of these, so we try to keep it to a reasonable size.
static_assertions::assert_eq_size!(CoercedAttr, [usize; 5]);

/// Provides roughly the stringified version of the starlark code that would produce this attr. For example, a dictionary
/// of string keys and values may result in `{"key1":"value1","key2":"value2"}` (note that strings will explicitly include
/// the wrapping `"`).
impl Display for CoercedAttr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoercedAttr::Literal(v) => Display::fmt(v, f),
            CoercedAttr::Selector(box (items, default)) => {
                write!(f, "select(")?;
                for (i, (condition, value)) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "\"{}\"={}", condition, value)?;
                }
                if let Some(default) = default {
                    if !items.is_empty() {
                        write!(f, ",")?;
                    }
                    write!(f, "\"DEFAULT\"={}", default)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            CoercedAttr::Concat(items) => write!(f, "{}", items.iter().format("+")),
        }
    }
}

impl PartialEq for CoercedAttr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (CoercedAttr::Concat(this), CoercedAttr::Concat(other)) => this == other,
            (CoercedAttr::Literal(this), CoercedAttr::Literal(other)) => this == other,
            (
                CoercedAttr::Selector(box (this_values, this_default)),
                CoercedAttr::Selector(box (other_values, other_default)),
            ) => {
                // ordering of selects matter, so we compare the iters
                this_default == other_default && this_values.iter().eq(other_values.iter())
            }
            _ => false,
        }
    }
}

impl Eq for CoercedAttr {}

impl Hash for CoercedAttr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            CoercedAttr::Concat(attr) => {
                state.write_u8(0);
                attr.hash(state);
            }
            CoercedAttr::Literal(lit) => {
                state.write_u8(1);
                lit.hash(state);
            }
            CoercedAttr::Selector(box (values, defaults)) => {
                state.write_u8(2);
                defaults.hash(state);
                values.iter().for_each(|v| v.hash(state))
            }
        }
    }
}

impl Serialize for CoercedAttr {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // TODO this is inefficient. We should impl Serialize and derive value from this instead.
        self.to_json()
            .map_err(|e| serde::ser::Error::custom(format!("{}", e)))?
            .serialize(s)
    }
}

impl CoercedAttr {
    pub(crate) fn new_literal(value: AttrLiteral<CoercedAttr>) -> Self {
        Self::Literal(value)
    }

    pub(crate) fn coerce(
        attr: &AttrType,
        configuable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
        default_attr: Option<&Self>,
    ) -> anyhow::Result<Self> {
        // A Selector in starlark is currently implemented as simply a Value (holding a
        // dict if valid).
        //
        // TODO(cjhopman): the select() function itself should
        // perform the conversion of its case arguments to configuration labels.
        //
        // TODO(cjhopman): Selectable addition (__ladd__ and __radd__) should perform
        // verification that the two sides of the addition have the same type.
        // Even if it did, we still need to verify that the two sides
        // are actually compatible (i.e. selectable can ensure that both sides are
        // lists, we can ensure that  both sides are List<T>)
        if let Some(selector) = Selector::from_value(value) {
            if let AttrIsConfigurable::No = configuable {
                return Err(SelectError::SelectCannotBeUsedForNonConfigurableAttr.into());
            }

            match *selector {
                SelectorGen::Inner(v) => {
                    if let Some(dict) = Dict::from_value(v) {
                        let mut items = SmallMap::new();
                        let mut default = None;
                        for (k, v) in dict.iter() {
                            let k = k
                                .unpack_str()
                                .ok_or_else(|| anyhow!(SelectError::KeyNotString(k.to_repr())))?;
                            let v = match default_attr {
                                Some(default_attr) if v.is_none() => default_attr.clone(),
                                _ => CoercedAttr::coerce(attr, configuable, ctx, v, None)?,
                            };
                            if k == "DEFAULT" {
                                assert!(
                                    default.is_none(),
                                    "Got repeated DEFAULT key, this should be impossible"
                                );
                                default = Some(v);
                            } else {
                                let target = ctx.coerce_target(k)?;
                                items.insert(target, v);
                            }
                        }
                        Ok(CoercedAttr::Selector(box (items, default)))
                    } else {
                        Err(anyhow!(SelectError::ValueNotDict(v.to_repr())))
                    }
                }
                SelectorGen::Added(l, r) => {
                    if !attr.supports_concat() {
                        return Err(anyhow!(SelectError::ConcatNotSupported(attr.to_string())));
                    }
                    let l = CoercedAttr::coerce(attr, configuable, ctx, l, None)?;
                    let mut l = match l {
                        CoercedAttr::Concat(l) => l,
                        l => vec![l],
                    };
                    let r = CoercedAttr::coerce(attr, configuable, ctx, r, None)?;
                    let r = match r {
                        CoercedAttr::Concat(r) => r,
                        r => vec![r],
                    };

                    l.extend(r);
                    Ok(CoercedAttr::Concat(l))
                }
            }
        } else {
            Ok(CoercedAttr::Literal(
                attr.coerce_item(configuable, ctx, value)
                    .with_context(|| format!("when coercing {}", value))?,
            ))
        }
    }

    pub(crate) fn to_value<'v>(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self {
            CoercedAttr::Literal(v) => v.to_value(heap),
            x @ (CoercedAttr::Concat(..) | CoercedAttr::Selector(..)) => {
                // It is possible to convert selects back to Starlark objects,
                // but there's no need to do it for now (and probably never will).
                Err(CoercionError::AttrCannotBeConvertedToValue(x.to_string()).into())
            }
        }
    }

    /// Converts the coerced attr to a serde_json Value. This is generally just used for debugging or introspective
    /// things, a lot of the types will be dropped without special handling. For example, an artifact will just end
    /// up as the stringified version of its coerced value (i.e. while `//a:b` might represent some list of targets,
    /// in to_json it just appears as the string "//a:b").
    pub fn to_json(&self) -> anyhow::Result<serde_json::Value> {
        match self {
            CoercedAttr::Literal(v) => v.to_json(),
            CoercedAttr::Selector(box (selector, default)) => {
                let mut map = serde_json::Map::new();
                for (k, v) in selector.iter() {
                    map.insert(k.to_string(), v.to_json()?);
                }
                if let Some(default) = default {
                    map.insert("DEFAULT".to_owned(), default.to_json()?);
                }
                let select = serde_json::Value::Object(map);

                Ok(serde_json::Value::Object(serde_json::Map::from_iter([
                    (
                        "__type".to_owned(),
                        serde_json::Value::String("selector".to_owned()),
                    ),
                    ("entries".to_owned(), select),
                ])))
            }
            CoercedAttr::Concat(items) => {
                Ok(serde_json::Value::Object(serde_json::Map::from_iter([
                    (
                        "__type".to_owned(),
                        serde_json::Value::String("concat".to_owned()),
                    ),
                    (
                        "items".to_owned(),
                        serde_json::Value::Array(items.try_map(CoercedAttr::to_json)?),
                    ),
                ])))
            }
        }
    }

    /// Returns `true` if the result of evaluating this literal can ever be None.
    pub(crate) fn may_return_none(&self) -> bool {
        match self {
            Self::Literal(AttrLiteral::None) => true,
            Self::Literal(_) => false,
            Self::Selector(box (alts, def)) => {
                def.as_ref().map_or(true, |x| x.may_return_none())
                    || alts.values().any(|x| x.may_return_none())
            }
            Self::Concat(_) => false,
        }
    }

    /// Traverses the coerced attribute and provides the traverser callbacks for all deps (those in select conditions
    /// are passed as configuration deps).
    pub(crate) fn traverse<'a>(
        &'a self,
        traversal: &mut dyn CoercedAttrTraversal<'a>,
    ) -> anyhow::Result<()> {
        match self {
            CoercedAttr::Literal(v) => v.traverse(traversal),
            CoercedAttr::Selector(box (selector, default)) => {
                for (condition, value) in selector.iter() {
                    traversal.configuration_dep(condition)?;
                    value.traverse(traversal)?;
                }
                if let Some(v) = default {
                    v.traverse(traversal)?;
                }
                Ok(())
            }
            CoercedAttr::Concat(items) => {
                for item in items {
                    item.traverse(traversal)?;
                }
                Ok(())
            }
        }
    }

    /// If more than one select key matches, select the most specific.
    fn select_the_most_specific<'a>(
        ctx: &dyn AttrConfigurationContext,
        select_entries: &'a OrderedMap<TargetLabel, CoercedAttr>,
    ) -> anyhow::Result<Option<&'a CoercedAttr>> {
        let mut matching: Option<(&TargetLabel, &ConfigurationData, &CoercedAttr)> = None;
        for (k, v) in select_entries {
            matching = match (ctx.matches(k), matching) {
                (None, matching) => matching,
                (Some(conf), None) => Some((k, conf, v)),
                (Some(conf), Some((prev_k, prev_conf, prev_v))) => {
                    if conf.refines(prev_conf) {
                        Some((k, conf, v))
                    } else if prev_conf.refines(conf) {
                        Some((prev_k, prev_conf, prev_v))
                    } else {
                        return Err(anyhow!(SelectError::TwoKeysDoNotRefineEachOther(
                            prev_k.to_string(),
                            k.to_string()
                        )));
                    }
                }
            }
        }
        Ok(matching.map(|(_k, _conf, v)| v))
    }

    /// Returns the "configured" representation of the attribute in the provided context.
    /// This handles the resolution of the select() conditions and delegates to
    /// the actual attr type for handling any appropriate configuration-time
    /// processing.
    pub(crate) fn configure(
        &self,
        ctx: &dyn AttrConfigurationContext,
    ) -> anyhow::Result<ConfiguredAttr> {
        match self {
            CoercedAttr::Literal(v) => v.configure(ctx),
            CoercedAttr::Selector(box (selector, default)) => {
                if let Some(v) = Self::select_the_most_specific(ctx, selector)? {
                    return v.configure(ctx);
                }
                default
                    .as_ref()
                    .ok_or_else(|| {
                        anyhow!(SelectError::MissingDefault(
                            ctx.cfg().dupe(),
                            selector.keys().duped().collect()
                        ))
                    })?
                    .configure(ctx)
            }
            CoercedAttr::Concat(items) => {
                let singleton = items.len() == 1;
                let mut it = items.iter().map(|item| item.configure(ctx));
                let first = it.next().ok_or(SelectError::ConcatEmpty)??;
                if singleton {
                    Ok(first)
                } else {
                    first.concat(it)
                }
            }
        }
    }

    /// Checks if this attr matches the filter. For selectors and container-like things, will return true if any
    /// contained item matches the filter.
    pub fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> anyhow::Result<bool>,
    ) -> anyhow::Result<bool> {
        match self {
            CoercedAttr::Literal(v) => v.any_matches(filter),
            CoercedAttr::Selector(box (selector, default)) => {
                for value in selector.values() {
                    if value.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                if let Some(v) = default {
                    v.any_matches(filter)
                } else {
                    Ok(false)
                }
            }
            CoercedAttr::Concat(items) => {
                for item in items {
                    if item.any_matches(filter)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::iter::FromIterator;
    use std::sync::Arc;

    use buck2_core::configuration::constraints::ConstraintKey;
    use buck2_core::configuration::constraints::ConstraintValue;
    use buck2_core::configuration::Configuration;
    use buck2_core::configuration::ConfigurationData;
    use buck2_core::target::testing::TargetLabelExt;
    use buck2_core::target::TargetLabel;
    use gazebo::prelude::Dupe;
    use starlark::collections::SmallMap;

    use crate::attrs::attr_type::attr_literal::AttrLiteral;
    use crate::attrs::coerced_attr::CoercedAttr;
    use crate::attrs::AttrConfigurationContext;
    use crate::attrs::OrderedMap;
    use crate::interpreter::rule_defs::transition::applied::TransitionApplied;
    use crate::interpreter::rule_defs::transition::id::TransitionId;

    #[test]
    fn selector_equals_accounts_for_ordering() {
        let s1 = CoercedAttr::Selector(box (
            smallmap![
                TargetLabel::testing_parse("cell1//pkg1:target1") => CoercedAttr::Literal(AttrLiteral::Bool(true)),
                TargetLabel::testing_parse("cell2//pkg2:target2") => CoercedAttr::Literal(AttrLiteral::Bool(false))
            ],
            None,
        ));
        let s2 = CoercedAttr::Selector(box (
            smallmap![
                TargetLabel::testing_parse(
                    "cell1//pkg1:target1"
                ) => CoercedAttr::Literal(AttrLiteral::Bool(true)),
                TargetLabel::testing_parse(
                    "cell2//pkg2:target2"
                ) => CoercedAttr::Literal(AttrLiteral::Bool(false))
            ],
            None,
        ));

        assert_eq!(s1 == s2, true);

        let s2 = CoercedAttr::Selector(box (
            smallmap![
                TargetLabel::testing_parse("cell2//pkg2:target2") => CoercedAttr::Literal(AttrLiteral::Bool(false)),
                TargetLabel::testing_parse("cell1//pkg1:target1") => CoercedAttr::Literal(AttrLiteral::Bool(true))
            ],
            None,
        ));

        assert_eq!(s1 == s2, false);
    }

    #[test]
    fn select_the_most_specific() {
        struct SelectTestConfigurationContext {
            settings: BTreeMap<TargetLabel, ConfigurationData>,
        }

        impl AttrConfigurationContext for SelectTestConfigurationContext {
            fn matches<'a>(&'a self, label: &TargetLabel) -> Option<&'a ConfigurationData> {
                self.settings.get(label)
            }

            fn cfg(&self) -> &Configuration {
                panic!()
            }

            fn exec_cfg(&self) -> &Configuration {
                unimplemented!()
            }

            fn platform_cfg(&self, _label: &TargetLabel) -> anyhow::Result<&Configuration> {
                panic!("not used in test")
            }

            fn resolved_transitions(&self) -> &SmallMap<Arc<TransitionId>, Arc<TransitionApplied>> {
                panic!("not used in test")
            }
        }

        fn constraint_key(t: &str) -> ConstraintKey {
            ConstraintKey(TargetLabel::testing_parse(t))
        }

        fn constraint_value(t: &str) -> ConstraintValue {
            ConstraintValue(TargetLabel::testing_parse(t))
        }

        let c_os = constraint_key("//c:os");
        let c_linux = constraint_value("//c:linux");
        let c_cpu = constraint_key("//c:cpu");
        let c_arm64 = constraint_value("//c:arm64");
        let c_x86_64 = constraint_value("//c:x86_64");

        let linux = TargetLabel::testing_parse("//:linux");
        let linux_arm64 = TargetLabel::testing_parse("//:linux-arm64");
        let linux_x86_64 = TargetLabel::testing_parse("//:linux-x86_64");

        let ctx = SelectTestConfigurationContext {
            settings: BTreeMap::from_iter([
                (
                    linux.dupe(),
                    ConfigurationData {
                        constraints: BTreeMap::from_iter([(c_os.dupe(), c_linux.dupe())]),
                        buckconfigs: BTreeMap::new(),
                    },
                ),
                (
                    linux_arm64.dupe(),
                    ConfigurationData {
                        constraints: BTreeMap::from_iter([
                            (c_os.dupe(), c_linux.dupe()),
                            (c_cpu.dupe(), c_arm64.dupe()),
                        ]),
                        buckconfigs: BTreeMap::new(),
                    },
                ),
                (
                    linux_x86_64.dupe(),
                    ConfigurationData {
                        constraints: BTreeMap::from_iter([
                            (c_os.dupe(), c_linux.dupe()),
                            (c_cpu.dupe(), c_x86_64.dupe()),
                        ]),
                        buckconfigs: BTreeMap::new(),
                    },
                ),
            ]),
        };

        fn literal_true() -> CoercedAttr {
            CoercedAttr::Literal(AttrLiteral::Bool(true))
        }
        fn literal_str() -> CoercedAttr {
            CoercedAttr::Literal(AttrLiteral::String("linux".to_owned()))
        }

        // Test more specific is selected even if it is not first.
        let select_entries = OrderedMap::from_iter([
            (linux.dupe(), literal_true()),
            (linux_x86_64.dupe(), literal_str()),
        ]);
        assert_eq!(
            Some(&literal_str()),
            CoercedAttr::select_the_most_specific(&ctx, &select_entries).unwrap()
        );

        // Test more specific is selected even if it is first.
        let select_entries = OrderedMap::from_iter([
            (linux_x86_64.dupe(), literal_str()),
            (linux.dupe(), literal_true()),
        ]);
        assert_eq!(
            Some(&literal_str()),
            CoercedAttr::select_the_most_specific(&ctx, &select_entries).unwrap()
        );

        // Conflicting keys.
        let select_entries = OrderedMap::from_iter([
            (linux_arm64.dupe(), literal_true()),
            (linux_x86_64.dupe(), literal_str()),
        ]);
        assert_eq!(
            "Both select keys `//:linux-arm64` and `//:linux-x86_64` match the configuration, \
            but neither is more specific",
            CoercedAttr::select_the_most_specific(&ctx, &select_entries)
                .unwrap_err()
                .to_string()
        );
    }

    #[test]
    fn test_to_json_concat() {
        assert_eq!(
            r#"{"__type":"concat","items":["a","b","c","d"]}"#,
            CoercedAttr::Concat(vec![
                CoercedAttr::Literal(AttrLiteral::String("a".to_owned())),
                CoercedAttr::Literal(AttrLiteral::String("b".to_owned())),
                CoercedAttr::Literal(AttrLiteral::String("c".to_owned())),
                CoercedAttr::Literal(AttrLiteral::String("d".to_owned())),
            ])
            .to_json()
            .unwrap()
            .to_string()
        );
    }

    #[test]
    fn test_to_json_selector() {
        assert_eq!(
            r#"{"__type":"selector","entries":{"//:a":true,"//:b":10,"DEFAULT":"ddd"}}"#,
            CoercedAttr::Selector(box (
                OrderedMap::from_iter([
                    (
                        TargetLabel::testing_parse("//:a"),
                        CoercedAttr::Literal(AttrLiteral::Bool(true))
                    ),
                    (
                        TargetLabel::testing_parse("//:b"),
                        CoercedAttr::Literal(AttrLiteral::Int(10))
                    ),
                ]),
                Some(CoercedAttr::Literal(AttrLiteral::String("ddd".to_owned()))),
            ))
            .to_json()
            .unwrap()
            .to_string()
        );
    }
}
