/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Contains the internal support within the attribute framework for `select()`.

use anyhow::Context;
use buck2_interpreter::selector::Selector;
use buck2_interpreter::selector::SelectorGen;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::collections::SmallMap;
use starlark::values::dict::Dict;
use starlark::values::Value;
use thiserror::Error;

use crate::attrs::coerce::attr_type::AttrTypeExt;

#[derive(Error, Debug)]
pub enum SelectError {
    #[error("select() condition was not a string, got `{0}`.")]
    KeyNotString(String),
    #[error("select() value was not a dict, got `{0}`.")]
    ValueNotDict(String),
    #[error("addition not supported for this attribute type `{0}`, got `{1}`.")]
    ConcatNotSupported(String, String),
    #[error("select() cannot be used in non-configuable attribute")]
    SelectCannotBeUsedForNonConfigurableAttr,
}

pub trait CoercedAttrExr: Sized {
    fn coerce(
        attr: &AttrType,
        configuable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
        default_attr: Option<&Self>,
    ) -> anyhow::Result<Self>;
}

impl CoercedAttrExr for CoercedAttr {
    fn coerce(
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
                        let mut items = SmallMap::with_capacity(dict.len());
                        let mut default = None;
                        for (k, v) in dict.iter() {
                            let k = k.unpack_str().ok_or_else(|| {
                                anyhow::anyhow!(SelectError::KeyNotString(k.to_repr()))
                            })?;
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
                        Err(anyhow::anyhow!(SelectError::ValueNotDict(v.to_repr())))
                    }
                }
                SelectorGen::Added(l, r) => {
                    if !attr.supports_concat() {
                        return Err(anyhow::anyhow!(SelectError::ConcatNotSupported(
                            attr.to_string(),
                            format!("{} + {}", l, r)
                        )));
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
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::sync::Arc;

    use buck2_common::ordered_map::OrderedMap;
    use buck2_core::configuration::constraints::ConstraintKey;
    use buck2_core::configuration::constraints::ConstraintValue;
    use buck2_core::configuration::transition::applied::TransitionApplied;
    use buck2_core::configuration::transition::id::TransitionId;
    use buck2_core::configuration::Configuration;
    use buck2_core::configuration::ConfigurationData;
    use buck2_core::target::testing::TargetLabelExt;
    use buck2_core::target::TargetLabel;
    use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
    use buck2_node::attrs::coerced_attr::CoercedAttr;
    use buck2_node::attrs::configuration_context::AttrConfigurationContext;
    use gazebo::prelude::Dupe;
    use starlark::collections::SmallMap;
    use starlark_map::smallmap;

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

            fn resolved_transitions(
                &self,
            ) -> &OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>> {
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
        let select_entries = SmallMap::from_iter([
            (linux.dupe(), literal_true()),
            (linux_x86_64.dupe(), literal_str()),
        ]);
        assert_eq!(
            Some(&literal_str()),
            CoercedAttr::select_the_most_specific(&ctx, &select_entries).unwrap()
        );

        // Test more specific is selected even if it is first.
        let select_entries = SmallMap::from_iter([
            (linux_x86_64.dupe(), literal_str()),
            (linux.dupe(), literal_true()),
        ]);
        assert_eq!(
            Some(&literal_str()),
            CoercedAttr::select_the_most_specific(&ctx, &select_entries).unwrap()
        );

        // Conflicting keys.
        let select_entries = SmallMap::from_iter([
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
                SmallMap::from_iter([
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
