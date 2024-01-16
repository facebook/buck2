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
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coerced_attr::CoercedSelector;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use starlark::values::dict::DictRef;
use starlark::values::Value;

use crate::attrs::coerce::attr_type::AttrTypeExt;
use crate::interpreter::selector::StarlarkSelector;
use crate::interpreter::selector::StarlarkSelectorGen;

#[derive(buck2_error::Error, Debug)]
enum SelectError {
    #[error("select() condition was not a string, got `{0}`.")]
    KeyNotString(String),
    #[error("select() value was not a dict, got `{0}`.")]
    ValueNotDict(String),
    #[error("addition not supported for this attribute type `{0}`, got `{1}`.")]
    ConcatNotSupported(String, String),
    #[error("select() cannot be used in non-configurable attribute")]
    SelectCannotBeUsedForNonConfigurableAttr,
    #[error("duplicate `\"DEFAULT\"` key in `select()` (internal error)")]
    DuplicateDefaultKey,
}

pub trait CoercedAttrExr: Sized {
    fn coerce(
        attr: &AttrType,
        configurable: AttrIsConfigurable,
        ctx: &dyn AttrCoercionContext,
        value: Value,
        default_attr: Option<&Self>,
    ) -> anyhow::Result<Self>;
}

impl CoercedAttrExr for CoercedAttr {
    fn coerce(
        attr: &AttrType,
        configurable: AttrIsConfigurable,
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
        if let Some(selector) = StarlarkSelector::from_value(value) {
            if let AttrIsConfigurable::No = configurable {
                return Err(SelectError::SelectCannotBeUsedForNonConfigurableAttr.into());
            }

            match *selector {
                StarlarkSelectorGen::Inner(v) => {
                    if let Some(dict) = DictRef::from_value(v) {
                        let has_default = dict.get_str("DEFAULT").is_some();
                        let mut entries =
                            Vec::with_capacity(dict.len().saturating_sub(has_default as usize));

                        let mut default = None;
                        for (k, v) in dict.iter() {
                            let k = k.unpack_str().ok_or_else(|| {
                                anyhow::anyhow!(SelectError::KeyNotString(k.to_repr()))
                            })?;
                            let v = match default_attr {
                                Some(default_attr) if v.is_none() => default_attr.clone(),
                                _ => CoercedAttr::coerce(attr, configurable, ctx, v, None)?,
                            };
                            if k == "DEFAULT" {
                                if default.is_some() {
                                    return Err(SelectError::DuplicateDefaultKey.into());
                                }
                                default = Some(v);
                            } else {
                                let target = ctx.coerce_target_label(k)?;
                                entries.push((target, v));
                            }
                        }

                        assert_eq!(entries.capacity(), entries.len());

                        Ok(CoercedAttr::Selector(Box::new(CoercedSelector::new(
                            ctx.intern_select(entries),
                            default,
                        )?)))
                    } else {
                        Err(anyhow::anyhow!(SelectError::ValueNotDict(v.to_repr())))
                    }
                }
                StarlarkSelectorGen::Added(l, r) => {
                    if !attr.supports_concat() {
                        return Err(anyhow::anyhow!(SelectError::ConcatNotSupported(
                            attr.to_string(),
                            format!("{} + {}", l, r)
                        )));
                    }
                    let l = CoercedAttr::coerce(attr, configurable, ctx, l, None)?;
                    let mut l = match l {
                        CoercedAttr::Concat(l) => l.into_vec(),
                        l => vec![l],
                    };
                    let r = CoercedAttr::coerce(attr, configurable, ctx, r, None)?;
                    let r = match r {
                        CoercedAttr::Concat(r) => r.into_vec(),
                        r => vec![r],
                    };

                    l.extend(r);
                    Ok(CoercedAttr::Concat(l.into_boxed_slice()))
                }
            }
        } else {
            Ok(attr
                .coerce_item(configurable, ctx, value)
                .with_context(|| format!("Error coercing {}", value))?)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::sync::Arc;

    use buck2_core::configuration::config_setting::ConfigSettingData;
    use buck2_core::configuration::constraints::ConstraintKey;
    use buck2_core::configuration::constraints::ConstraintValue;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::configuration::pair::ConfigurationNoExec;
    use buck2_core::configuration::pair::ConfigurationWithExec;
    use buck2_core::configuration::transition::applied::TransitionApplied;
    use buck2_core::configuration::transition::id::TransitionId;
    use buck2_core::target::label::TargetLabel;
    use buck2_node::attrs::attr_type::bool::BoolLiteral;
    use buck2_node::attrs::attr_type::string::StringLiteral;
    use buck2_node::attrs::coerced_attr::CoercedAttr;
    use buck2_node::attrs::coerced_attr::CoercedSelector;
    use buck2_node::attrs::configuration_context::AttrConfigurationContext;
    use buck2_node::attrs::fmt_context::AttrFmtContext;
    use buck2_util::arc_str::ArcSlice;
    use buck2_util::arc_str::ArcStr;
    use dupe::Dupe;
    use starlark_map::ordered_map::OrderedMap;

    #[test]
    fn selector_equals_accounts_for_ordering() {
        let s1 = CoercedAttr::Selector(Box::new(
            CoercedSelector::new(
                ArcSlice::new([
                    (
                        TargetLabel::testing_parse("cell1//pkg1:target1"),
                        CoercedAttr::Bool(BoolLiteral(true)),
                    ),
                    (
                        TargetLabel::testing_parse("cell2//pkg2:target2"),
                        CoercedAttr::Bool(BoolLiteral(false)),
                    ),
                ]),
                None,
            )
            .unwrap(),
        ));
        let s2 = CoercedAttr::Selector(Box::new(
            CoercedSelector::new(
                ArcSlice::new([
                    (
                        TargetLabel::testing_parse("cell1//pkg1:target1"),
                        CoercedAttr::Bool(BoolLiteral(true)),
                    ),
                    (
                        TargetLabel::testing_parse("cell2//pkg2:target2"),
                        CoercedAttr::Bool(BoolLiteral(false)),
                    ),
                ]),
                None,
            )
            .unwrap(),
        ));

        assert_eq!(s1 == s2, true);

        let s2 = CoercedAttr::Selector(Box::new(
            CoercedSelector::new(
                ArcSlice::new([
                    (
                        TargetLabel::testing_parse("cell2//pkg2:target2"),
                        CoercedAttr::Bool(BoolLiteral(false)),
                    ),
                    (
                        TargetLabel::testing_parse("cell1//pkg1:target1"),
                        CoercedAttr::Bool(BoolLiteral(true)),
                    ),
                ]),
                None,
            )
            .unwrap(),
        ));

        assert_eq!(s1 == s2, false);
    }

    #[test]
    fn select_the_most_specific() {
        struct SelectTestConfigurationContext {
            settings: BTreeMap<TargetLabel, ConfigSettingData>,
        }

        impl AttrConfigurationContext for SelectTestConfigurationContext {
            fn matches<'a>(&'a self, label: &TargetLabel) -> Option<&'a ConfigSettingData> {
                self.settings.get(label)
            }

            fn cfg(&self) -> ConfigurationNoExec {
                panic!()
            }

            fn exec_cfg(&self) -> ConfigurationNoExec {
                unimplemented!()
            }

            fn toolchain_cfg(&self) -> ConfigurationWithExec {
                panic!("not used in test")
            }

            fn platform_cfg(&self, _label: &TargetLabel) -> anyhow::Result<ConfigurationData> {
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

        let c_os = constraint_key("config//c:os");
        let c_linux = constraint_value("config//c:linux");
        let c_cpu = constraint_key("config//c:cpu");
        let c_arm64 = constraint_value("config//c:arm64");
        let c_x86_64 = constraint_value("config//c:x86_64");

        let linux = TargetLabel::testing_parse("config//:linux");
        let linux_arm64 = TargetLabel::testing_parse("config//:linux-arm64");
        let linux_x86_64 = TargetLabel::testing_parse("config//:linux-x86_64");

        let ctx = SelectTestConfigurationContext {
            settings: BTreeMap::from_iter([
                (
                    linux.dupe(),
                    ConfigSettingData {
                        constraints: BTreeMap::from_iter([(c_os.dupe(), c_linux.dupe())]),
                        buckconfigs: BTreeMap::new(),
                    },
                ),
                (
                    linux_arm64.dupe(),
                    ConfigSettingData {
                        constraints: BTreeMap::from_iter([
                            (c_os.dupe(), c_linux.dupe()),
                            (c_cpu.dupe(), c_arm64.dupe()),
                        ]),
                        buckconfigs: BTreeMap::new(),
                    },
                ),
                (
                    linux_x86_64.dupe(),
                    ConfigSettingData {
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
            CoercedAttr::Bool(BoolLiteral(true))
        }
        fn literal_str() -> CoercedAttr {
            CoercedAttr::String(StringLiteral(ArcStr::from("linux")))
        }

        // Test more specific is selected even if it is not first.
        let select_entries = Box::new([
            (linux.dupe(), literal_true()),
            (linux_x86_64.dupe(), literal_str()),
        ]);
        assert_eq!(
            Some(&literal_str()),
            CoercedAttr::select_the_most_specific(&ctx, &*select_entries).unwrap()
        );

        // Test more specific is selected even if it is first.
        let select_entries = Box::new([
            (linux_x86_64.dupe(), literal_str()),
            (linux.dupe(), literal_true()),
        ]);
        assert_eq!(
            Some(&literal_str()),
            CoercedAttr::select_the_most_specific(&ctx, &*select_entries).unwrap()
        );

        // Conflicting keys.
        let select_entries = Box::new([
            (linux_arm64.dupe(), literal_true()),
            (linux_x86_64.dupe(), literal_str()),
        ]);
        assert_eq!(
            "Both select keys `config//:linux-arm64` and `config//:linux-x86_64` \
            match the configuration, but neither is more specific",
            CoercedAttr::select_the_most_specific(&ctx, &*select_entries)
                .unwrap_err()
                .to_string()
        );
    }

    #[test]
    fn test_to_json_concat() {
        assert_eq!(
            r#"{"__type":"concat","items":["a","b","c","d"]}"#,
            CoercedAttr::Concat(Box::new([
                CoercedAttr::String(StringLiteral(ArcStr::from("a"))),
                CoercedAttr::String(StringLiteral(ArcStr::from("b"))),
                CoercedAttr::String(StringLiteral(ArcStr::from("c"))),
                CoercedAttr::String(StringLiteral(ArcStr::from("d"))),
            ]))
            .to_json(&AttrFmtContext::NO_CONTEXT)
            .unwrap()
            .to_string()
        );
    }

    #[test]
    fn test_to_json_selector() {
        assert_eq!(
            r#"{"__type":"selector","entries":{"DEFAULT":"ddd","config//:a":true,"config//:b":10}}"#,
            CoercedAttr::Selector(Box::new(
                CoercedSelector::new(
                    ArcSlice::new([
                        (
                            TargetLabel::testing_parse("config//:a"),
                            CoercedAttr::Bool(BoolLiteral(true))
                        ),
                        (
                            TargetLabel::testing_parse("config//:b"),
                            CoercedAttr::Int(10)
                        ),
                    ]),
                    Some(CoercedAttr::String(StringLiteral(ArcStr::from("ddd")))),
                )
                .unwrap()
            ))
            .to_json(&AttrFmtContext::NO_CONTEXT)
            .unwrap()
            .to_string()
        );
    }
}
