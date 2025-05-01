/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::configuration::constraints::ConstraintKey;
use buck2_core::configuration::constraints::ConstraintValue;
use buck2_node::attrs::attr_type::bool::BoolLiteral;
use buck2_node::attrs::attr_type::string::StringLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coerced_attr::CoercedConcat;
use buck2_node::attrs::coerced_attr::CoercedSelector;
use buck2_node::attrs::fmt_context::AttrFmtContext;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;

#[test]
fn selector_equals_accounts_for_ordering() {
    let s1 = CoercedAttr::Selector(Box::new(
        CoercedSelector::new(
            ArcSlice::new([
                (
                    ConfigurationSettingKey::testing_parse("cell1//pkg1:target1"),
                    CoercedAttr::Bool(BoolLiteral(true)),
                ),
                (
                    ConfigurationSettingKey::testing_parse("cell2//pkg2:target2"),
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
                    ConfigurationSettingKey::testing_parse("cell1//pkg1:target1"),
                    CoercedAttr::Bool(BoolLiteral(true)),
                ),
                (
                    ConfigurationSettingKey::testing_parse("cell2//pkg2:target2"),
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
                    ConfigurationSettingKey::testing_parse("cell2//pkg2:target2"),
                    CoercedAttr::Bool(BoolLiteral(false)),
                ),
                (
                    ConfigurationSettingKey::testing_parse("cell1//pkg1:target1"),
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
    let c_os = ConstraintKey::testing_new("config//c:os");
    let c_linux = ConstraintValue::testing_new("config//c:linux");
    let c_cpu = ConstraintKey::testing_new("config//c:cpu");
    let c_arm64 = ConstraintValue::testing_new("config//c:arm64");
    let c_x86_64 = ConstraintValue::testing_new("config//c:x86_64");

    let linux = ConfigurationSettingKey::testing_parse("config//:linux");
    let linux_arm64 = ConfigurationSettingKey::testing_parse("config//:linux-arm64");
    let linux_x86_64 = ConfigurationSettingKey::testing_parse("config//:linux-x86_64");

    let linux_data =
        ConfigSettingData::testing_new(BTreeMap::from_iter([(c_os.dupe(), c_linux.dupe())]));
    let linux_arm64_data = ConfigSettingData::testing_new(BTreeMap::from_iter([
        (c_os.dupe(), c_linux.dupe()),
        (c_cpu.dupe(), c_arm64.dupe()),
    ]));
    let linux_x86_64_data = ConfigSettingData::testing_new(BTreeMap::from_iter([
        (c_os.dupe(), c_linux.dupe()),
        (c_cpu.dupe(), c_x86_64.dupe()),
    ]));

    let literal_true = CoercedAttr::Bool(BoolLiteral(true));
    let literal_str = CoercedAttr::String(StringLiteral(ArcStr::from("linux")));

    // Test more specific is selected even if it is not first.
    let select_entries = [
        (&linux, &linux_data, &literal_true),
        (&linux_x86_64, &linux_x86_64_data, &literal_str),
    ];
    assert_eq!(
        Some(&literal_str),
        CoercedAttr::select_the_most_specific(select_entries).unwrap()
    );

    // Test more specific is selected even if it is first.
    let select_entries = [
        (&linux_x86_64, &linux_x86_64_data, &literal_str),
        (&linux, &linux_data, &literal_true),
    ];
    assert_eq!(
        Some(&literal_str),
        CoercedAttr::select_the_most_specific(select_entries).unwrap()
    );

    // Conflicting keys.
    let select_entries = [
        (&linux_arm64, &linux_arm64_data, &literal_true),
        (&linux_x86_64, &linux_x86_64_data, &literal_str),
    ];
    assert_eq!(
        "Both select keys `config//:linux-arm64` and `config//:linux-x86_64` \
            match the configuration, but neither is more specific",
        CoercedAttr::select_the_most_specific(select_entries)
            .unwrap_err()
            .to_string()
    );
}

#[test] // T177093673
fn test_select_refines_bug() {
    let c_windows = (
        ConstraintKey::testing_new("config//c:os"),
        ConstraintValue::testing_new("config//c:windows"),
    );
    let c_x86_64 = (
        ConstraintKey::testing_new("config//c:cpu"),
        ConstraintValue::testing_new("config//c:x86_64"),
    );

    let windows = ConfigurationSettingKey::testing_parse("config//:windows");
    let x86_64 = ConfigurationSettingKey::testing_parse("config//:x86_64");
    let windows_x86_64 = ConfigurationSettingKey::testing_parse("config//:windows-x86_64");

    let windows_data = ConfigSettingData::testing_new(BTreeMap::from_iter([c_windows.dupe()]));
    let x86_64_data = ConfigSettingData::testing_new(BTreeMap::from_iter([c_x86_64.dupe()]));
    let windows_x86_64_data =
        ConfigSettingData::testing_new(BTreeMap::from_iter([c_windows, c_x86_64]));

    let value_windows = CoercedAttr::String(StringLiteral(ArcStr::from("windows")));
    let value_x86_64 = CoercedAttr::String(StringLiteral(ArcStr::from("x86_64")));
    let value_windows_x86_64 = CoercedAttr::String(StringLiteral(ArcStr::from("windows-x86_64")));
    let select_entries = [
        (&windows, &windows_data, &value_windows),
        (&x86_64, &x86_64_data, &value_x86_64),
        (&windows_x86_64, &windows_x86_64_data, &value_windows_x86_64),
    ];

    assert_eq!(
        Some(&value_windows_x86_64),
        CoercedAttr::select_the_most_specific(select_entries).unwrap()
    );
}

#[test]
fn test_to_json_concat() {
    assert_eq!(
        r#"{"__type":"concat","items":["a","b","c","d"]}"#,
        CoercedAttr::Concat(CoercedConcat(Box::new([
            CoercedAttr::String(StringLiteral(ArcStr::from("a"))),
            CoercedAttr::String(StringLiteral(ArcStr::from("b"))),
            CoercedAttr::String(StringLiteral(ArcStr::from("c"))),
            CoercedAttr::String(StringLiteral(ArcStr::from("d"))),
        ])))
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
                        ConfigurationSettingKey::testing_parse("config//:a"),
                        CoercedAttr::Bool(BoolLiteral(true))
                    ),
                    (
                        ConfigurationSettingKey::testing_parse("config//:b"),
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
