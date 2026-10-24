/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeMap;

use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::configuration::constraints::ConstraintKey;
use buck2_core::configuration::constraints::ConstraintValue;
use buck2_node::attrs::arena::CoercedAttrArena;
use buck2_node::attrs::arena::OwnedCoercedAttr;
use buck2_node::attrs::attr_type::bool::BoolLiteral;
use buck2_node::attrs::attr_type::string::StringLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::fmt_context::AttrFmtContext;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;

fn key(s: &str) -> ConfigurationSettingKey {
    ConfigurationSettingKey::testing_parse(s)
}

fn to_json(value: &OwnedCoercedAttr) -> String {
    value
        .as_ref()
        .to_json(&AttrFmtContext::NO_CONTEXT)
        .unwrap()
        .to_string()
}

#[test]
fn selector_identity_accounts_for_ordering() -> buck2_error::Result<()> {
    let mut b = CoercedAttrArena::builder();
    let t = b.encode_bool(true)?;
    let f = b.encode_bool(false)?;
    let s1 = b.encode_selector(
        &[
            (key("cell1//pkg1:target1"), t),
            (key("cell2//pkg2:target2"), f),
        ],
        Some(f),
    )?;
    let s2 = b.encode_selector(
        &[
            (key("cell1//pkg1:target1"), t),
            (key("cell2//pkg2:target2"), f),
        ],
        Some(f),
    )?;
    let reordered = b.encode_selector(
        &[
            (key("cell2//pkg2:target2"), f),
            (key("cell1//pkg1:target1"), t),
        ],
        Some(f),
    )?;
    let nested_s1 = b.encode_list(&[s1])?;
    let nested_s2 = b.encode_list(&[s2])?;
    let arena = b.finish();

    assert_eq!(s1, s2);
    assert_eq!(nested_s1, nested_s2);
    assert_ne!(s1.word(), reordered.word());
    assert!(
        !arena
            .view(s1.word())
            .content_eq(&arena.view(reordered.word()))
    );
    Ok(())
}

#[test]
fn select_the_most_specific() {
    let c_os = ConstraintKey::testing_new("config//c:os");
    let c_linux = ConstraintValue::testing_new("config//c:linux", None);
    let c_cpu = ConstraintKey::testing_new("config//c:cpu");
    let c_arm64 = ConstraintValue::testing_new("config//c:arm64", None);
    let c_x86_64 = ConstraintValue::testing_new("config//c:x86_64", None);

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

    // Conflicting keys with different values.
    let select_entries = [
        (&linux_arm64, &linux_arm64_data, &literal_true),
        (&linux_x86_64, &linux_x86_64_data, &literal_str),
    ];
    assert_eq!(
        "Both select keys `config//:linux-arm64` and `config//:linux-x86_64` \
            match the configuration, but neither is more specific and they have different values",
        CoercedAttr::select_the_most_specific(select_entries)
            .unwrap_err()
            .to_string()
    );

    // Conflicting keys with same values - should not error.
    let select_entries = [
        (&linux_arm64, &linux_arm64_data, &literal_true),
        (&linux_x86_64, &linux_x86_64_data, &literal_true),
    ];
    assert_eq!(
        Some(&literal_true),
        CoercedAttr::select_the_most_specific(select_entries).unwrap()
    );
}

#[test]
fn select_the_first_match() {
    // This test simulates the resolved entries flow from `resolve_single_select`:
    // After filtering to only matching config settings, `select_the_first_match`
    // returns the first entry in iteration order, regardless of specificity.
    // This contrasts with `select_the_most_specific` which returns the most refined match.

    let c_os = ConstraintKey::testing_new("config//c:os");
    let c_linux = ConstraintValue::testing_new("config//c:linux", None);
    let c_cpu = ConstraintKey::testing_new("config//c:cpu");
    let c_x86_64 = ConstraintValue::testing_new("config//c:x86_64", None);

    let linux = ConfigurationSettingKey::testing_parse("config//:linux");
    let linux_x86_64 = ConfigurationSettingKey::testing_parse("config//:linux-x86_64");

    let linux_data =
        ConfigSettingData::testing_new(BTreeMap::from_iter([(c_os.dupe(), c_linux.dupe())]));
    let linux_x86_64_data = ConfigSettingData::testing_new(BTreeMap::from_iter([
        (c_os.dupe(), c_linux.dupe()),
        (c_cpu.dupe(), c_x86_64.dupe()),
    ]));

    let literal_true = CoercedAttr::Bool(BoolLiteral(true));
    let literal_str = CoercedAttr::String(StringLiteral(ArcStr::from("linux")));

    // Simulate resolved_entries where both keys match the target configuration (linux-x86_64).
    // The less specific entry (linux) comes first in the original select() definition.
    let resolved_entries = [
        (&linux, &linux_data, &literal_true),
        (&linux_x86_64, &linux_x86_64_data, &literal_str),
    ];

    // select_the_first_match returns the first entry (literal_true from linux)
    assert_eq!(
        Some(&literal_true),
        CoercedAttr::select_the_first_match(resolved_entries.iter().copied())
    );
    // select_the_most_specific returns the more specific entry (literal_str from linux_x86_64)
    assert_eq!(
        Some(&literal_str),
        CoercedAttr::select_the_most_specific(resolved_entries.iter().copied()).unwrap()
    );

    // When the more specific entry comes first in the original order,
    // both methods return the same value.
    let resolved_entries = [
        (&linux_x86_64, &linux_x86_64_data, &literal_str),
        (&linux, &linux_data, &literal_true),
    ];

    assert_eq!(
        Some(&literal_str),
        CoercedAttr::select_the_first_match(resolved_entries.iter().copied())
    );
    assert_eq!(
        Some(&literal_str),
        CoercedAttr::select_the_most_specific(resolved_entries.iter().copied()).unwrap()
    );

    // Empty resolved entries returns None.
    let resolved_entries: [(&ConfigurationSettingKey, &ConfigSettingData, &CoercedAttr); 0] = [];
    assert_eq!(
        None,
        CoercedAttr::select_the_first_match(resolved_entries.iter().copied())
    );
}

#[test] // T177093673
fn test_select_refines_bug() {
    let c_windows = (
        ConstraintKey::testing_new("config//c:os"),
        ConstraintValue::testing_new("config//c:windows", None),
    );
    let c_x86_64 = (
        ConstraintKey::testing_new("config//c:cpu"),
        ConstraintValue::testing_new("config//c:x86_64", None),
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
fn test_to_json_concat() -> buck2_error::Result<()> {
    let concat = OwnedCoercedAttr::new_with(|b| {
        let items = ["a", "b", "c", "d"]
            .iter()
            .map(|s| b.encode_string(s))
            .collect::<buck2_error::Result<Vec<_>>>()?;
        b.encode_concat(&items)
    })?;
    assert_eq!(
        r#"{"__type":"concat","items":["a","b","c","d"]}"#,
        to_json(&concat)
    );
    Ok(())
}

#[test]
fn test_to_json_selector() -> buck2_error::Result<()> {
    let selector = OwnedCoercedAttr::new_with(|b| {
        let t = b.encode_bool(true)?;
        let ten = b.encode_int(10)?;
        let ddd = b.encode_string("ddd")?;
        b.encode_selector(
            &[(key("config//:a"), t), (key("config//:b"), ten)],
            Some(ddd),
        )
    })?;
    assert_eq!(
        r#"{"__type":"selector","entries":{"DEFAULT":"ddd","config//:a":true,"config//:b":10}}"#,
        to_json(&selector)
    );
    Ok(())
}

#[test]
fn select_the_most_specific_with_subtargets() {
    // Test select resolution with the new unified constraint syntax (subtargets)
    let c_os = ConstraintKey::testing_new("config//c:os");
    let c_linux = ConstraintValue::testing_new("config//c:os", Some("linux"));

    let c_cpu = ConstraintKey::testing_new("config//c:cpu");
    let c_arm64 = ConstraintValue::testing_new("config//c:cpu", Some("arm64"));
    let c_x86_64 = ConstraintValue::testing_new("config//c:cpu", Some("x86_64"));

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

    // Conflicting keys with different values.
    let select_entries = [
        (&linux_arm64, &linux_arm64_data, &literal_true),
        (&linux_x86_64, &linux_x86_64_data, &literal_str),
    ];
    assert_eq!(
        "Both select keys `config//:linux-arm64` and `config//:linux-x86_64` \
            match the configuration, but neither is more specific and they have different values",
        CoercedAttr::select_the_most_specific(select_entries)
            .unwrap_err()
            .to_string()
    );

    // Conflicting keys with same values - should not error.
    let select_entries = [
        (&linux_arm64, &linux_arm64_data, &literal_true),
        (&linux_x86_64, &linux_x86_64_data, &literal_true),
    ];
    assert_eq!(
        Some(&literal_true),
        CoercedAttr::select_the_most_specific(select_entries).unwrap()
    );
}
