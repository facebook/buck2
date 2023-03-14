/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Tests for `PACKAGE` files.

use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::package::PackageLabel;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use indoc::indoc;

use crate::tests::calculation;
use crate::tests::root_cell;

const RULES: &str = r#"
rrr = rule(
    impl = lambda ctx: DefaultInfo(),
    attrs = {
        "value": attrs.string(),
    },
)
"#;

#[tokio::test]
async fn test_package_value_same_dir_package_file() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES);
    fs.write_file(
        "headphones/PACKAGE",
        "write_package_value('aaa.bbb', 'ccc')",
    );
    fs.write_file(
        "headphones/BUCK",
        indoc!(
            r#"
                load("//:rules.bzl", "rrr")
                rrr(
                    name = "headphones",
                    value = read_package_value("aaa.bbb"),
                )
        "#
        ),
    );

    let ctx = calculation(&fs).await;
    let interpreter = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(
            PackageLabel::testing_parse("root//headphones"),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )
        .await
        .unwrap();

    let target_nodes: Vec<_> = result.targets().values().collect();
    assert_eq!(1, target_nodes.len());
    let target_node = &target_nodes[0];
    assert_eq!(
        "\"ccc\"",
        target_node
            .attr("value", AttrInspectOptions::DefinedOnly)
            .unwrap()
            .unwrap()
            .as_display_no_ctx()
            .to_string()
    );
}

#[tokio::test]
async fn test_package_value_parent_dir_package_file() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES);
    fs.write_file("PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file(
        "trackpad/BUCK",
        indoc!(
            r#"
                load("//:rules.bzl", "rrr")
                rrr(
                    name = "trackpad",
                    value = read_package_value("aaa.bbb"),
                )
            "#
        ),
    );

    let ctx = calculation(&fs).await;
    let interpreter = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(
            PackageLabel::testing_parse("root//trackpad"),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )
        .await
        .unwrap();

    let target_nodes: Vec<_> = result.targets().values().collect();
    assert_eq!(1, target_nodes.len());
    let target_node = &target_nodes[0];
    assert_eq!(
        "\"ccc\"",
        target_node
            .attr("value", AttrInspectOptions::DefinedOnly)
            .unwrap()
            .unwrap()
            .as_display_no_ctx()
            .to_string()
    );
}

#[tokio::test]
async fn test_overwrite_package_value_not_allowed_without_overwrite_flag() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file("foo/PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file("foo/BUCK", "");

    let ctx = calculation(&fs).await;
    let interpreter = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();
    let err = interpreter
        .eval_build_file(
            PackageLabel::testing_parse("root//foo"),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )
        .await;
    assert!(
        format!("{:?}", err)
            .contains("key set in parent `PACKAGE` file, and overwrite flag is not set"),
        "err = {:?}",
        err
    );
}

#[tokio::test]
async fn test_overwrite_package_value_with_flag() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES);
    fs.write_file("PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file(
        "foo/PACKAGE",
        "write_package_value('aaa.bbb', 'ddd', overwrite = True)",
    );
    fs.write_file(
        "foo/BUCK",
        indoc!(
            r#"
                load("//:rules.bzl", "rrr")
                rrr(
                    name = "foo",
                    value = read_package_value("aaa.bbb"),
                )
            "#
        ),
    );

    let ctx = calculation(&fs).await;
    let interpreter = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(
            PackageLabel::testing_parse("root//foo"),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )
        .await
        .unwrap();

    let target_nodes: Vec<_> = result.targets().values().collect();
    assert_eq!(1, target_nodes.len());
    let target_node = &target_nodes[0];
    assert_eq!(
        "\"ddd\"",
        target_node
            .attr("value", AttrInspectOptions::DefinedOnly)
            .unwrap()
            .unwrap()
            .as_display_no_ctx()
            .to_string()
    );
}
