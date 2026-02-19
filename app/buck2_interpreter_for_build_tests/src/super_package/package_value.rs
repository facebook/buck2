/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Tests for `PACKAGE` files.

use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::package::PackageLabel;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter::paths::path::OwnedStarlarkPath;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use dice::CancellationContext;
use indoc::indoc;

use crate::tests::calculation;

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
        indoc!(
            r#"
            write_package_value('aaa.bbb', 'ccc')
            test_config_unification_rollout(enabled=True)
            "#
        ),
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

    let package_label = PackageLabel::testing_parse("root//headphones");

    let mut ctx = calculation(&fs).await;
    let mut interpreter = ctx
        .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
            PackageFilePath::package_file_for_dir(package_label.as_cell_path()),
        ))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(package_label, CancellationContext::testing())
        .await
        .1
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
            .value
            .as_display_no_ctx()
            .to_string()
    );
    assert!(target_node.test_config_unification_rollout());
}

#[tokio::test]
async fn test_package_value_parent_dir_package_file() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES);
    fs.write_file(
        "PACKAGE",
        indoc!(
            r#"
            write_package_value('aaa.bbb', 'ccc')
            test_config_unification_rollout(enabled=True)
            "#
        ),
    );
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

    let package_label = PackageLabel::testing_parse("root//trackpad");

    let mut ctx = calculation(&fs).await;
    let mut interpreter = ctx
        .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
            PackageFilePath::package_file_for_dir(package_label.as_cell_path()),
        ))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(package_label, CancellationContext::testing())
        .await
        .1
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
            .value
            .as_display_no_ctx()
            .to_string()
    );
    assert!(target_node.test_config_unification_rollout());
}

#[tokio::test]
async fn test_overwrite_package_value_not_allowed_without_overwrite_flag() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file("foo/PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file("foo/BUCK", "");

    let package_label = PackageLabel::testing_parse("root//foo");

    let mut ctx = calculation(&fs).await;
    let mut interpreter = ctx
        .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
            PackageFilePath::package_file_for_dir(package_label.as_cell_path()),
        ))
        .await
        .unwrap();
    let err = interpreter
        .eval_build_file(package_label, CancellationContext::testing())
        .await
        .1;
    assert!(
        format!("{err:?}")
            .contains("key set in parent `PACKAGE` file, and overwrite flag is not set"),
        "err = {err:?}"
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

    let mut ctx = calculation(&fs).await;
    let result = ctx
        .get_interpreter_results(PackageLabel::testing_parse("root//foo"))
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
            .value
            .as_display_no_ctx()
            .to_string()
    );
}

#[tokio::test]
async fn test_read_parent_package_value() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES);
    fs.write_file("PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file(
        "foo/PACKAGE",
        "write_package_value('xxx.yyy', read_parent_package_value('aaa.bbb'))",
    );
    fs.write_file(
        "foo/BUCK",
        indoc!(
            r#"
                load("//:rules.bzl", "rrr")
                rrr(
                    name = "trackpad",
                    value = read_package_value("xxx.yyy"),
                )
            "#
        ),
    );

    let package_label = PackageLabel::testing_parse("root//foo");

    let mut ctx = calculation(&fs).await;
    let mut interpreter = ctx
        .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
            PackageFilePath::package_file_for_dir(package_label.as_cell_path()),
        ))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(package_label, CancellationContext::testing())
        .await
        .1
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
            .value
            .as_display_no_ctx()
            .to_string()
    );
}

#[tokio::test]
async fn test_read_parent_package_value_from_bzl() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES);
    fs.write_file("PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file(
        "test.bzl",
        indoc!(
            r#"
            def test():
                write_package_value('xxx.yyy', read_parent_package_value('aaa.bbb'))
            "#
        ),
    );
    fs.write_file(
        "foo/PACKAGE",
        indoc!(
            r#"
            load("//:test.bzl", "test")
            test()
            "#
        ),
    );
    fs.write_file(
        "foo/BUCK",
        indoc!(
            r#"
                load("//:rules.bzl", "rrr")
                rrr(
                    name = "trackpad",
                    value = read_package_value("xxx.yyy"),
                )
            "#
        ),
    );

    let package_label = PackageLabel::testing_parse("root//foo");

    let mut ctx = calculation(&fs).await;
    let mut interpreter = ctx
        .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
            PackageFilePath::package_file_for_dir(package_label.as_cell_path()),
        ))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(package_label, CancellationContext::testing())
        .await
        .1
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
            .value
            .as_display_no_ctx()
            .to_string()
    );
}

#[tokio::test]
async fn test_read_parent_package_value_is_suggested_in_package_file() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file("foo/PACKAGE", "read_package_value('aaa.bbb')");
    fs.write_file("foo/BUCK", "");

    let package_label = PackageLabel::testing_parse("root//foo");

    let mut ctx = calculation(&fs).await;
    let mut interpreter = ctx
        .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
            PackageFilePath::package_file_for_dir(package_label.as_cell_path()),
        ))
        .await
        .unwrap();
    let err = interpreter
        .eval_build_file(package_label, CancellationContext::testing())
        .await
        .1;
    assert!(
        format!("{err:?}")
            .contains("In a Package context, consider using `read_parent_package_value`"),
        "err = {err:?}"
    );
}

#[tokio::test]
async fn test_read_parent_package_value_is_suggested_in_bzl_file() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("PACKAGE", "write_package_value('aaa.bbb', 'ccc')");
    fs.write_file(
        "test.bzl",
        indoc!(
            r#"
            def test():
                return read_package_value("aaa.bbb")
            "#
        ),
    );
    fs.write_file(
        "foo/PACKAGE",
        indoc!(
            r#"
                load("//:test.bzl", "test")
                test()
            "#
        ),
    );
    fs.write_file("foo/BUCK", "");

    let package_label = PackageLabel::testing_parse("root//foo");

    let mut ctx = calculation(&fs).await;
    let mut interpreter = ctx
        .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
            PackageFilePath::package_file_for_dir(package_label.as_cell_path()),
        ))
        .await
        .unwrap();
    let err = interpreter
        .eval_build_file(package_label, CancellationContext::testing())
        .await;
    assert!(
        format!("{err:?}")
            .contains("In a Package context, consider using `read_parent_package_value`"),
        "err = {err:?}"
    );
}

#[tokio::test]
async fn test_config_unification_rollout_function_override() {
    let fs = ProjectRootTemp::new().unwrap();
    fs.write_file("rules.bzl", RULES);
    fs.write_file(
        "PACKAGE",
        indoc!(
            r#"
            write_package_value('aaa.bbb', 'ccc')
            test_config_unification_rollout(enabled=True)
            "#
        ),
    );
    fs.write_file(
        "foo/PACKAGE",
        "test_config_unification_rollout(enabled=False)",
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
    let package_label = PackageLabel::testing_parse("root//foo");
    let mut ctx = calculation(&fs).await;
    let mut interpreter = ctx
        .get_interpreter_calculator(OwnedStarlarkPath::PackageFile(
            PackageFilePath::package_file_for_dir(package_label.as_cell_path()),
        ))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(package_label, CancellationContext::testing())
        .await
        .1
        .unwrap();

    let target_nodes: Vec<_> = result.targets().values().collect();
    assert_eq!(1, target_nodes.len());
    let target_node = &target_nodes[0];
    assert!(!target_node.test_config_unification_rollout());
}
