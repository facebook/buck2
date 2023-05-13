/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::target::label::TargetLabel;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::visibility::VisibilitySpecification;

use crate::tests::calculation;

const RULES_BZL: &str = r#"
simple = rule(
    impl = lambda ctx: fail(),
    attrs = {},
)
"#;

#[tokio::test]
async fn test_package() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    fs.write_file(
        "juxtaposition/PACKAGE",
        r#"
package(
    visibility = ["//aaa/..."],
    within_view = ["//bbb/..."],
    inherit = True,
)
"#,
    );
    fs.write_file(
        "juxtaposition/BUCK",
        r#"
load("//:rules.bzl", "simple")
simple(name = "a")
"#,
    );

    let ctx = calculation(&fs).await;

    let a = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    assert_eq!(
        &VisibilitySpecification::testing_parse(&["root//aaa/..."]),
        a.visibility().unwrap(),
    );
}

#[tokio::test]
async fn test_package_inherit() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    fs.write_file(
        "PACKAGE",
        r#"
package(
    visibility = ["//aaa/..."],
)
"#,
    );
    fs.write_file(
        "juxtaposition/PACKAGE",
        r#"
package(
    visibility = ["//bbb/..."],
    inherit = True,
)
"#,
    );
    fs.write_file(
        "juxtaposition/BUCK",
        r#"
load("//:rules.bzl", "simple")
simple(name = "a")
"#,
    );

    let ctx = calculation(&fs).await;

    let a = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    assert_eq!(
        &VisibilitySpecification::testing_parse(&["root//aaa/...", "root//bbb/..."]),
        a.visibility().unwrap(),
    );
}

#[tokio::test]
async fn test_package_not_inherit() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    fs.write_file(
        "PACKAGE",
        r#"
package(
    visibility = ["//aaa/..."],
)
"#,
    );
    fs.write_file(
        "juxtaposition/PACKAGE",
        r#"
package(
    visibility = ["//bbb/..."],
)
"#,
    );
    fs.write_file(
        "juxtaposition/BUCK",
        r#"
load("//:rules.bzl", "simple")
simple(name = "a")
"#,
    );

    let ctx = calculation(&fs).await;

    let a = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    assert_eq!(
        &VisibilitySpecification::testing_parse(&["root//bbb/..."]),
        a.visibility().unwrap(),
    );
}
