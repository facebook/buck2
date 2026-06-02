/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::target::label::label::TargetLabel;
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

    let mut ctx = calculation(&fs).await;

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
async fn test_package_target_name_glob_visibility() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    fs.write_file(
        "juxtaposition/PACKAGE",
        r#"
package(
    visibility = [target_name_glob(["*-test"], within = ["//consumer/..."])],
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

    let mut ctx = calculation(&fs).await;

    let a = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    let visibility = a.visibility().unwrap();
    assert!(
        visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//consumer:unit-test"))
            .unwrap()
    );
    assert!(
        !visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//consumer:library"))
            .unwrap()
    );
    assert!(
        !visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//other:unit-test"))
            .unwrap()
    );
}

#[tokio::test]
async fn test_package_target_name_glob_multi_visibility() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    fs.write_file(
        "juxtaposition/PACKAGE",
        r#"
package(
    visibility = [
        target_name_glob(["*-test", "*-bench"], within = ["//consumer/..."]),
    ],
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

    let mut ctx = calculation(&fs).await;

    let a = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    let visibility = a.visibility().unwrap();
    // Any of the listed globs matches inside the scope.
    assert!(
        visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//consumer:unit-test"))
            .unwrap(),
        "first glob should match"
    );
    assert!(
        visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//consumer:micro-bench"))
            .unwrap(),
        "second glob should match"
    );
    // Negative case: name doesn't match either glob.
    assert!(
        !visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//consumer:library"))
            .unwrap(),
        "non-matching name must not pass"
    );
    // Negative case: matching name outside the scope.
    assert!(
        !visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//other:unit-test"))
            .unwrap(),
        "outside-scope target must not pass"
    );
}

#[tokio::test]
async fn test_package_target_name_glob_multiple_within() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    fs.write_file(
        "juxtaposition/PACKAGE",
        r#"
package(
    visibility = [
        target_name_glob(["*-test"], within = ["//consumer/...", "//other/..."]),
    ],
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

    let mut ctx = calculation(&fs).await;

    let a = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    let visibility = a.visibility().unwrap();
    // Matches inside either listed `within` scope (scopes are OR-ed).
    assert!(
        visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//consumer:unit-test"))
            .unwrap(),
        "first within scope should match"
    );
    assert!(
        visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//other:unit-test"))
            .unwrap(),
        "second within scope should match"
    );
    // Negative: name matches but package is in neither scope.
    assert!(
        !visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//elsewhere:unit-test"))
            .unwrap(),
        "target outside all within scopes must not pass"
    );
    // Negative: package in a scope but the name matches no glob.
    assert!(
        !visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//consumer:library"))
            .unwrap(),
        "non-matching name must not pass"
    );
}

#[tokio::test]
async fn test_target_name_glob_in_buck_visibility() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    // `target_name_glob` directly on a rule's `visibility` exercises the
    // attribute coercer path, distinct from the `package()` PACKAGE parser.
    fs.write_file(
        "juxtaposition/BUCK",
        r#"
load("//:rules.bzl", "simple")
simple(
    name = "a",
    visibility = [target_name_glob(["*-test"], within = ["//consumer/..."])],
)
"#,
    );

    let mut ctx = calculation(&fs).await;

    let a = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    let visibility = a.visibility().unwrap();
    assert!(
        visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//consumer:unit-test"))
            .unwrap(),
        "name + scope match should be visible"
    );
    // Negative: name matches but package is outside the within scope.
    assert!(
        !visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//other:unit-test"))
            .unwrap(),
        "out-of-scope package must not be visible"
    );
    // Negative: package in scope but name matches no glob.
    assert!(
        !visibility
            .0
            .matches_target(&TargetLabel::testing_parse("root//consumer:library"))
            .unwrap(),
        "non-matching name must not be visible"
    );
}

#[tokio::test]
async fn test_package_visibility_rejects_non_str() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    // `42` is neither a string nor a `target_name_glob`; the `Either` unpack in
    // `package()` must reject it rather than silently accepting it.
    fs.write_file("juxtaposition/PACKAGE", "package(visibility = [42])\n");
    fs.write_file(
        "juxtaposition/BUCK",
        r#"
load("//:rules.bzl", "simple")
simple(name = "a")
"#,
    );

    let mut ctx = calculation(&fs).await;

    let err = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .expect_err("non-str/non-target_name_glob visibility element must be rejected");
    let msg = format!("{err:?}").to_lowercase();
    assert!(
        msg.contains("target_name_glob") || msg.contains("expected") || msg.contains("int"),
        "expected a type-mismatch error, got: {msg}"
    );
}

#[tokio::test]
async fn test_package_target_name_glob_rejects_public_within() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    // `PUBLIC` is a target name, not a package scope; using it in `within` must
    // be rejected with a clear error rather than a confusing parse failure.
    fs.write_file(
        "juxtaposition/PACKAGE",
        r#"
package(
    visibility = [target_name_glob(["*-test"], within = ["PUBLIC"])],
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

    let mut ctx = calculation(&fs).await;

    let err = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .expect_err("`PUBLIC` in `within` must be rejected");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("`PUBLIC` is not a valid `within`"),
        "expected PublicNotAllowedInWithin error, got: {msg}"
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

    let mut ctx = calculation(&fs).await;

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

    let mut ctx = calculation(&fs).await;

    let a = ctx
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    assert_eq!(
        &VisibilitySpecification::testing_parse(&["root//bbb/..."]),
        a.visibility().unwrap(),
    );
}
