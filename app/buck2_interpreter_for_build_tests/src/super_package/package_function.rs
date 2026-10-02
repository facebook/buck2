/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::settings::PackageVisibilityDefaultIntersection;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::target::label::label::TargetLabel;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::visibility::VisibilitySpecification;

use crate::tests::calculation;
use crate::tests::calculation_with_package_visibility_mode;

const RULES_BZL: &str = r#"
simple = rule(
    impl = lambda ctx: fail(),
    attrs = {},
)
"#;

/// Evaluate target `root//juxtaposition:a` under a `PACKAGE` file with the
/// given body and the given default-intersection mode.
async fn target_a_with_package(
    package_body: &str,
    mode: PackageVisibilityDefaultIntersection,
) -> TargetNode {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    fs.write_file("juxtaposition/PACKAGE", package_body);
    fs.write_file(
        "juxtaposition/BUCK",
        "load(\"//:rules.bzl\", \"simple\")\nsimple(name = \"a\")\n",
    );

    let ctx = calculation_with_package_visibility_mode(&fs, mode).await;

    ctx.ctx()
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap()
}

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
        .ctx()
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

    let ctx = calculation(&fs).await;

    let a = ctx
        .ctx()
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

    let ctx = calculation(&fs).await;

    let a = ctx
        .ctx()
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

    let ctx = calculation(&fs).await;

    let a = ctx
        .ctx()
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

    let ctx = calculation(&fs).await;

    let a = ctx
        .ctx()
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

    let ctx = calculation(&fs).await;

    let err = ctx
        .ctx()
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

    let ctx = calculation(&fs).await;

    let err = ctx
        .ctx()
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

    let ctx = calculation(&fs).await;

    let a = ctx
        .ctx()
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    assert_eq!(
        &VisibilitySpecification::testing_parse(&["root//aaa/...", "root//bbb/..."]),
        a.visibility().unwrap(),
    );
}

#[tokio::test]
async fn test_package_visibility_off_ignores_ordinary_visibility() {
    let a = target_a_with_package(
        "package(\n    visibility = [\"//allowed/...\"],\n)\n",
        PackageVisibilityDefaultIntersection::Off,
    )
    .await;

    assert!(
        a.visibility_intersection().is_unrestricted(),
        "off mode without a marker must leave the intersection empty, got: {}",
        a.visibility_intersection(),
    );
}

#[tokio::test]
async fn test_package_visibility_marker_enforces_when_off() {
    let a = target_a_with_package(
        "enforce_visibility_intersection()\npackage(\n    visibility = [\"//allowed/...\"],\n)\n",
        PackageVisibilityDefaultIntersection::Off,
    )
    .await;

    assert!(
        a.visibility_intersection()
            .matches(&TargetLabel::testing_parse("root//allowed:lib"))
            .unwrap(),
        "marker-selected visibility must restrict to //allowed/..."
    );
    assert!(
        !a.visibility_intersection()
            .matches(&TargetLabel::testing_parse("root//other:lib"))
            .unwrap(),
        "marker-selected visibility must block //other/..."
    );
}

#[tokio::test]
async fn test_package_visibility_enforce_without_marker() {
    let a = target_a_with_package(
        "package(\n    visibility = [\"//allowed/...\"],\n)\n",
        PackageVisibilityDefaultIntersection::Enforce,
    )
    .await;

    assert!(
        a.visibility_intersection()
            .matches(&TargetLabel::testing_parse("root//allowed:lib"))
            .unwrap(),
        "enforce mode must restrict to //allowed/... without a marker"
    );
    assert!(
        !a.visibility_intersection()
            .matches(&TargetLabel::testing_parse("root//other:lib"))
            .unwrap(),
        "enforce mode must block //other/... without a marker"
    );
}

#[tokio::test]
async fn test_package_visibility_audit_computes_intersection_like_enforce() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("rules.bzl", RULES_BZL);
    fs.write_file(
        "juxtaposition/PACKAGE",
        r#"
package(
    visibility = ["//allowed/..."],
)
"#,
    );
    fs.write_file(
        "juxtaposition/BUCK",
        r#"
load("//:rules.bzl", "simple")
simple(name = "a")
# Explicit `PUBLIC` own-visibility isolates the intersection: any blocking
# below comes from the intersection layer, not from the target itself.
simple(name = "b", visibility = ["PUBLIC"])
"#,
    );

    let ctx =
        calculation_with_package_visibility_mode(&fs, PackageVisibilityDefaultIntersection::Audit)
            .await;

    let a = ctx
        .ctx()
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    assert!(
        a.visibility_intersection()
            .matches(&TargetLabel::testing_parse("root//allowed:lib"))
            .unwrap(),
        "audit mode must cap to //allowed/... like enforce"
    );
    assert!(
        !a.visibility_intersection()
            .matches(&TargetLabel::testing_parse("root//other:lib"))
            .unwrap(),
        "audit mode must block //other/... like enforce"
    );

    // `b` is `PUBLIC`, so the intersection alone decides.
    let b = ctx
        .ctx()
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:b"))
        .await
        .unwrap();
    let allowed = TargetLabel::testing_parse("root//allowed:lib");
    let other = TargetLabel::testing_parse("root//other:lib");
    assert!(b.is_visible_to(&allowed).unwrap());
    assert!(!b.is_visible_to(&other).unwrap());

    let enforce_ctx = calculation_with_package_visibility_mode(
        &fs,
        PackageVisibilityDefaultIntersection::Enforce,
    )
    .await;
    let enforce_b = enforce_ctx
        .ctx()
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:b"))
        .await
        .unwrap();
    for consumer in [&allowed, &other] {
        assert_eq!(
            b.visibility_intersection().matches(consumer).unwrap(),
            enforce_b
                .visibility_intersection()
                .matches(consumer)
                .unwrap(),
            "audit and enforce intersections must agree for {consumer}",
        );
    }
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
        .ctx()
        .get_target_node(&TargetLabel::testing_parse("root//juxtaposition:a"))
        .await
        .unwrap();

    assert_eq!(
        &VisibilitySpecification::testing_parse(&["root//bbb/..."]),
        a.visibility().unwrap(),
    );
}
