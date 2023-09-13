load("@fbcode//target_determinator/macros:ci.bzl", "ci")
load("@fbcode_macros//build_defs:native_rules.bzl", "alias")
load("@fbsource//tools/build_defs/buck2:is_buck2.bzl", "is_buck2")
load(":defs.bzl?v2_only", "symlinked_buck2_and_tpx")

oncall("build_infra")

alias(
    name = "buck2",
    actual = "//buck2/app/buck2:buck2-bin",
    labels = [ci.aarch64(ci.skip_test())],
)

# buildifier: disable=no-effect
symlinked_buck2_and_tpx(
    name = "symlinked_buck2_and_tpx",
    buck2 = "//buck2:buck2",
    labels = [ci.skip_test(ci.windows(ci.opt()))],
    tpx = "//buck2/buck2_tpx_cli:buck2_tpx_cli",
) if is_buck2() else None
