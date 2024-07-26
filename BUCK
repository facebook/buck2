load("@fbcode//target_determinator/macros:ci.bzl", "ci")
load("@fbcode_macros//build_defs:native_rules.bzl", "alias")
load("@fbsource//tools/build_defs/buck2:is_buck2.bzl", "is_buck2")
load(":defs.bzl?v2_only", "buck2_bundle")

oncall("build_infra")

alias(
    name = "buck2",
    actual = "//buck2/app/buck2:buck2-bin",
    labels = [ci.aarch64(ci.skip_test())],
)

# buildifier: disable=no-effect
buck2_bundle(
    name = "buck2_bundle",
    buck2 = "//buck2:buck2",
    buck2_client = "//buck2/app/buck2:buck2_client-bin",
    tpx = "//buck2/buck2_tpx_cli:buck2_tpx_cli",
    visibility = ["PUBLIC"],
) if is_buck2() else None

# For backcompat with bash aliases and so forth
alias(
    name = "symlinked_buck2_and_tpx",
    actual = ":buck2_bundle",
)
