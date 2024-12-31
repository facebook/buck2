load("@fbcode_macros//build_defs:native_rules.bzl", "alias")
load("@fbsource//tools/target_determinator/macros:ci.bzl", "ci")
load(":defs.bzl", "buck2_bundle")

oncall("build_infra")

alias(
    name = "buck2",
    actual = "//buck2/app/buck2:buck2-bin",
    labels = [ci.aarch64(ci.skip_test())],
)

buck2_bundle(
    name = "buck2_bundle",
    buck2 = "//buck2:buck2",
    buck2_client = "//buck2/app/buck2:buck2_client-bin",
    tpx = "//buck2/buck2_tpx_cli:buck2_tpx_cli",
    visibility = ["PUBLIC"],
)

# For backcompat with bash aliases and so forth
alias(
    name = "symlinked_buck2_and_tpx",
    actual = ":buck2_bundle",
)
