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
    buck2_health_check = "//buck2/buck2_health_check_cli:buck2_health_check_cli",
    tpx = "//buck2/buck2_tpx_cli:buck2_tpx_cli",
    visibility = ["PUBLIC"],
)

# For backcompat with bash aliases and so forth
# You can use this target to test custom builds of buck2.
#
# Step 1: `buck2 build @fbcode//mode/opt fbcode//buck2:symlinked_buck2_and_tpx --out ~/buck2`
# Step 2: Use the buck2 binary from `~/buck2/buck2`
#
# If you're testing on macOS, use `@fbcode//mode/opt-mac-arm64`
alias(
    name = "symlinked_buck2_and_tpx",
    actual = ":buck2_bundle",
)
