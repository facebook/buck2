load("@fbcode_macros//build_defs:export_files.bzl", "export_file")
load("@fbcode_macros//build_defs:native_rules.bzl", "alias")
load("@fbsource//tools/build_defs/buck2:is_buck2.bzl", "is_buck2")
load(":defs.bzl?v2_only", "symlinked_buck2_and_tpx")

oncall("buck2")

# @oss-disable: _INCLUDE_EXECUTABLES = True 
_INCLUDE_EXECUTABLES = False # @oss-enable

export_file(
    name = ".buck2",
) if _INCLUDE_EXECUTABLES else None

alias(
    name = "buck2",
    actual = "//buck2/app/buck2:buck2-bin",
)

# buildifier: disable=no-effect
symlinked_buck2_and_tpx(
    name = "symlinked_buck2_and_tpx",
    buck2 = "//buck2:buck2",
    tpx = "//buck2/buck2_tpx_cli:buck2_tpx_cli",
) if is_buck2() else None
