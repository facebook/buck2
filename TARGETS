load("@fbcode_macros//build_defs:export_files.bzl", "export_file")
load("@fbcode_macros//build_defs:native_rules.bzl", "alias")

oncall("buck2")

# @oss-disable: _INCLUDE_EXECUTABLES = True 
_INCLUDE_EXECUTABLES = False # @oss-enable

export_file(
    name = ".buck2-tpx",
) if _INCLUDE_EXECUTABLES else None

export_file(
    name = ".buck2",
) if _INCLUDE_EXECUTABLES else None

alias(
    name = "buck2",
    actual = "//buck2/cli:buck2",
)
