load("@prelude//:none.bzl", "none_rule")
load("@prelude//utils:source_listing.bzl", "source_listing")
load(":native.bzl", prelude = "native")

oncall("build_infra")

source_listing(exclude = [
    # Exclude PACKAGE file using modifiers since those are not enabled everywhere yet.
    "PACKAGE",
])

# Done to avoid triggering a lint rule that replaces glob with an fbcode macro
globby = glob

srcs = globby(
    ["**"],
    # Context: https://fb.workplace.com/groups/buck2users/posts/3121903854732641/
    exclude = [
        "**/.pyre_configuration.local",
        # Unfortunately, using modifiers require loading bzl files in outside of prelude,
        # and that currently breaks isolated tests that attempt to grab a best-effort prelude
        # from the filegroup below.
        # TODO: Switch these tests to use the bundled prelude instead.
        "PACKAGE",
    ],
)

# Re-export filegroups that are behind package boundary violations for
# Buck2.
prelude.filegroup(
    name = "files",
    srcs = srcs,
    visibility = ["PUBLIC"],
)

# Tests want BUCK.v2 instead of TARGETS.v2
prelude.genrule(
    name = "copy_android_constraint",
    out = "BUCK",
    cmd = "cp $(location prelude//android/constraints:files)/BUCK $OUT",
    visibility = ["PUBLIC"],
)

prelude.filegroup(
    name = "prelude",
    srcs = {
        "": ":files",
        "android/constraints/BUCK": ":copy_android_constraint",
        "cfg/exec_platform/marker/BUCK": "//cfg/exec_platform/marker:buck_file",
        "rust/tools/attrs.bzl": "//rust/tools:attrs.bzl",
    },
    visibility = ["PUBLIC"],
)

none_rule(
    name = "none",
    visibility = ["PUBLIC"],
)
