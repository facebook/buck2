# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//python:manifest.bzl", "create_manifest_for_entries")
load("@prelude//unix:providers.bzl", "UnixEnv", "create_unix_env_info")

def _export_exe_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs.src and ctx.attrs.exe:
        fail("Must supply one of src or exe to export_exe")

    src = ctx.attrs.src if ctx.attrs.src else ctx.attrs.exe

    providers = []
    providers.append(DefaultInfo(other_outputs = ctx.attrs.resources))
    providers.append(RunInfo(args = cmd_args(src, hidden = ctx.attrs.resources)))

    if ctx.attrs.src != None:
        providers.append(
            create_unix_env_info(
                actions = ctx.actions,
                env = UnixEnv(
                    label = ctx.label,
                    binaries = [
                        create_manifest_for_entries(
                            ctx = ctx,
                            name = "unix_env",
                            entries = [
                                (ctx.attrs.src.basename, ctx.attrs.src, ""),
                            ],
                        ),
                    ],
                ),
            ),
        )

    return providers

_export_exe = rule(
    doc = """Exports a file as an executable, for use in $(exe) macros or as a valid target for an exec_dep().
    Accepts either a string `src`, which is a relative path to a file that will be directly referenced,
    or an arg `exe` which should be a path to an executable relative to a $(location) macro.

    The first form is a more ergonomic replacement for export_file + command_alias. Eg. Instead of

    export_file(
        name = "script_sh",
        src = "bin/script.sh",
    )

    command_alias(
        name = "script.sh"
        exe = ":script_sh",
    )

    You can write

    export_exe(
        name = "script.sh",
        src = "bin/script.sh",
    )

    The latter form allows executing checked in binaries with required resources (eg. runtime shared libraries)
    without unnecessary indirection via another rule which allows args, like command_alias. Eg. instead of

    export_file(
        name = "bin"
        src = "bin",
        mode = "reference",
    )

    export_file(
        name = "exec.sh",
        src = "exec.sh",
    )

    command_alias(
        name = "compiler",
        exe = ":exec.sh", # Just calls exec $@
        args = [
            "$(location :bin)/compiler",
        ],
    )

    You can write

    export_file(
        name = "bin",
        src = "bin",
        mode = "reference",
    )

    export_exe(
        name = "compiler",
        exe = "$(location :bin)/compiler",
    )

    To ensure additional resources are materialized in Remote Execution (e.g., internal
    compiler components that the executable invokes), use the `resources` parameter:

    export_file(
        name = "bin",
        src = "bin",
        mode = "reference",
    )

    export_file(
        name = "libexec",
        src = "libexec",
        mode = "reference",
    )

    export_exe(
        name = "compiler",
        exe = "$(location :bin)/compiler",
        resources = [":bin", ":libexec"],
    )
    """,
    impl = _export_exe_impl,
    attrs = {
        "exe": attrs.option(attrs.arg(), default = None, doc = "arg which should evaluate to a path to an executable binary"),
        "resources": attrs.list(attrs.source(), default = [], doc = "Additional artifacts to materialize alongside the executable (for Remote Execution)"),
        "src": attrs.option(attrs.source(), default = None, doc = "path to an executable binary relative to this package"),
    },
)

def export_exe(name, exe = None, src = None, **kwargs):
    # If neither `exe` nor `src` is passed, treat the target's name as the src.
    #
    #     export_exe(
    #         name = "script.sh",
    #     )
    #
    # is equivalent to:
    #
    #     export_exe(
    #         name = "script.sh",
    #         src = "script.sh",
    #     )
    #
    _export_exe(
        name = name,
        exe = exe,
        src = src if (exe or src) else name,
        **kwargs
    )
