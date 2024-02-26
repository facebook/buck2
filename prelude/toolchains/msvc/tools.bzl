# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:cmd_script.bzl", "ScriptOs", "cmd_script")

VisualStudio = provider(
    # @unsorted-dict-items
    fields = {
        # cl.exe
        "cl_exe": provider_field(typing.Any, default = None),
        # cvtres.exe
        "cvtres_exe": provider_field(typing.Any, default = None),
        # lib.exe
        "lib_exe": provider_field(typing.Any, default = None),
        # ml64.exe
        "ml64_exe": provider_field(typing.Any, default = None),
        # link.exe
        "link_exe": provider_field(typing.Any, default = None),
        # rc.exe
        "rc_exe": provider_field(typing.Any, default = None),
    },
)

def _find_msvc_tools_impl(ctx: AnalysisContext) -> list[Provider]:
    cl_exe_json = ctx.actions.declare_output("cl.exe.json")
    cvtres_exe_json = ctx.actions.declare_output("cvtres.exe.json")
    lib_exe_json = ctx.actions.declare_output("lib.exe.json")
    ml64_exe_json = ctx.actions.declare_output("ml64.exe.json")
    link_exe_json = ctx.actions.declare_output("link.exe.json")
    rc_exe_json = ctx.actions.declare_output("rc.exe.json")

    cmd = [
        ctx.attrs.vswhere[RunInfo],
        cmd_args("--cl=", cl_exe_json.as_output(), delimiter = ""),
        cmd_args("--cvtres=", cvtres_exe_json.as_output(), delimiter = ""),
        cmd_args("--lib=", lib_exe_json.as_output(), delimiter = ""),
        cmd_args("--ml64=", ml64_exe_json.as_output(), delimiter = ""),
        cmd_args("--link=", link_exe_json.as_output(), delimiter = ""),
        cmd_args("--rc=", rc_exe_json.as_output(), delimiter = ""),
    ]

    ctx.actions.run(
        cmd,
        category = "vswhere",
        local_only = True,
    )

    run_msvc_tool = ctx.attrs.run_msvc_tool[RunInfo]
    cl_exe_script = cmd_script(
        ctx = ctx,
        name = "cl",
        cmd = cmd_args(run_msvc_tool, cl_exe_json),
        os = ScriptOs("windows"),
    )
    cvtres_exe_script = cmd_script(
        ctx = ctx,
        name = "cvtres",
        cmd = cmd_args(run_msvc_tool, cvtres_exe_json),
        os = ScriptOs("windows"),
    )
    lib_exe_script = cmd_script(
        ctx = ctx,
        name = "lib",
        cmd = cmd_args(run_msvc_tool, lib_exe_json),
        os = ScriptOs("windows"),
    )
    ml64_exe_script = cmd_script(
        ctx = ctx,
        name = "ml64",
        cmd = cmd_args(run_msvc_tool, ml64_exe_json),
        os = ScriptOs("windows"),
    )
    link_exe_script = cmd_script(
        ctx = ctx,
        name = "link",
        cmd = cmd_args(run_msvc_tool, link_exe_json),
        os = ScriptOs("windows"),
    )
    rc_exe_script = cmd_script(
        ctx = ctx,
        name = "rc",
        cmd = cmd_args(run_msvc_tool, rc_exe_json),
        os = ScriptOs("windows"),
    )

    return [
        # Supports `buck2 run prelude//toolchains/msvc:msvc_tools[cl.exe]`
        # and `buck2 build prelude//toolchains/msvc:msvc_tools[cl.exe][json]`
        DefaultInfo(sub_targets = {
            "cl.exe": [
                RunInfo(args = [cl_exe_script]),
                DefaultInfo(sub_targets = {
                    "json": [DefaultInfo(default_output = cl_exe_json)],
                }),
            ],
            "cvtres.exe": [
                RunInfo(args = [cvtres_exe_script]),
                DefaultInfo(sub_targets = {
                    "json": [DefaultInfo(default_output = cvtres_exe_json)],
                }),
            ],
            "lib.exe": [
                RunInfo(args = [lib_exe_script]),
                DefaultInfo(sub_targets = {
                    "json": [DefaultInfo(default_output = lib_exe_json)],
                }),
            ],
            "link.exe": [
                RunInfo(args = [link_exe_script]),
                DefaultInfo(sub_targets = {
                    "json": [DefaultInfo(default_output = link_exe_json)],
                }),
            ],
            "ml64.exe": [
                RunInfo(args = [ml64_exe_script]),
                DefaultInfo(sub_targets = {
                    "json": [DefaultInfo(default_output = ml64_exe_json)],
                }),
            ],
            "rc.exe": [
                RunInfo(args = [rc_exe_script]),
                DefaultInfo(sub_targets = {
                    "json": [DefaultInfo(default_output = rc_exe_json)],
                }),
            ],
        }),
        VisualStudio(
            cl_exe = cl_exe_script,
            cvtres_exe = cvtres_exe_script,
            lib_exe = lib_exe_script,
            ml64_exe = ml64_exe_script,
            link_exe = link_exe_script,
            rc_exe = rc_exe_script,
        ),
    ]

find_msvc_tools = rule(
    impl = _find_msvc_tools_impl,
    attrs = {
        "run_msvc_tool": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//toolchains/msvc:run_msvc_tool")),
        "vswhere": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//toolchains/msvc:vswhere")),
    },
)
