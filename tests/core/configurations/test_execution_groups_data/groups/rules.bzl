# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

Resources = exec_group()
Package = exec_group()
Link = exec_group()
Unused = exec_group()

ToolPlugin = plugins.kind()

def _tool_impl(ctx):
    script = ctx.actions.write(
        "tool.py",
        [
            "import sys",
            "with open(sys.argv[1], 'w') as f:",
            "    f.write('{}')".format(ctx.label.name),
        ],
    )
    return [DefaultInfo(default_output = script), RunInfo(args = cmd_args("python3", script))]

tool = rule(impl = _tool_impl, attrs = {})

def _toolchain_impl(ctx):
    return [DefaultInfo(), ctx.attrs.tool[RunInfo]]

toolchain = rule(
    impl = _toolchain_impl,
    attrs = {"tool": attrs.exec_dep(providers = [RunInfo])},
    is_toolchain_rule = True,
)

def _noop_impl(_ctx):
    return [DefaultInfo()]

# Resources and packaging use toolchains that need different platforms; neither constrains
# the default execution group.
bundle = rule(
    impl = _noop_impl,
    attrs = {
        "_package_toolchain": attrs.toolchain_dep(
            default = "//groups:package_toolchain",
            exec_group = Package,
        ),
        "_resource_toolchain": attrs.toolchain_dep(
            default = "//groups:resource_toolchain",
            exec_group = Resources,
        ),
    },
    exec_groups = [Resources, Package],
)

# Compiles remotely and links locally; `Unused` has no deps and still resolves.
binary = rule(
    impl = _noop_impl,
    attrs = {
        "_any_tool": attrs.exec_dep(default = "//groups:any_tool"),
        "_any_tool_link": attrs.exec_dep(default = "//groups:any_tool", exec_group = Link),
        "_compiler": attrs.exec_dep(default = "//groups:remote_tool"),
        "_linker": attrs.exec_dep(default = "//groups:any_tool", exec_group = Link),
        "_local_marker": attrs.exec_dep(default = "//groups:local_tool", exec_group = Link),
    },
    exec_groups = [Link, Unused],
)

# The same tool in the default group and in `Link`, both resolving to the same platform.
same_platform = rule(
    impl = _noop_impl,
    attrs = {
        "_tool": attrs.exec_dep(default = "//groups:any_tool"),
        "_tool_in_link": attrs.exec_dep(default = "//groups:any_tool", exec_group = Link),
    },
    exec_groups = [Link],
)

# The same toolchain in the default group and in `Link`, both resolving to the same platform.
same_platform_toolchain = rule(
    impl = _noop_impl,
    attrs = {
        "_toolchain": attrs.toolchain_dep(default = "//groups:any_toolchain"),
        "_toolchain_in_link": attrs.toolchain_dep(default = "//groups:any_toolchain", exec_group = Link),
    },
    exec_groups = [Link],
)

# Gets `local_tool` as an exec dep from a plugin rather than from an attribute value.
plugin_user = rule(
    impl = _noop_impl,
    attrs = {
        "_linker": attrs.exec_dep(default = "//groups:any_tool", exec_group = Link),
        "_plugin": attrs.plugin_dep(kind = ToolPlugin, default = "//groups:local_tool"),
    },
    exec_groups = [Link],
    uses_plugins = [ToolPlugin],
)

# `exec_group = None` is the default group, so wrappers can pass an optional group through.
plain = rule(impl = _noop_impl, attrs = {"_tool": attrs.exec_dep(default = "//groups:any_tool", exec_group = None)})
