# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

Link = exec_group()
Unused = exec_group()

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

def _noop_impl(_ctx):
    return [DefaultInfo()]

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
