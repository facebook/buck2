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

def _run(ctx, name, tool, exec_group = None, extra_inputs = []):
    out = ctx.actions.declare_output(name + ".txt")
    ctx.actions.run(
        cmd_args(tool[RunInfo], out.as_output(), hidden = extra_inputs),
        category = name,
        exec_group = exec_group,
    )
    return out

# Resources and packaging use toolchains that need different platforms; neither constrains
# the default execution group.
def _bundle_impl(ctx):
    resources = _run(ctx, "compile_resources", ctx.attrs._resource_toolchain, Resources)
    bundle = _run(ctx, "package", ctx.attrs._package_toolchain, Package, [resources])
    return [DefaultInfo(default_output = bundle)]

bundle = rule(
    impl = _bundle_impl,
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
def _binary_impl(ctx):
    objects = _run(ctx, "compile", ctx.attrs._compiler)
    explicit_default = _run(ctx, "explicit_default", ctx.attrs._any_tool, DefaultExecGroup)
    link_args = ctx.actions.write("link_args.txt", [objects], exec_group = Link)
    linked = _run(ctx, "link", ctx.attrs._linker, Link, [objects, link_args])
    unused = _run(ctx, "unused_group", ctx.attrs._any_tool_link, Unused)
    return [DefaultInfo(default_outputs = [linked, explicit_default, unused])]

binary = rule(
    impl = _binary_impl,
    attrs = {
        "_any_tool": attrs.exec_dep(default = "//groups:any_tool"),
        "_any_tool_link": attrs.exec_dep(default = "//groups:any_tool", exec_group = Link),
        "_compiler": attrs.exec_dep(default = "//groups:remote_tool"),
        "_linker": attrs.exec_dep(default = "//groups:any_tool", exec_group = Link),
        "_local_marker": attrs.exec_dep(default = "//groups:local_tool", exec_group = Link),
    },
    exec_groups = [Link, Unused],
)

# Runs `dynamic_compile` in the default group and `dynamic_link` in `Link` from a dynamic output.
def _dynamic_binary_impl(ctx):
    dynamic_input = ctx.actions.write("dynamic_input.txt", "")
    compiled = ctx.actions.declare_output("dynamic_compile.txt")
    linked = ctx.actions.declare_output("dynamic_link.txt")
    compiler = ctx.attrs._compiler[RunInfo]
    linker = ctx.attrs._linker[RunInfo]

    def f(ctx, _artifacts, outputs):
        ctx.actions.run(cmd_args(compiler, outputs[compiled].as_output()), category = "dynamic_compile")
        ctx.actions.run(
            cmd_args(linker, outputs[linked].as_output()),
            category = "dynamic_link",
            exec_group = Link,
        )

    ctx.actions.dynamic_output(
        dynamic = [dynamic_input],
        inputs = [],
        outputs = [compiled.as_output(), linked.as_output()],
        f = f,
    )
    return [DefaultInfo(default_outputs = [compiled, linked])]

dynamic_binary = rule(
    impl = _dynamic_binary_impl,
    attrs = {
        "_compiler": attrs.exec_dep(default = "//groups:remote_tool"),
        "_linker": attrs.exec_dep(default = "//groups:any_tool", exec_group = Link),
        "_local_marker": attrs.exec_dep(default = "//groups:local_tool", exec_group = Link),
    },
    exec_groups = [Link],
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

# The target, not the rule, says what the `Link` platform must satisfy.
constrained_link = rule(
    impl = _noop_impl,
    attrs = {
        "link_constraints": attrs.exec_compatible_with(exec_group = Link, default = []),
        "_compiler": attrs.exec_dep(default = "//groups:remote_tool"),
    },
    exec_groups = [Link],
)

# `default_constraints` constrains only the default group; `Link` resolves on its own.
default_constrained = rule(
    impl = _noop_impl,
    attrs = {
        "default_constraints": attrs.exec_compatible_with(exec_group = DefaultExecGroup, default = []),
        "_linker": attrs.exec_dep(default = "//groups:any_tool", exec_group = Link),
    },
    exec_groups = [DefaultExecGroup, Link],
)

# A rule without named groups constrains its only group, the default, the same way.
default_constrained_no_groups = rule(
    impl = _noop_impl,
    attrs = {
        "default_constraints": attrs.exec_compatible_with(exec_group = DefaultExecGroup, default = []),
    },
)

# `exec_group = None` is the default group, so wrappers can pass an optional group through.
plain = rule(impl = _noop_impl, attrs = {"_tool": attrs.exec_dep(default = "//groups:any_tool", exec_group = None)})
