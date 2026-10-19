# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:command_alias.bzl", "CommandAliasOutput", "command_alias")
load("@prelude//:prelude_rule.bzl", "prelude_rule")
load("@prelude//decls:common.bzl", "buck")
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused: Used as a type
load("@prelude//zip_file:zip_file_toolchain.bzl", "ZipFileToolchainInfo")
load(":nodejs_package.bzl", "get_bundled_node", "package_standalone")
load(
    ":nodejs_providers.bzl",
    "build_node_modules",
    "check_relative_path",
    "get_nodejs_dep_infos",
    "get_nodejs_library_info",
    "get_nodejs_tree_info",
    "get_transitive_outputs",
)
load(":nodejs_toolchain.bzl", "NodejsToolchainInfo")

NodejsEntry = record(
    # The file's short path, or the `library`'s `entry_point`.
    name = field(str),
    path = field(Artifact | cmd_args),
)

def stage_nodejs_entries(
    ctx: AnalysisContext, mains: list[Artifact], mains_attr: str = "`main`", staged_attrs: str = "`main`/`srcs`", bundled_node: (str, Artifact) | None = None
) -> struct:
    """Stages the files a target runs beside the `node_modules` merged from `deps` and `library`.

    `mains` are the files to run without `library`; `mains_attr` names them in errors, and
    `staged_attrs` names them together with `srcs` in staging conflicts. Returns:
    - `runtime`: the staged directory, or `None` when `mains` run in place.
    - `entries`: a `NodejsEntry` per main, or for the `library`'s `entry_point`.
    - `hidden`: the inputs of an in-place run.
    - `env`: the environment to run with.
    """
    tree_infos = [get_nodejs_tree_info(dep, consumer_label = ctx.label) for dep in ctx.attrs.node_modules]
    lib_info = None
    if ctx.attrs.library != None:
        if mains:
            fail("{}: {} cannot be combined with `library`; use `entry_point`".format(ctx.label, mains_attr))
        if ctx.attrs.srcs:
            fail("{}: `srcs` cannot be combined with `library`".format(ctx.label))
        if ctx.attrs.entry_point == None:
            fail("{}: `library` requires `entry_point`".format(ctx.label))
        check_relative_path(ctx.attrs.entry_point, "{}: invalid `entry_point`".format(ctx.label))
        lib_info = get_nodejs_library_info(ctx.attrs.library, consumer_label = ctx.label)
    else:
        if ctx.attrs.entry_point != None:
            fail("{}: `entry_point` requires `library`".format(ctx.label))
        if not mains:
            fail("{}: {} is required without `library`".format(ctx.label, mains_attr))
    mains_by_path = {}
    for main in mains:
        existing = mains_by_path.get(main.short_path)
        if existing != None and existing != main:
            fail("{}: {} entries '{}' and '{}' both have the short path '{}'".format(ctx.label, mains_attr, existing, main, main.short_path))
        mains_by_path[main.short_path] = main
    mains = mains_by_path.values()
    dep_infos = ([lib_info] if lib_info != None else []) + get_nodejs_dep_infos(ctx.attrs.deps, consumer_label = ctx.label)
    runtime = None
    if dep_infos or bundled_node:
        staged = {}
        if lib_info == None:
            staged.update(mains_by_path)
            for src in ctx.attrs.srcs:
                existing = staged.get(src.short_path)
                if existing != None and existing != src:
                    fail("{}: {} entries '{}' and '{}' both stage to '{}'".format(ctx.label, staged_attrs, existing, src, src.short_path))
                staged[src.short_path] = src
        name = "_node_modules_"
        if bundled_node:
            node_name, node_dist = bundled_node
            if node_name in staged:
                fail(
                    "{}: {} entry '{}' and the bundled Node binary '{}' both stage to '{}'".format(
                        ctx.label, staged_attrs, staged[node_name], node_dist, node_name
                    )
                )
            staged[node_name] = node_dist
            for i, tree_info in enumerate(tree_infos):
                dest = "node_modules" if i == 0 and not dep_infos else "node_modules_tree_{}".format(i)
                for staged_path, existing in staged.items():
                    if staged_path.lower() == dest or staged_path.lower().startswith(dest + "/"):
                        fail(
                            "{}: package entry '{}' at '{}' collides with the `node_modules` tree '{}' staged at '{}'".format(
                                ctx.label, existing, staged_path, tree_info.node_modules, dest
                            )
                        )
                staged[dest] = tree_info.node_modules
            name = "__standalone__/" + ctx.label.name
        runtime = build_node_modules(
            ctx.actions,
            name,
            get_transitive_outputs(ctx.actions, deps = dep_infos),
            root_files = staged,
            for_runtime = True,
        )

    hidden = []
    if lib_info != None:
        entries = [
            NodejsEntry(
                name = ctx.attrs.entry_point,
                path = cmd_args(runtime, format = "{}/node_modules/" + lib_info.dir_name + "/" + ctx.attrs.entry_point),
            )
        ]
    elif runtime != None:
        entries = [NodejsEntry(name = main.short_path, path = cmd_args(runtime, format = "{}/" + main.short_path)) for main in mains]
    else:
        entries = [NodejsEntry(name = main.short_path, path = main) for main in mains]
        hidden = ctx.attrs.srcs

    is_windows = ctx.attrs._target_os_type[OsLookup].os == Os("windows")
    env = dict(ctx.attrs.env)
    node_path_entries = []
    if dep_infos:
        node_path_entries.append(cmd_args(runtime, format = "{}/node_modules"))
    for tree_info in tree_infos:
        node_path_entries.append(tree_info.node_modules)
    if node_path_entries:
        delimiter = ";" if is_windows else ":"
        parts = [env["NODE_PATH"]] + node_path_entries if "NODE_PATH" in env else node_path_entries
        env["NODE_PATH"] = cmd_args(parts, delimiter = delimiter)

    return struct(runtime = runtime, entries = entries, hidden = hidden, env = env)

def nodejs_command(ctx: AnalysisContext, args: cmd_args, env: dict[str, ArgLike]) -> CommandAliasOutput:
    """Runs the toolchain's Node with `args` and `env`, through a trampoline script for the target platform."""
    return command_alias(
        actions = ctx.actions,
        path = None,
        target_os = ctx.attrs._target_os_type[OsLookup],
        base = ctx.attrs._nodejs_toolchain[NodejsToolchainInfo].node,
        args = args,
        env = env,
        labels = ctx.attrs.labels,
    )

def build_nodejs_command(ctx: AnalysisContext, bundled_node: (str, Artifact) | None = None) -> struct:
    """Runs `main`, or the `library`'s `entry_point`, with `node_args` and `args`."""
    staged = stage_nodejs_entries(ctx, [ctx.attrs.main] if ctx.attrs.main != None else [], bundled_node = bundled_node)
    args = cmd_args(ctx.attrs.node_args, staged.entries[0].path, ctx.attrs.args, hidden = staged.hidden)
    return struct(merged = staged.runtime, output = nodejs_command(ctx, args, staged.env), env = staged.env)

def nodejs_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    bundled_node = None
    if ctx.attrs.package_style == "standalone":
        bundled_node = get_bundled_node(ctx.label, ctx.attrs._nodejs_toolchain[NodejsToolchainInfo].node_dist, ctx.attrs._target_os_type)
    command = build_nodejs_command(ctx, bundled_node)
    default_output = command.merged
    if bundled_node:
        default_output = package_standalone(
            ctx.actions,
            ctx.attrs._zip_file_toolchain[ZipFileToolchainInfo].create_zip,
            command.merged,
            ctx.label.name,
        )
    return [
        DefaultInfo(default_output = default_output, other_outputs = command.output.output.other_outputs),
        RunInfo(args = command.output.cmd),
    ]

nodejs_binary = prelude_rule(
    name = "nodejs_binary",
    impl = nodejs_binary_impl,
    docs = """A runnable Node.js script, executed with the toolchain's
    Node runtime and no custom loaders. When `deps` is non-empty, builds
    a merged `node_modules` from transitive deps, stages `main` and
    `srcs` beside it (each at its short path; distinct files staging to
    the same path fail), and runs the staged `main` so Node's own
    resolution finds the deps. The default output is that directory.
    With no `deps` and the `inplace` package style, `main` runs at its
    original path, files it imports via relative paths must be listed in
    `srcs` to be declared as build inputs, and there is no default
    output. Deps must be loadable as regular `node_modules` packages:
    each package's `package.json` governs whether it is CommonJS or an
    ES module, and TypeScript must be compiled to JavaScript before it
    is packaged. The merged tree is also exposed via `NODE_PATH` at
    runtime, appended to any `NODE_PATH` set in `env`, for CommonJS
    resolution. Transitive deps' package directories must be unique,
    as for `nodejs_library`. `package_style` is `"inplace"` (the
    default) or `"standalone"`. `standalone` always stages, adds the
    toolchain's `node_dist` binary to the staged directory as `node`
    (`node.exe` on Windows), and replaces the default output with a zip of that
    directory under a top-level directory named after the target (which
    therefore cannot contain `/`), for hosts without Node. The package runs
    as `<name>/node <name>/<main>`;
    `args`, `node_args`, `env`, `NODE_PATH` and any flags or env the
    toolchain's `node` wrapper adds are not applied there.
    With `library`, the entry is `<name>/node_modules/<package_name>/<entry_point>`.
    The tree at index `<i>` of the `node_modules` attribute is staged as
    `node_modules_tree_<i>`, which whoever runs the package must put on
    `NODE_PATH`; with no merged tree, the first is staged as `node_modules`
    instead.
    `library`, when set, runs a file from inside a `nodejs_library`'s staged
    package instead of `main`: `entry_point` names the file relative to the
    package root (e.g. `"dist/main.js"`; a relative `/`-separated path with
    no `:` and no `.` or `..` segments), and the library's transitive closure
    (plus `deps`) forms the `node_modules`. `main`/`srcs` cannot be combined
    with `library`; `entry_point` is required with it, and required to be
    absent without it. `node_modules`, when set, appends prebuilt trees
    (`nodejs_node_modules` targets) to `NODE_PATH` alongside any merged
    `deps` tree; they are used as installed, so unlike the merged tree
    they keep their `.d.ts` files.
    On non-Windows executors, the merged tree stages each `nodejs_library`
    package without its `.d.ts`/`.d.mts`/`.d.cts` files and their maps,
    because Node's type stripping refuses declarations under `node_modules`;
    the stripped copy is built once per library and shared by every consumer.
    On Windows executors the merged tree keeps them.""",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "main": attrs.option(attrs.source(), default = None),
            "srcs": attrs.list(attrs.source(), default = []),
            "args": attrs.list(attrs.arg(), default = []),
            "node_args": attrs.list(attrs.string(), default = []),
            "env": attrs.dict(attrs.string(), attrs.arg(), default = {}),
            "deps": attrs.list(attrs.dep(), default = []),
            "library": attrs.option(attrs.dep(), default = None),
            "entry_point": attrs.option(attrs.string(), default = None),
            "node_modules": attrs.list(attrs.dep(), default = []),
            "package_style": attrs.enum(["inplace", "standalone"], default = "inplace"),
            "_nodejs_toolchain": toolchains_common.nodejs(),
            "_target_os_type": buck.target_os_type_arg(),
            "_zip_file_toolchain": toolchains_common.zip_file(),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
    ),
)
