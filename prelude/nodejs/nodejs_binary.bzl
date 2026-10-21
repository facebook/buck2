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
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused: Used as a type
load("@prelude//zip_file:zip_file_toolchain.bzl", "ZipFileToolchainInfo")
load(":nodejs_closure.bzl", "compose_node_modules")
load(":nodejs_package.bzl", "get_bundled_node", "package_standalone")
load(
    ":nodejs_providers.bzl",
    "NodejsLibraryInfo",
    "NodejsTreeInfo",
    "check_relative_path",
    "collect_packages",
    "get_nodejs_dep_infos",
    "get_nodejs_library_info",
    "get_nodejs_tree_info",
    "get_transitive_outputs",
    "get_transitive_trees",
    "get_trees",
)
load(":nodejs_toolchain.bzl", "NodejsToolchainInfo")

NodejsEntry = record(
    # The file's short path, or the `library`'s entry.
    name = field(str),
    path = field(Artifact | cmd_args),
)

def _library_entry(label: Label, entry_point: str | None, lib_info: NodejsLibraryInfo) -> str:
    if entry_point != None:
        check_relative_path(entry_point, "{}: invalid `entry_point`".format(label))
        return entry_point
    if len(lib_info.bins) == 1:
        return lib_info.bins.values()[0]
    if lib_info.main != None:
        return lib_info.main
    fail("{}: `library` requires `entry_point` when the library sets neither a single `bin` nor `main` attribute".format(label))

def _tree_infos(ctx: AnalysisContext, dep_infos: list[NodejsLibraryInfo]) -> list[NodejsTreeInfo]:
    if ctx.attrs.node_modules == None:
        return get_trees(get_transitive_trees(ctx.actions, deps = dep_infos))
    return [get_nodejs_tree_info(dep, consumer_label = ctx.label) for dep in ctx.attrs.node_modules]

def _manifest_name(name: str) -> str:
    chars = []
    for c in name.lower().elems():
        chars.append(c if c.isalnum() or c in "-._~" else "-")
    return "".join(chars).lstrip("._")[:214] or "app"

def stage_nodejs_entries(
    ctx: AnalysisContext,
    mains: list[Artifact],
    *,
    mains_attr: str = "`main`",
    staged_attrs: str = "`main`/`srcs`",
    srcs: list[Artifact] | None = None,
    stage: bool = False,
    bundled_node: (str, Artifact) | None = None,
) -> struct:
    """Stages the files a target runs beside a `node_modules` of `library`, `deps` and trees.

    `mains` are the files to run without `library`; `mains_attr` names them in errors, and
    `staged_attrs` names them together with `srcs` in staging conflicts. `srcs`
    (default: the `srcs` attribute) are staged with `mains`. The directory is always declared;
    `mains` run from it when there is a `library`, `deps`, a third-party tree, `bundled_node`
    or `stage`. Returns:
    - `dir`: the staged directory.
    - `runtime`: `dir`, or `None` when `mains` run in place.
    - `entries`: a `NodejsEntry` per main, or for the `library`'s entry.
    - `hidden`: the inputs of an in-place run.
    - `env`: the environment to run with.
    """
    if srcs == None:
        srcs = ctx.attrs.srcs
    lib_info = None
    entry_point = ""
    if ctx.attrs.library != None:
        if mains:
            fail("{}: {} cannot be combined with `library`; use `entry_point`".format(ctx.label, mains_attr))
        if srcs:
            fail("{}: `srcs` cannot be combined with `library`".format(ctx.label))
        lib_info = get_nodejs_library_info(ctx.attrs.library, consumer_label = ctx.label)
        entry_point = _library_entry(ctx.label, ctx.attrs.entry_point, lib_info)
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
    tree_infos = _tree_infos(ctx, dep_infos)
    staged = {}
    if lib_info == None:
        staged.update(mains_by_path)
        for src in srcs:
            existing = staged.get(src.short_path)
            if existing != None and existing != src:
                fail("{}: {} entries '{}' and '{}' both stage to '{}'".format(ctx.label, staged_attrs, existing, src, src.short_path))
            staged[src.short_path] = src
    name = "app"
    if bundled_node:
        node_name, node_dist = bundled_node
        if node_name in staged:
            fail(
                "{}: {} entry '{}' and the bundled Node binary '{}' both stage to '{}'".format(ctx.label, staged_attrs, staged[node_name], node_dist, node_name)
            )
        staged[node_name] = node_dist
        name = "__standalone__/" + ctx.label.name
    packages = collect_packages(get_transitive_outputs(ctx.actions, deps = dep_infos))
    if "package.json" not in staged:
        # Tools that find the project root beside `node_modules`, such as `module-alias/register`, read it.
        staged["package.json"] = ctx.actions.write_json("app.json", {"name": _manifest_name(ctx.label.name), "private": True, "version": "0.0.0"})
    staged_dir = compose_node_modules(
        ctx.actions,
        name,
        {n: p.runtime_dir for n, p in packages.items()},
        {n: p.label for n, p in packages.items()},
        tree_infos,
        root_files = staged,
        root_file_desc = "{}: {} entry '{{artifact}}' at '{{path}}'".format(ctx.label, staged_attrs),
    )
    runtime = staged_dir if dep_infos or tree_infos or bundled_node or stage else None

    hidden = []
    if lib_info != None:
        entries = [
            NodejsEntry(
                name = entry_point,
                path = cmd_args(runtime, format = "{}/node_modules/" + lib_info.dir_name + "/" + entry_point),
            ),
        ]
    elif runtime != None:
        entries = [NodejsEntry(name = main.short_path, path = cmd_args(runtime, format = "{}/" + main.short_path)) for main in mains]
    else:
        entries = [NodejsEntry(name = main.short_path, path = main) for main in mains]
        hidden = srcs

    return struct(dir = staged_dir, runtime = runtime, entries = entries, hidden = hidden, env = dict(ctx.attrs.env))

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
    """Runs `main`, or the `library`'s entry, with `node_args` and `args`."""
    staged = stage_nodejs_entries(ctx, [ctx.attrs.main] if ctx.attrs.main != None else [], bundled_node = bundled_node)
    args = cmd_args(ctx.attrs.node_args, staged.entries[0].path, ctx.attrs.args, hidden = staged.hidden)
    return struct(dir = staged.dir, merged = staged.runtime, output = nodejs_command(ctx, args, staged.env), env = staged.env)

def launcher_outputs(command: struct, default_output: Artifact | None = None) -> DefaultInfo:
    """The launcher, or `default_output` when set, as the default output, with the staged directory as `[dir]`."""
    return DefaultInfo(
        default_output = default_output or command.output.output.default_outputs[0],
        other_outputs = command.output.output.other_outputs,
        sub_targets = {"dir": [DefaultInfo(default_output = command.dir)]},
    )

def nodejs_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    bundled_node = None
    if ctx.attrs.package_style == "standalone":
        bundled_node = get_bundled_node(ctx.label, ctx.attrs._nodejs_toolchain[NodejsToolchainInfo].node_dist, ctx.attrs._target_os_type)
    command = build_nodejs_command(ctx, bundled_node)
    standalone = None
    if bundled_node:
        standalone = package_standalone(
            ctx.actions,
            ctx.attrs._zip_file_toolchain[ZipFileToolchainInfo].create_zip,
            command.merged,
            ctx.label.name,
        )
    default_info = launcher_outputs(command, standalone)
    run_args = command.output.maybe_directly_runnable
    return [
        default_info,
        RunInfo(args = run_args if run_args != None else command.output.cmd),
    ]

nodejs_binary = prelude_rule(
    name = "nodejs_binary",
    impl = nodejs_binary_impl,
    docs = """A runnable Node.js script, executed with the toolchain's
    Node runtime and no custom loaders. The `[dir]` sub-target is one
    directory holding a `node_modules` composed from the transitive
    `nodejs_library` packages and the packages of the third-party trees,
    `main` and `srcs` beside it at their short paths (distinct files
    staging to the same path fail, as does one under `node_modules` when
    any package is composed), and
    a `package.json` at its root: the one in `srcs`, or else
    `{"name": <target name>, "private": true, "version": "0.0.0"}`, for
    tools that look for the project root beside `node_modules` (the name
    is lowercased and cut to 214 characters, with characters npm rejects
    replaced by `-`). When
    `library`, `deps` or a third-party tree is involved, the staged `main`
    runs from `[dir]`, so Node's own resolution finds every package, for
    `import`, `require`, worker threads and child `node` processes alike.
    Otherwise, `main` runs at its original path and files it imports via
    relative paths must be listed in `srcs` to be declared as build
    inputs. `NODE_PATH` is never set; one set in `env` is passed through.
    The default output is the launcher: a `command_alias` trampoline for
    the target platform that runs the toolchain's `node` with `node_args`,
    the entry and `args`, and sets `env`; `$(location)` of the target runs
    it. `RunInfo` runs the same command, without the trampoline when `env`
    is empty.
    Deps must be loadable as regular `node_modules` packages: each
    package's `package.json` governs whether it is CommonJS or an ES
    module, and TypeScript must be compiled to JavaScript before it is
    packaged. Each `nodejs_library` package is staged without its
    `.d.ts`/`.d.mts`/`.d.cts` files and their maps (the library's
    `[runtime]`), because Node's type stripping refuses declarations under
    `node_modules`. Transitive deps' package directories must be unique,
    as for `nodejs_library`.
    `node_modules`, when unset, takes the third-party trees of `library`
    and `deps` (see `nodejs_library`'s `node_modules`); a list of
    `nodejs_node_modules` targets replaces them, and `[]` means none.
    Third-party packages are composed one by one as installed (they keep
    their `.d.ts` files; dot entries such as `.bin` are dropped): a
    package that two sources provide fails the build and names both, and
    `exclude` on a `nodejs_node_modules` drops packages.
    `library`, when set, runs a file from inside a `nodejs_library`'s
    package instead of `main` (a library with only `deps` or
    `node_modules` stages none and fails here): `entry_point` names the file relative to the package root
    (e.g. `"main.js"`; a relative `/`-separated path with no `:` and
    no `.` or `..` segments), and defaults to the library's `bin` attribute
    when it has exactly one, else its `main` attribute; entries set only in
    the library's `package_json` or `prebuilt` manifest are not read, so
    such a library needs `entry_point`. `main`/`srcs` cannot be combined
    with `library`, and `entry_point` requires it.
    `package_style` is `"inplace"` (the default) or `"standalone"`.
    `standalone` always stages, adds the toolchain's `node_dist` binary to
    the staged directory as `node` (`node.exe` on Windows), and replaces
    the default output with a zip of that directory under a top-level
    directory named after the target (which therefore cannot contain
    `/`), for hosts without Node. The package runs as
    `<name>/node <name>/<main>`, or with `library`, as
    `<name>/node <name>/node_modules/<dir>/<entry_point>`, where `<dir>`
    is the library's `out`, or its `package_name` when `out` is unset;
    `args`, `node_args`, `env` and any flags or env the toolchain's `node`
    wrapper adds are not applied there.""",
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
            "node_modules": attrs.option(attrs.list(attrs.dep()), default = None),
            "package_style": attrs.enum(["inplace", "standalone"], default = "inplace"),
            "_nodejs_toolchain": toolchains_common.nodejs(),
            "_target_os_type": buck.target_os_type_arg(),
            "_zip_file_toolchain": toolchains_common.zip_file(),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
    ),
)
