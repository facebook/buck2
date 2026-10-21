# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":nodejs_providers.bzl", "build_node_modules", "get_transitive_outputs")

# Output paths below are short and leave out the target name, which buck-out already
# includes twice: `tsc` on Windows stops resolving modules once a path passes MAX_PATH.

_TSC_EXTENSIONS = (".ts", ".tsx", ".mts", ".cts", ".js", ".jsx", ".mjs", ".cjs", ".json")
_DECLARATION_EXTENSIONS = (".d.ts", ".d.mts", ".d.cts")

def compile_typescript(
    actions: AnalysisActions,
    label: Label,
    tsc: RunInfo | None,
    srcs: list[Artifact],
    src_root: str,
    tsconfig: Artifact,
    package_json: Artifact,
    dep_infos: list,
    identifier: str,
    package_tree_tool: RunInfo,
    node_modules_tree = None,
) -> Artifact:
    """Compile `srcs` with `tsc` against `dep_infos` and `node_modules_tree`, returning the emitted tree.

    JavaScript and JSON sources go to `tsc` like TypeScript ones; the `tsconfig`'s `allowJs` and
    `resolveJsonModule` decide what it does with them.
    Emitted paths are source paths relative to `src_root`, which every source must be under.
    `package_json` is staged beside the sources so its `"type"` decides the emitted module format.
    """
    if tsc == None:
        fail("nodejs_library {}: typescript compilation requires `tsc` on the nodejs toolchain".format(label))
    if not srcs:
        fail("nodejs_library {}: typescript compilation requires at least one source in `srcs`".format(label))
    for src in srcs:
        if not src.short_path.endswith(_TSC_EXTENSIONS):
            fail(
                "nodejs_library {}: cannot compile '{}': `tsc` takes only `.ts`, `.tsx`, `.mts`, `.cts`, `.js`, `.jsx`, `.mjs`, `.cjs` and `.json` sources; use `resources` for other files".format(
                    label, src.short_path
                )
            )
        if src.short_path.endswith(_DECLARATION_EXTENSIONS):
            fail("nodejs_library {}: cannot compile declaration file '{}': `tsc` emits nothing for it".format(label, src.short_path))

    reserved = ["tsconfig.json", "tsconfig.buck.json", "package.json"]
    if dep_infos or node_modules_tree != None:
        reserved.append("node_modules")

    stage = {}
    for src in srcs:
        key = src.short_path
        root = key.split("/")[0]
        if root in reserved:
            fail("nodejs_library {}: source '{}' collides with the staged `{}`".format(label, key, root))
        existing = stage.get(key)
        if existing != None and existing != src:
            fail("nodejs_library {}: duplicate sources '{}' and '{}' both stage to '{}'".format(label, existing, src, key))
        stage[key] = src
    stage["tsconfig.buck.json"] = actions.write_json(
        "ts/c.json",
        {
            "compilerOptions": {"declarationDir": None},
            "extends": "./tsconfig.json",
            "files": sorted(stage.keys()),
            "include": [],
        },
    )
    stage["tsconfig.json"] = tsconfig
    stage["package.json"] = package_json
    if dep_infos or node_modules_tree != None:
        if dep_infos:
            dep_tree = build_node_modules(
                actions,
                "ts/d",
                get_transitive_outputs(actions, deps = dep_infos),
            )
            dep_node_modules = dep_tree.project("node_modules")
            if node_modules_tree != None:
                dep_node_modules = merge_node_modules(actions, package_tree_tool, dep_node_modules, node_modules_tree, identifier)
            stage["node_modules"] = dep_node_modules
        else:
            # Copy under a real `node_modules` segment (see `merge_node_modules`).
            stage["node_modules"] = actions.copied_dir(
                "ts/n",
                {"node_modules": node_modules_tree},
            ).project("node_modules")
    src_dir = actions.symlinked_dir("ts/s", stage)

    out_dir = actions.declare_output("ts/o", dir = True)

    cmd = cmd_args(
        tsc,
        "-p",
        cmd_args(src_dir, format = "{}/tsconfig.buck.json"),
        "--outDir",
        out_dir.as_output(),
        "--rootDir",
        cmd_args(src_dir, format = "{}/" + src_root) if src_root else src_dir,
        "--declaration",
        "--noEmit",
        "false",
        "--emitDeclarationOnly",
        "false",
    )

    actions.run(
        cmd,
        category = "tsc_compile",
        identifier = identifier,
    )

    return out_dir

def merge_node_modules(actions: AnalysisActions, package_tree_tool: RunInfo, dep_node_modules: Artifact, prebuilt_tree: Artifact, identifier: str) -> Artifact:
    """Merge a deps-built `node_modules` tree with a prebuilt tree.

    A package (or scoped package) present in both trees fails the action.
    On Windows executors symlinks are copied as their targets.
    The result sits under a real `node_modules` path segment: resolvers realpath-resolve
    into the tree and then walk ancestor `node_modules` directories, which
    fails when the tree root itself is mounted as `node_modules`.
    """
    merged = actions.declare_output("ts/m/node_modules", dir = True)
    actions.run(
        cmd_args(
            package_tree_tool,
            "merge",
            "--deps",
            dep_node_modules,
            "--prebuilt",
            prebuilt_tree,
            "--out",
            merged.as_output(),
        ),
        category = "tsc_merge_node_modules",
        identifier = identifier,
    )
    return merged

def compile_module_typescript(actions: AnalysisActions, label: Label, tsc: RunInfo | None, module_dir: Artifact, src_root: str, identifier: str) -> Artifact:
    """Compile a whole project directory with the toolchain's `tsc`, returning the emitted tree.

    Emitted paths mirror source paths from `src_root` inside the module, so with
    an empty `src_root`, `src/main.ts` emits `src/main.js` even when the
    `tsconfig` sets `rootDir: "src"`. The module tsconfig must not require
    writes into the tree (no incremental or composite state).
    """
    if tsc == None:
        fail("nodejs_library {}: typescript compilation requires `tsc` on the nodejs toolchain".format(label))

    out_dir = actions.declare_output("ts/mo", dir = True)

    cmd = cmd_args(
        tsc,
        "-p",
        cmd_args(module_dir, format = "{}/tsconfig.json"),
        "--outDir",
        out_dir.as_output(),
        "--rootDir",
        cmd_args(module_dir, format = "{}/" + src_root) if src_root else module_dir,
        "--declaration",
        "--declarationDir",
        out_dir.as_output(),
        "--noEmit",
        "false",
        "--emitDeclarationOnly",
        "false",
    )

    actions.run(
        cmd,
        category = "tsc_module_compile",
        identifier = identifier,
    )

    return out_dir
