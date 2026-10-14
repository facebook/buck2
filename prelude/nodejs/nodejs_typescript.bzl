# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":nodejs_providers.bzl", "build_node_modules", "get_transitive_outputs")

_TS_EXTENSIONS = (".ts", ".tsx", ".mts", ".cts")
_DECLARATION_EXTENSIONS = (".d.ts", ".d.mts", ".d.cts")

def compile_typescript(
    actions: AnalysisActions,
    label: Label,
    tsc: RunInfo | None,
    srcs: list[Artifact],
    tsconfig: Artifact,
    package_json: Artifact | None,
    dep_infos: list,
    identifier: str,
    node_modules_tree = None,
) -> Artifact:
    """Compile TypeScript `srcs` with `tsc` against the `node_modules` of `dep_infos` or `node_modules_tree`, returning the emitted tree.

    `package_json` is staged beside the sources so its `"type"` decides the emitted module format.
    """
    if tsc == None:
        fail("nodejs_library {}: typescript compilation requires `tsc` on the nodejs toolchain".format(label))
    if not srcs:
        fail("nodejs_library {}: typescript compilation requires at least one TypeScript source in `srcs`".format(label))
    for src in srcs:
        if not src.short_path.endswith(_TS_EXTENSIONS):
            fail(
                "nodejs_library {}: cannot compile non-TypeScript source '{}': only `.ts`, `.tsx`, `.mts` and `.cts` sources may be compiled".format(
                    label, src.short_path
                )
            )
        if src.short_path.endswith(_DECLARATION_EXTENSIONS):
            fail("nodejs_library {}: cannot compile declaration file '{}': `tsc` emits nothing for it".format(label, src.short_path))

    reserved = ["tsconfig.json", "tsconfig.buck.json"]
    if package_json != None:
        reserved.append("package.json")
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
        "__ts_config__/{}.json".format(identifier),
        {
            "compilerOptions": {"declarationDir": None},
            "extends": "./tsconfig.json",
            "files": sorted(stage.keys()),
            "include": [],
        },
    )
    stage["tsconfig.json"] = tsconfig
    if package_json != None:
        stage["package.json"] = package_json
    if dep_infos:
        dep_tree = build_node_modules(
            actions,
            "__ts_deps__/" + identifier,
            get_transitive_outputs(actions, deps = dep_infos),
        )
        stage["node_modules"] = dep_tree.project("node_modules")
    elif node_modules_tree != None:
        stage["node_modules"] = node_modules_tree
    src_dir = actions.symlinked_dir("__ts_src__/" + identifier, stage)

    out_dir = actions.declare_output("__tsc__/" + identifier, dir = True)

    cmd = cmd_args(
        tsc,
        "-p",
        cmd_args(src_dir, format = "{}/tsconfig.buck.json"),
        "--outDir",
        out_dir.as_output(),
        "--rootDir",
        src_dir,
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

def compile_module_typescript(actions: AnalysisActions, label: Label, tsc: RunInfo | None, module_dir: Artifact, identifier: str) -> Artifact:
    """Compile a whole project directory with the toolchain's `tsc`, returning the emitted tree.

    Emitted paths mirror source paths from the module root, so `src/main.ts`
    emits `src/main.js` even when the `tsconfig` sets `rootDir: "src"`. The
    module tsconfig must not require writes into the tree (no incremental or
    composite state).
    """
    if tsc == None:
        fail("nodejs_library {}: typescript compilation requires `tsc` on the nodejs toolchain".format(label))

    out_dir = actions.declare_output("__tsc_mod__/" + identifier, dir = True)

    cmd = cmd_args(
        tsc,
        "-p",
        cmd_args(module_dir, format = "{}/tsconfig.json"),
        "--outDir",
        out_dir.as_output(),
        "--rootDir",
        module_dir,
        "--declaration",
        "--noEmit",
        "false",
    )

    actions.run(
        cmd,
        category = "tsc_module_compile",
        identifier = identifier,
    )

    return out_dir
