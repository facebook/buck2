# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":common.bzl", "buck", "prelude_rule")
load(":toolchains_common.bzl", "toolchains_common")

nodejs_library = prelude_rule(
    name = "nodejs_library",
    docs = """A Node.js library that packages code as a regular
    `node_modules` package loadable by Node without custom loaders.
    Without `tsconfig`, the artifacts it is given are packaged as-is
    (no compilation): whatever produced them (plain JavaScript sources,
    generated files, another rule's output) feeds `srcs`, and each entry
    is staged under `dist/<short_path>` inside the package (repeating the
    same source is allowed; distinct sources mapping to the same staged
    path fail, and staged paths must not nest, e.g. `dist/a` and
    `dist/a/b` fail).
    With `tsconfig`, `srcs` must be TypeScript compiled with the
    toolchain's `tsc` before staging: sources are staged by short path
    next to the `tsconfig`, the `package_json` (whose `"type"` decides the
    emitted module format) and a `node_modules` (the merged tree of `deps`,
    or the prebuilt `node_modules` tree; so cross-package type imports
    resolve), compiled with `--outDir`,
    `--rootDir` (the staging root, so `<path>.ts` emits
    `dist/<path>.js` and `dist/<path>.d.ts`, and `.mts` / `.cts` emit
    `.mjs` / `.cjs`), `--declaration`, `--noEmit false` and
    `--emitDeclarationOnly false`, and the emitted tree is staged as the
    package's `dist/`. `srcs` are the compiled files: the `tsconfig`'s
    `files` and `include` are replaced by `srcs` and its `declarationDir`
    is cleared. At least one source is required. Only `.ts`, `.tsx`,
    `.mts` and `.cts` sources may be compiled, not `.d.ts`, `.d.mts` or
    `.d.cts` declaration files; declarations are always emitted because
    downstream compiles need them. Only the `tsconfig` file itself is staged, so an `extends`
    pointing at a sibling config file does not resolve. A source whose
    first path segment is `tsconfig.json`, `tsconfig.buck.json`, or a
    staged `package.json` or `node_modules` fails.
    Without `tsconfig`, each entry of `resources` is staged under
    `dist/<short_path>`, for non-code files (including generated artifacts
    and directory artifacts from other rules) that ship with the package;
    `resources` cannot be combined with `tsconfig`. `deps` must
    be `nodejs_library` targets; use `resources` for non-code artifacts.
    Staging copies files so the package runs under Node without
    `--preserve-symlinks`. `package_json`, when supplied, is staged at the
    package root as `package.json`. Its `name` is not checked against
    `package_name`; keep the two in sync. Without it, importing by package
    name does not resolve in Node; for packages that must be imported by name,
    supply `package_json` whose `exports` (or `main`) entry points at the
    staged file, e.g. `{"exports": {".": "./dist/<file>"}}`.
    `package_name` (default: target name): a valid npm package name of
    at most 214 characters, `name` or `@scope/name`, where each segment
    contains only lowercase letters, digits, `-`, `.`, `_`, `~` and does
    not start with `.` or `_`. The unscoped names `node_modules` and
    `favicon.ico` are reserved and rejected, as is `@scope/node_modules`;
    either may be used as the scope.
    `node_modules`, when set, stages a prebuilt `node_modules` tree (a
    `nodejs_node_modules` target, e.g. over a yarn install output) for the
    `tsc` compile instead of the merged `deps` tree, for third-party
    packages with no `nodejs_library` target. It requires `tsconfig` and
    cannot be combined with `deps`: prebuilt trees cannot merge with
    package deps.
    The default output is the package's own directory
    (`package_dir`). The `[node_modules]` sub-target is the merged
    `node_modules` tree for this package and its transitive `deps`,
    laid out by `package_name`. A duplicate `package_name` in the
    transitive closure with a different package output fails the build.""",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "srcs": attrs.list(attrs.source(), default = []),
            "resources": attrs.list(attrs.source(), default = []),
            "package_name": attrs.option(attrs.string(), default = None),
            "package_json": attrs.option(attrs.source(allow_directory = False), default = None),
            "tsconfig": attrs.option(attrs.source(allow_directory = False), default = None),
            "deps": attrs.list(attrs.dep(), default = []),
            "node_modules": attrs.option(attrs.dep(), default = None),
            "_nodejs_toolchain": toolchains_common.nodejs(),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
    ),
)

nodejs_node_modules = prelude_rule(
    name = "nodejs_node_modules",
    docs = """A prebuilt `node_modules` directory tree, for third-party
    packages with no `nodejs_library` target (e.g. a yarn install output).
    `src` is a target whose default output is a directory; `path` (default
    `"node_modules"`) is the tree's relative, `/`-separated location inside
    that output, with no `.` or `..` segments. The default
    output is the tree itself, and consumers take it via `node_modules`.""",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "src": attrs.dep(),
            "path": attrs.string(default = "node_modules"),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
    ),
)

nodejs_binary = prelude_rule(
    name = "nodejs_binary",
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
    resolution. Transitive deps' `package_name` values must be unique,
    as for `nodejs_library`. `package_style` is `"inplace"` (the
    default) or `"standalone"`. `standalone` always stages, adds the
    toolchain's `node_dist` binary to the staged directory as `node`
    (`node.exe` on Windows), and replaces the default output with a zip of that
    directory under a top-level directory named after the target (which
    therefore cannot contain `/`), for hosts without Node. The package runs
    as `<name>/node <name>/<main>`;
    `args`, `node_args`, `env`, `NODE_PATH` and any flags or env the
    toolchain's `node` wrapper adds are not applied there.""",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "main": attrs.source(),
            "srcs": attrs.list(attrs.source(), default = []),
            "args": attrs.list(attrs.arg(), default = []),
            "node_args": attrs.list(attrs.string(), default = []),
            "env": attrs.dict(attrs.string(), attrs.arg(), default = {}),
            "deps": attrs.list(attrs.dep(), default = []),
            "package_style": attrs.enum(["inplace", "standalone"], default = "inplace"),
            "_nodejs_toolchain": toolchains_common.nodejs(),
            "_target_os_type": buck.target_os_type_arg(),
            "_zip_file_toolchain": toolchains_common.zip_file(),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
    ),
)

nodejs_test = prelude_rule(
    name = "nodejs_test",
    docs = """A Node.js test target, executed with the toolchain's Node
    runtime and no custom loaders; the exit code is pass/fail. When
    `deps` is non-empty, builds a merged `node_modules` from transitive
    deps, stages `main` and `srcs` beside it (each at its short path;
    distinct files staging to the same path fail), and runs the staged
    `main` so Node's own resolution finds the deps. The default output
    is that directory. With no `deps`, `main` runs at its original path,
    files it imports via relative paths must be listed in `srcs` to be
    declared as build inputs, and there is no default output. Deps must
    be loadable as regular `node_modules` packages: each package's
    `package.json` governs whether it is CommonJS or an ES module, and
    TypeScript must be compiled to JavaScript before it is packaged.
    The merged tree is also exposed via `NODE_PATH` at runtime, appended
    to any `NODE_PATH` set in `env`, for CommonJS resolution. Transitive
    deps' `package_name` values must be unique, as for `nodejs_library`.
    It runs via a `command_alias` wrapper script for the target platform,
    which sets `env` (including `NODE_PATH` from `deps`) on Unix and
    Windows.""",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "main": attrs.source(),
            "srcs": attrs.list(attrs.source(), default = []),
            "args": attrs.list(attrs.arg(), default = []),
            "node_args": attrs.list(attrs.string(), default = []),
            "env": attrs.dict(attrs.string(), attrs.arg(), default = {}),
            "deps": attrs.list(attrs.dep(), default = []),
            "_nodejs_toolchain": toolchains_common.nodejs(),
            "_target_os_type": buck.target_os_type_arg(),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
        | buck.inject_test_env_arg()
    ),
)

nodejs_rules = struct(
    nodejs_binary = nodejs_binary,
    nodejs_library = nodejs_library,
    nodejs_node_modules = nodejs_node_modules,
    nodejs_test = nodejs_test,
)
