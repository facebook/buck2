# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:prelude_rule.bzl", "prelude_rule")
load("@prelude//decls:common.bzl", "buck")
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load(
    ":nodejs_providers.bzl",
    "NodejsLibraryInfo",
    "NodejsLibraryPackage",
    "build_node_modules",
    "check_relative_path",
    "get_nodejs_dep_infos",
    "get_nodejs_tree_info",
    "get_transitive_outputs",
    "strip_type_declarations",
)
load(":nodejs_toolchain.bzl", "NodejsToolchainInfo")
load(":nodejs_typescript.bzl", "compile_module_typescript", "compile_typescript")

_StagedEntry = record(
    artifact = field(Artifact),
    attr = field(str),
    # Set only for `resources`, the only entries passed in `--entries`.
    name = field(str | None, None),
)

def _fail_invalid(label: Label, package_name: str, reason: str, hint: str) -> None:
    fail("nodejs_library {}: invalid `package_name` '{}': {}{}".format(label, package_name, reason, hint))

def _validate_package_name(label: Label, package_name: str, is_default: bool = False) -> None:
    hint = " (defaulted from target name; set `package_name` explicitly to override)" if is_default else ""
    if not package_name:
        _fail_invalid(
            label,
            package_name,
            "package names must not be empty",
            hint,
        )
    if len(package_name) > 214:
        _fail_invalid(label, package_name, "package names must be at most 214 characters long", hint)
    # `favicon.ico` may be reused as a scoped name; `node_modules` may not: it stages a `node_modules` directory under the scope, which Node treats as a lookup root for sibling packages.
    # Either may be the scope, which stages with its `@` prefix (for example `@node_modules`).
    if package_name in ["node_modules", "favicon.ico"]:
        _fail_invalid(label, package_name, "unscoped package names must not be 'node_modules' or 'favicon.ico'", hint)
    if package_name.startswith("@"):
        parts = package_name[1:].split("/")
        if len(parts) != 2 or not parts[0] or not parts[1]:
            _fail_invalid(label, package_name, "scoped names must have the form '@scope/name'", hint)
        if parts[1] == "node_modules":
            _fail_invalid(label, package_name, "scoped package names must not use 'node_modules' as the name segment", hint)
        segments = parts
    else:
        if "@" in package_name or "/" in package_name:
            _fail_invalid(
                label,
                package_name,
                "package names must not contain '@' or '/' unless they have the form '@scope/name'",
                hint,
            )
        segments = [package_name]
    for segment in segments:
        if segment.startswith(".") or segment.startswith("_"):
            _fail_invalid(label, package_name, "name segments must not start with '.' or '_'", hint)
        for ch in segment.elems():
            if ch not in "abcdefghijklmnopqrstuvwxyz0123456789-._~":
                _fail_invalid(
                    label,
                    package_name,
                    "package names must contain only lowercase letters, digits and '-._~'",
                    hint,
                )

def _check_path_attr(label: Label, what: str, path: str) -> None:
    segments = check_relative_path(path, "nodejs_library {}: invalid {}".format(label, what))
    if "node_modules" in [s.lower() for s in segments]:
        fail("nodejs_library {}: invalid {} '{}': must not contain a 'node_modules' segment".format(label, what, path))

def _src_root_relative(label: Label, src_root: str, attr: str, artifact: Artifact) -> str:
    path = artifact.short_path
    if not src_root:
        return path
    if not path.startswith(src_root + "/"):
        fail("nodejs_library {}: `{}` entry '{}' is not under `src_root` '{}'".format(label, attr, path, src_root))
    return path[len(src_root) + 1 :]

def _join(directory: str, path: str) -> str:
    return directory + "/" + path if directory else path

def _resource_entries(label: Label, src_root: str, package_subdir: str, resources: dict[str, Artifact] | list[Artifact]) -> list[(str, _StagedEntry)]:
    if isinstance(resources, dict):
        named = [(path, path, artifact) for path, artifact in resources.items()]
    else:
        named = [(artifact.short_path, _join(package_subdir, _src_root_relative(label, src_root, "resources", artifact)), artifact) for artifact in resources]
    entries = []
    for name, path, artifact in named:
        _check_path_attr(label, "`resources` path", path)
        if path == "package.json":
            fail("nodejs_library {}: `resources` '{}' maps to 'package.json', which is reserved for `package_json`".format(label, artifact))
        entries.append((path, _StagedEntry(artifact = artifact, attr = "`resources`", name = "`resources` entry '{}'".format(name))))
    return entries

def _stage(label: Label, staged: dict[str, _StagedEntry], path: str, entry: _StagedEntry) -> None:
    existing = staged.get(path)
    if existing == None:
        staged[path] = entry
    elif existing.artifact != entry.artifact:
        fail(
            "nodejs_library {}: duplicate {} '{}' and {} '{}' both map to '{}'".format(
                label, existing.attr, existing.artifact, entry.attr, entry.artifact, path
            )
        )

def _ancestors(path: str) -> list[str]:
    segments = path.split("/")
    return ["/".join(segments[:i]) for i in range(1, len(segments))]

def _nests(a: str, b: str) -> bool:
    return a == b or a in _ancestors(b) or b in _ancestors(a)

def _any_nests(staged: dict[str, _StagedEntry], path: str) -> bool:
    for staged_path in staged:
        if _nests(staged_path, path):
            return True
    return False

def _assemble_package(
    actions: AnalysisActions,
    package_tree_tool: RunInfo,
    emit: Artifact,
    package_subdir: str,
    staged: dict[str, _StagedEntry],
    identifier: str,
) -> Artifact:
    """The entry staged at `package.json` replaces an emitted one; any other entry that collides with an emitted file or directory fails the action."""
    package_dir = actions.declare_output("ts/p", dir = True)
    entries = actions.write_json(
        "ts/p.json",
        [(path, staged[path].artifact, staged[path].name) for path in sorted(staged) if path != "package.json"],
        with_inputs = True,
    )
    manifest = staged.get("package.json")
    actions.run(
        cmd_args(
            package_tree_tool,
            "assemble-package",
            "--emit",
            emit,
            cmd_args("--package-subdir", package_subdir) if package_subdir else [],
            "--entries",
            entries,
            cmd_args("--package-json", manifest.artifact) if manifest != None else [],
            "--out",
            package_dir.as_output(),
        ),
        category = "assemble_package",
        identifier = identifier,
    )
    return package_dir

def nodejs_library_impl(ctx: AnalysisContext) -> list[Provider]:
    package_name = ctx.attrs.package_name
    is_default = package_name == None
    if is_default:
        package_name = ctx.attrs.name
    _validate_package_name(ctx.label, package_name, is_default = is_default)

    dep_infos = get_nodejs_dep_infos(ctx.attrs.deps, consumer_label = ctx.label, non_code_attr = "resources")
    if ctx.attrs.out != None:
        _check_path_attr(ctx.label, "`out`", ctx.attrs.out)
    dir_name = ctx.attrs.out if ctx.attrs.out != None else package_name
    src_root = ctx.attrs.src_root
    if src_root:
        _check_path_attr(ctx.label, "`src_root`", src_root)
    package_subdir = ctx.attrs.package_subdir
    if package_subdir:
        _check_path_attr(ctx.label, "`package_subdir`", package_subdir)
    if ctx.attrs.module != None:
        if ctx.attrs.srcs or dep_infos or ctx.attrs.node_modules != None:
            fail("nodejs_library {}: `module` cannot be combined with `srcs`, `deps` or `node_modules`".format(ctx.label))
        if ctx.attrs.tsconfig != None:
            fail("nodejs_library {}: `module` uses the module's own tsconfig.json; `tsconfig` cannot be combined with `module`".format(ctx.label))
    compiled = ctx.attrs.module != None or ctx.attrs.tsconfig != None
    staged = {}
    for src in ctx.attrs.srcs:
        path = _src_root_relative(ctx.label, src_root, "srcs", src)
        if not compiled:
            _stage(ctx.label, staged, _join(package_subdir, path), _StagedEntry(artifact = src, attr = "`srcs`"))
    for path, entry in _resource_entries(ctx.label, src_root, package_subdir, ctx.attrs.resources):
        _stage(ctx.label, staged, path, entry)
    if ctx.attrs.package_json != None:
        _stage(ctx.label, staged, "package.json", _StagedEntry(artifact = ctx.attrs.package_json, attr = "`package_json`"))
    for path in sorted(staged):
        for prefix in _ancestors(path):
            if prefix in staged:
                fail(
                    "nodejs_library {}: overlapping staged paths '{}' and '{}' ('{}' and '{}'): staged entries must not nest".format(
                        ctx.label, prefix, path, staged[prefix].artifact, staged[path].artifact
                    )
                )

    node_modules_tree = None
    if ctx.attrs.node_modules != None:
        node_modules_tree = get_nodejs_tree_info(ctx.attrs.node_modules, consumer_label = ctx.label).node_modules

    emit = None
    if ctx.attrs.module != None:
        module_outs = ctx.attrs.module[DefaultInfo].default_outputs
        if len(module_outs) != 1:
            fail("nodejs_library {}: `module` target `{}` must have exactly one default output".format(ctx.label, ctx.attrs.module.label))
        emit = compile_module_typescript(
            ctx.actions,
            ctx.label,
            ctx.attrs._nodejs_toolchain[NodejsToolchainInfo].tsc,
            module_outs[0],
            src_root,
            ctx.attrs.name,
        )
    elif ctx.attrs.tsconfig != None:
        emit = compile_typescript(
            actions = ctx.actions,
            label = ctx.label,
            tsc = ctx.attrs._nodejs_toolchain[NodejsToolchainInfo].tsc,
            srcs = ctx.attrs.srcs,
            src_root = src_root,
            tsconfig = ctx.attrs.tsconfig,
            package_json = ctx.attrs.package_json,
            dep_infos = dep_infos,
            identifier = ctx.attrs.name,
            package_tree_tool = ctx.attrs._package_tree_tool[RunInfo],
            node_modules_tree = node_modules_tree,
        )
    elif node_modules_tree != None:
        fail("nodejs_library {}: `node_modules` requires `tsconfig`: without compilation the tree is unused".format(ctx.label))
    files = {path: entry.artifact for path, entry in staged.items()}
    if emit == None:
        package_dir = ctx.actions.copied_dir("package_dir", files)
    elif package_subdir and not _any_nests(staged, package_subdir):
        package_dir = ctx.actions.copied_dir("package_dir", files | {package_subdir: emit})
    else:
        package_dir = _assemble_package(ctx.actions, ctx.attrs._package_tree_tool[RunInfo], emit, package_subdir, staged, ctx.attrs.name)

    runtime_dir = strip_type_declarations(ctx.actions, ctx.attrs._package_tree_tool[RunInfo], package_dir, "runtime_dir")
    own_package = NodejsLibraryPackage(
        label = ctx.label,
        package_dir = package_dir,
        package_name = package_name,
        runtime_dir = runtime_dir,
        dir_name = dir_name,
    )
    transitive_outputs = get_transitive_outputs(ctx.actions, value = own_package, deps = dep_infos)
    node_modules = build_node_modules(ctx.actions, "node_modules", transitive_outputs, own_package = own_package)

    return [
        DefaultInfo(
            default_output = package_dir,
            sub_targets = {
                "node_modules": [DefaultInfo(default_output = node_modules)],
                "runtime": [DefaultInfo(default_output = runtime_dir)],
            },
        ),
        NodejsLibraryInfo(
            package_dir = package_dir,
            package_name = package_name,
            dir_name = dir_name,
            transitive_outputs = transitive_outputs,
        ),
    ]

nodejs_library = prelude_rule(
    name = "nodejs_library",
    impl = nodejs_library_impl,
    docs = """A Node.js library that packages code as a regular
    `node_modules` package loadable by Node without custom loaders.
    `src_root` (default `""`) is the directory that staged and emitted
    paths are relative to. For `srcs` and a `resources` list it applies to
    each artifact's short path (a source file's path inside its BUCK
    package, a generated artifact's path inside its rule's output), and
    every entry must be under it; for `module` it applies inside the
    module root. `package_subdir` (default `""`, the package root, beside
    `package.json`) is the directory inside the package that compiled
    output, the `module` emit, uncompiled `srcs` and `resources` list
    entries are staged in, e.g. `"dist"`. Both are
    relative paths with `/` separators, no `:` and no `.`, `..` or
    `node_modules` segments.
    Without `tsconfig`, the artifacts it is given are packaged as-is
    (no compilation): whatever produced them (plain JavaScript sources,
    generated files, another rule's output) feeds `srcs`, and each entry
    is staged at `<package_subdir>/<path>` inside the package, where `<path>`
    is its short path relative to `src_root` (repeating the
    same source is allowed).
    With `tsconfig`, `srcs` must be TypeScript compiled with the
    toolchain's `tsc` before staging: sources are staged by short path
    next to the `tsconfig`, the `package_json` (whose `"type"` decides the
    emitted module format) and a `node_modules` (the merged tree of `deps`
    and the prebuilt `node_modules` tree; so cross-package type imports
    resolve), compiled with `--outDir`,
    `--rootDir` (`src_root` inside that staging directory), `--declaration`,
    `--noEmit false` and `--emitDeclarationOnly false`, into `package_subdir`.
    `srcs` replace the `tsconfig`'s `files` and `include`. Only `.ts`,
    `.tsx`, `.mts` and `.cts` sources, not declarations, are compiled.
    Only the `tsconfig` is staged, so an `extends`
    pointing at a sibling config file does not resolve. A source whose
    first path segment is `tsconfig.json`, `tsconfig.buck.json`, or a
    staged `package.json` or `node_modules` fails.
    `resources` ship non-code files and directories with the package,
    whether or not its code is compiled: source files and directories,
    generated files, and other rules' directory outputs. A list entry is
    staged like an uncompiled source, at `<package_subdir>/<path>` where
    `<path>` is its short path relative to `src_root`, so it keeps its
    place relative to code compiled from the same package and lookups
    through `__dirname` resolve as they do in the source tree. The
    `module` emit mirrors the module root instead, so a list entry lines
    up with it only when the entry's short path matches its place in the
    module root, as a source of a `filegroup` module's own package does;
    otherwise use a dict. A dict maps package-relative paths to entries
    staged at exactly those paths. Every path a resource is staged at is
    relative and `/`-separated, with no `:` and no `.`, `..` or
    `node_modules` segments; `package.json` is reserved for
    `package_json`. Two distinct entries (uncompiled `srcs`, `resources`
    or `package_json`) that map to the same staged path fail, and staged
    paths must not nest, e.g. `a` and `a/b` fail. With compiled
    content, `resources` may be staged inside the compiled output and
    merge into its directories, but one that collides with an emitted
    file or directory fails the build with an error naming both. `deps`
    must be `nodejs_library` targets; use `resources` for non-code
    artifacts.
    Staging copies files so the package runs under Node without
    `--preserve-symlinks`. `package_json`, when supplied, is staged at the
    package root as `package.json`, replacing a `package.json` the compile
    emits there. Its `name` is not checked against
    `package_name`; keep the two in sync. Without it, importing by package
    name does not resolve in Node; for packages that must be imported by name,
    supply `package_json` whose `exports` (or `main`) entry points at the
    staged file, e.g. `{"exports": {".": "./<file>"}}`.
    `package_name` (default: target name): a valid npm package name of
    at most 214 characters, `name` or `@scope/name`, where each segment
    contains only lowercase letters, digits, `-`, `.`, `_`, `~` and does
    not start with `.` or `_`. The unscoped names `node_modules` and
    `favicon.ico` are reserved and rejected, as is `@scope/node_modules`;
    either may be used as the scope.
    `node_modules`, when set, stages a prebuilt `node_modules` tree (a
    `nodejs_node_modules` target, e.g. over a yarn install output) for the
    `tsc` compile, merged with the `deps` tree when both are present, for
    third-party packages with no `nodejs_library` target. It requires
    `tsconfig`.
    `module`, when set, compiles a whole project directory (a target whose
    single output holds a `tsconfig.json` and its sources) instead of
    compiling `srcs`: `tsc -p` runs on the module's own `tsconfig.json`
    with `--outDir`, `--rootDir` (`src_root` inside the
    module root, overriding any `tsconfig` `rootDir`), `--declaration` and
    `--noEmit false`,
    and the emitted tree is staged as the package's `package_subdir`, mirroring
    source paths (with the defaults, `src/main.ts` emits
    `src/main.js`). It cannot be
    combined with `srcs`, `deps`, `node_modules` or `tsconfig`;
    the module tsconfig must not require writes into the tree. Only the
    emitted tree is staged, not the module's own `node_modules`: a
    `nodejs_binary` running the package must supply that tree through its
    own `node_modules`.
    `out`, when set, stages the package under that directory name inside
    `node_modules` instead of `package_name` (a relative path with `/`
    separators, no `:` and no `.`, `..` or `node_modules` segments), for packages whose
    imports address a canonical directory.
    The default output is the package's own directory
    (`package_dir`). The `[node_modules]` sub-target is the merged
    `node_modules` tree for this package and its transitive `deps`,
    laid out by `package_name` (or `out`). A duplicate `package_name` in the
    transitive closure with a different package output fails the build, as
    does one package directory nested inside another. The `[runtime]`
    sub-target is the package directory without its `.d.ts`, `.d.mts` and
    `.d.cts` files and their maps, with symlinks copied as their targets;
    `nodejs_binary` and `nodejs_test` stage it in their merged tree.""",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "srcs": attrs.list(attrs.source(), default = []),
            "resources": attrs.named_set(attrs.source(allow_directory = True), sorted = True, default = []),
            "package_name": attrs.option(attrs.string(), default = None),
            "package_json": attrs.option(attrs.source(allow_directory = False), default = None),
            "tsconfig": attrs.option(attrs.source(allow_directory = False), default = None),
            "deps": attrs.list(attrs.dep(), default = []),
            "node_modules": attrs.option(attrs.dep(), default = None),
            "module": attrs.option(attrs.dep(), default = None),
            "out": attrs.option(attrs.string(), default = None),
            "src_root": attrs.string(default = ""),
            "package_subdir": attrs.string(default = ""),
            "_nodejs_toolchain": toolchains_common.nodejs(),
            "_package_tree_tool": attrs.default_only(attrs.exec_dep(providers = [RunInfo], default = "prelude//nodejs/tools:package_tree")),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
    ),
)
