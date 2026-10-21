# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

NodejsLibraryPackage = record(
    label = field(Label),
    package_dir = field(Artifact),
    package_name = field(str),
    runtime_dir = field(Artifact),
    dir_name = field(str),
)

NodejsLibraryTSet = transitive_set()

NodejsLibraryInfo = provider(
    doc = "Information about a Node.js library package.",
    fields = {
        "dir_name": provider_field(str),
        "package_dir": provider_field(Artifact),
        "package_name": provider_field(str),
        "transitive_outputs": provider_field(NodejsLibraryTSet),
    },
)

NodejsTreeInfo = provider(
    doc = "A prebuilt `node_modules` directory tree (for example a yarn install output), usable where third-party packages have no `nodejs_library` target.",
    fields = {
        "node_modules": provider_field(Artifact),
    },
)

NodejsRuntimeKind = enum(
    # A Node.js binary for the target platform.
    "native",
    # One file, such as a DotSlash file, that runs Node.js on every platform.
    "pointer",
    # Nothing to bundle: Node.js comes from the host.
    "host",
)

NodejsRuntimeInfo = provider(
    doc = "A Node.js runtime that an app runs on or ships with.",
    fields = {
        "env": provider_field(dict[str, str], default = {}),
        # Runs this runtime in place, for example a wrapper that adds flags.
        "exe": provider_field(RunInfo | None, default = None),
        # Staged next to `node`, keyed by their path relative to it.
        "files": provider_field(dict[str, Artifact], default = {}),
        "kind": provider_field(NodejsRuntimeKind),
        # `None` for a host runtime, and for a native runtime with no binary for the target platform.
        "node": provider_field(Artifact | None, default = None),
        "node_args": provider_field(list[str], default = []),
        # Where the executable to run sits on POSIX: where `node` is staged.
        "posix_exe": provider_field(str, default = "node"),
        "version": provider_field(str | None, default = None),
        # Where the executable to run sits on Windows: where a native `node` is staged, or a pointer runtime's key of `files`.
        "windows_exe": provider_field(str, default = "node.exe"),
    },
)

def is_relative_slash_path(path: str) -> bool:
    segments = path.split("/")
    return not (path.startswith("/") or "\\" in path or "" in segments or "." in segments or ".." in segments)

def get_nodejs_dep_infos(deps: list, consumer_label, non_code_attr: str | None = None) -> list[NodejsLibraryInfo]:
    infos = []
    for dep in deps:
        if NodejsLibraryInfo not in dep:
            hint = " (use `{}` for non-code artifacts)".format(non_code_attr) if non_code_attr else ""
            fail(
                "target {} has `deps` entry `{}` which does not provide `NodejsLibraryInfo`; `deps` must be `nodejs_library` targets{}".format(
                    consumer_label, dep.label, hint
                )
            )
        infos.append(dep[NodejsLibraryInfo])
    return infos

def get_nodejs_library_info(library, consumer_label):
    if NodejsLibraryInfo not in library:
        fail(
            "target {} has `library` target `{}` which does not provide `NodejsLibraryInfo`; `library` must be a `nodejs_library`".format(
                consumer_label, library.label
            )
        )
    return library[NodejsLibraryInfo]

def get_nodejs_tree_info(node_modules: Dependency, consumer_label, attr: str = "node_modules") -> NodejsTreeInfo:
    if NodejsTreeInfo not in node_modules:
        fail(
            "target {} has `{}` `{}` which does not provide `NodejsTreeInfo`; `{}` must be a `nodejs_node_modules` target".format(
                consumer_label, attr, node_modules.label, attr
            )
        )
    return node_modules[NodejsTreeInfo]

def get_transitive_outputs(actions: AnalysisActions, value: NodejsLibraryPackage | None = None, deps: list[NodejsLibraryInfo] = []) -> NodejsLibraryTSet:
    kwargs = {}
    if value != None:
        kwargs["value"] = value
    if deps:
        kwargs["children"] = [info.transitive_outputs for info in deps]
    return actions.tset(NodejsLibraryTSet, **kwargs)

def check_relative_path(path: str, error_prefix: str, allow_colon: bool = False) -> list[str]:
    if not is_relative_slash_path(path) or (not allow_colon and ":" in path):
        colon_rule = "" if allow_colon else " no ':' and"
        fail("{} '{}': must be a relative path using '/' separators, with{} no '.' or '..' segments".format(error_prefix, path, colon_rule))
    return path.split("/")

def build_node_modules(
    actions: AnalysisActions,
    name: str,
    transitive_outputs: NodejsLibraryTSet,
    own_package: NodejsLibraryPackage | None = None,
    root_files: dict[str, Artifact] | None = None,
    for_runtime: bool = False,
) -> Artifact:
    packages_by_name = {}
    if own_package != None:
        packages_by_name[own_package.dir_name] = own_package
    for pkg in transitive_outputs.traverse():
        if own_package != None and pkg.dir_name == own_package.dir_name:
            if pkg.package_dir != own_package.package_dir:
                fail(
                    "duplicate package directory '{}' ('{}' vs '{}'); package directories must be unique across transitive deps".format(
                        pkg.dir_name, own_package.label, pkg.label
                    )
                )
            continue
        existing = packages_by_name.get(pkg.dir_name)
        if existing == None:
            packages_by_name[pkg.dir_name] = pkg
        elif existing.package_dir != pkg.package_dir:
            fail(
                "duplicate package directory '{}' ('{}' vs '{}'); package directories must be unique across transitive deps".format(
                    pkg.dir_name, existing.label, pkg.label
                )
            )
    for n in packages_by_name:
        segments = n.split("/")
        for i in range(1, len(segments)):
            parent = packages_by_name.get("/".join(segments[:i]))
            if parent != None:
                fail(
                    "package directory '{}' ('{}') is nested inside package directory '{}' ('{}'); package directories must not nest".format(
                        n, packages_by_name[n].label, parent.dir_name, parent.label
                    )
                )
    tree = {"node_modules/" + n: packages_by_name[n].runtime_dir if for_runtime else packages_by_name[n].package_dir for n in sorted(packages_by_name)}
    for root_name, root_file in (root_files or {}).items():
        segments = check_relative_path(root_name, "invalid `root_files` path", allow_colon = True)
        if packages_by_name and segments[0].lower() == "node_modules":
            fail("root file '{}' collides with the merged `node_modules` tree".format(root_name))
        tree[root_name] = root_file
    return actions.copied_dir(name, tree)

def strip_type_declarations(actions: AnalysisActions, package_tree_tool: RunInfo, tree: Artifact, identifier: str) -> Artifact:
    """Copy a package without `.d.ts`/`.d.mts`/`.d.cts` files or their maps, for packages that Node runs.

    Node's type stripping refuses declarations under `node_modules`, which breaks
    directory scanners like `require-directory`. Symlinks are copied as their
    targets, so declarations behind a linked file or directory are stripped
    too; a symlink cycle fails the action.
    """
    stripped = actions.declare_output(identifier, dir = True)
    actions.run(
        cmd_args(
            package_tree_tool,
            "strip-types",
            "--src",
            tree,
            "--out",
            stripped.as_output(),
        ),
        category = "strip_type_declarations",
        identifier = identifier,
    )
    return stripped
