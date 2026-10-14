# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")

NodejsLibraryPackage = record(
    label = field(Label),
    package_dir = field(Artifact),
    package_name = field(str),
    runtime_dir = field(Artifact),
)

NodejsLibraryTSet = transitive_set()

NodejsLibraryInfo = provider(
    doc = "Information about a Node.js library package.",
    fields = {
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

def get_nodejs_tree_info(node_modules: Dependency, consumer_label) -> NodejsTreeInfo:
    if NodejsTreeInfo not in node_modules:
        fail(
            "target {} has `node_modules` `{}` which does not provide `NodejsTreeInfo`; `node_modules` must be a `nodejs_node_modules` target".format(
                consumer_label, node_modules.label
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
        packages_by_name[own_package.package_name] = own_package
    for pkg in transitive_outputs.traverse():
        if own_package != None and pkg.package_name == own_package.package_name:
            if pkg.package_dir != own_package.package_dir:
                fail(
                    "duplicate `package_name` '{}' ('{}' vs '{}'); `package_name` must be unique across transitive deps".format(
                        pkg.package_name, own_package.label, pkg.label
                    )
                )
            continue
        existing = packages_by_name.get(pkg.package_name)
        if existing == None:
            packages_by_name[pkg.package_name] = pkg
        elif existing.package_dir != pkg.package_dir:
            fail(
                "duplicate `package_name` '{}' ('{}' vs '{}'); `package_name` must be unique across transitive deps".format(
                    pkg.package_name, existing.label, pkg.label
                )
            )
    tree = {"node_modules/" + n: packages_by_name[n].runtime_dir if for_runtime else packages_by_name[n].package_dir for n in sorted(packages_by_name)}
    for root_name, root_file in (root_files or {}).items():
        segments = check_relative_path(root_name, "invalid `root_files` path", allow_colon = True)
        if packages_by_name and segments[0].lower() == "node_modules":
            fail("root file '{}' collides with the merged `node_modules` tree".format(root_name))
        tree[root_name] = root_file
    return actions.copied_dir(name, tree)

def strip_type_declarations(actions: AnalysisActions, tree: Artifact, identifier: str, exec_os_type = None) -> Artifact:
    """Copy a package without `.d.ts`/`.d.mts`/`.d.cts` files or their maps, for packages that Node runs.

    Node's type stripping refuses declarations under `node_modules`, which breaks
    directory scanners like `require-directory`. Symlinks are copied as their
    targets, so declarations behind a linked file or directory are stripped too;
    a symlink cycle fails the action. On Windows executors the tree is returned
    unchanged.
    """
    is_windows = exec_os_type != None and exec_os_type[OsLookup].os == Os("windows")
    if is_windows:
        return tree
    stripped = actions.declare_output(identifier, dir = True)
    actions.run(
        cmd_args(
            "/bin/sh",
            "-c",
            'mkdir -p "$1" && cp -RL "$0/." "$1" && chmod -R u+w "$1" && find "$1" \\( -name "*.d.ts" -o -name "*.d.[mc]ts" -o -name "*.d.ts.map" -o -name "*.d.[mc]ts.map" \\) -delete',
            tree,
            stripped.as_output(),
        ),
        category = "strip_type_declarations",
        identifier = identifier,
    )
    return stripped
