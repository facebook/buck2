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

def get_nodejs_dep_infos(deps: list, consumer_label) -> list[NodejsLibraryInfo]:
    infos = []
    for dep in deps:
        if NodejsLibraryInfo not in dep:
            fail(
                "target {} has `deps` entry `{}` which does not provide `NodejsLibraryInfo`; `deps` must be `nodejs_library` targets (use `resources` for non-code artifacts)".format(
                    consumer_label, dep.label
                )
            )
        infos.append(dep[NodejsLibraryInfo])
    return infos

def get_transitive_outputs(actions: AnalysisActions, value: NodejsLibraryPackage | None = None, deps: list[NodejsLibraryInfo] = []) -> NodejsLibraryTSet:
    kwargs = {}
    if value != None:
        kwargs["value"] = value
    if deps:
        kwargs["children"] = [info.transitive_outputs for info in deps]
    return actions.tset(NodejsLibraryTSet, **kwargs)

def build_node_modules(
    actions: AnalysisActions,
    name: str,
    transitive_outputs: NodejsLibraryTSet,
    own_package: NodejsLibraryPackage | None = None,
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
    return actions.copied_dir(name, {"node_modules/" + n: packages_by_name[n].package_dir for n in sorted(packages_by_name)})
