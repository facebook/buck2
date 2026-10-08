# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":nodejs_providers.bzl", "NodejsLibraryInfo", "NodejsLibraryPackage", "build_node_modules", "get_nodejs_dep_infos", "get_transitive_outputs")

def _validate_package_name(label: Label, package_name: str) -> None:
    if not package_name:
        fail("nodejs_library {} has invalid `package_name` `{}`: must not be empty, expected `name` or `@scope/name`".format(label, package_name))
    if "\\" in package_name:
        fail("nodejs_library {} has invalid `package_name` `{}`: must not contain backslashes".format(label, package_name))
    if len(package_name) > 214:
        fail("nodejs_library {} has invalid `package_name` `{}`: must not exceed 214 characters".format(label, package_name))
    parts = package_name.split("/")
    if "" in parts:
        fail("nodejs_library {} has invalid `package_name` `{}`: must not contain empty segments".format(label, package_name))
    if "." in parts or ".." in parts:
        fail("nodejs_library {} has invalid `package_name` `{}`: must not contain `.` or `..` segments".format(label, package_name))
    is_unscoped = len(parts) == 1 and not parts[0].startswith("@")
    is_scoped = len(parts) == 2 and parts[0].startswith("@") and len(parts[0]) > 1
    if not is_unscoped and not is_scoped:
        fail("nodejs_library {} has invalid `package_name` `{}`: expected `name` or `@scope/name`".format(label, package_name))
    allowed_chars = "abcdefghijklmnopqrstuvwxyz0123456789-._~"
    for idx in range(len(parts)):
        part = parts[idx]
        segment = part[1:] if idx == 0 and part.startswith("@") else part
        if segment.startswith(".") or segment.startswith("_"):
            fail("nodejs_library {} has invalid `package_name` `{}`: segment `{}` must not start with `.` or `_`".format(label, package_name, segment))
        for i in range(len(segment)):
            if segment[i] not in allowed_chars:
                fail(
                    "nodejs_library {} has invalid `package_name` `{}`: segment `{}` must contain only lowercase letters, digits, and `-`, `.`, `_`, `~`".format(
                        label, package_name, segment
                    )
                )

def nodejs_library_impl(ctx: AnalysisContext) -> list[Provider]:
    package_name = ctx.attrs.package_name
    if package_name == None:
        package_name = ctx.attrs.name
    _validate_package_name(ctx.label, package_name)

    srcs_map = {}
    for src in ctx.attrs.srcs:
        key = "dist/" + src.short_path
        existing = srcs_map.get(key)
        if existing != None and existing != src:
            fail("nodejs_library {} has `srcs` with duplicate paths `{}`: {} and {}".format(ctx.label, key, existing, src))
        srcs_map[key] = src
    for res in ctx.attrs.resources:
        key = "dist/" + res.short_path
        existing = srcs_map.get(key)
        if existing != None and existing != res:
            fail("nodejs_library {} has `srcs`/`resources` with duplicate paths `{}`: {} and {}".format(ctx.label, key, existing, res))
        srcs_map[key] = res
    if ctx.attrs.package_json != None:
        srcs_map["package.json"] = ctx.attrs.package_json
    package_dir = ctx.actions.copied_dir("package_dir", srcs_map)

    dep_infos = get_nodejs_dep_infos(ctx.attrs.deps, consumer_label = ctx.label)

    own_package = NodejsLibraryPackage(
        label = ctx.label,
        package_dir = package_dir,
        package_name = package_name,
    )
    transitive_outputs = get_transitive_outputs(ctx.actions, value = own_package, deps = dep_infos)
    node_modules = build_node_modules(ctx.actions, "node_modules", transitive_outputs, own_package = own_package)

    return [
        DefaultInfo(
            default_output = package_dir,
            sub_targets = {"node_modules": [DefaultInfo(default_output = node_modules)]},
        ),
        NodejsLibraryInfo(
            package_dir = package_dir,
            package_name = package_name,
            transitive_outputs = transitive_outputs,
        ),
    ]
