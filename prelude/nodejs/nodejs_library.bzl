# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":nodejs_providers.bzl", "NodejsLibraryInfo", "NodejsLibraryPackage", "build_node_modules", "get_nodejs_dep_infos", "get_transitive_outputs")

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

def nodejs_library_impl(ctx: AnalysisContext) -> list[Provider]:
    package_name = ctx.attrs.package_name
    is_default = package_name == None
    if is_default:
        package_name = ctx.attrs.name
    _validate_package_name(ctx.label, package_name, is_default = is_default)

    srcs_map = {}
    for src in ctx.attrs.srcs:
        key = "dist/" + src.short_path
        existing = srcs_map.get(key)
        if existing != None and existing != src:
            fail("nodejs_library {}: duplicate `srcs` '{}' and '{}' both map to '{}'".format(ctx.label, existing, src, key))
        srcs_map[key] = src
    for res in ctx.attrs.resources:
        key = "dist/" + res.short_path
        existing = srcs_map.get(key)
        if existing != None and existing != res:
            fail("nodejs_library {}: duplicate `srcs`/`resources` '{}' and '{}' both map to '{}'".format(ctx.label, existing, res, key))
        srcs_map[key] = res
    if ctx.attrs.package_json != None:
        srcs_map["package.json"] = ctx.attrs.package_json
    for key in sorted(srcs_map):
        segments = key.split("/")
        prefix = segments[0]
        for i in range(1, len(segments)):
            if prefix in srcs_map:
                fail(
                    "nodejs_library {}: overlapping staged paths '{}' and '{}' ('{}' and '{}'): staged entries must not nest".format(
                        ctx.label, prefix, key, srcs_map[prefix], srcs_map[key]
                    )
                )
            prefix += "/" + segments[i]
    package_dir = ctx.actions.copied_dir("package_dir", srcs_map)

    dep_infos = get_nodejs_dep_infos(ctx.attrs.deps, consumer_label = ctx.label, non_code_attr = "resources")

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
