# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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
load(":nodejs_typescript.bzl", "assemble_flat_package", "compile_module_typescript", "compile_typescript")

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

    dep_infos = get_nodejs_dep_infos(ctx.attrs.deps, consumer_label = ctx.label, non_code_attr = "resources")
    if ctx.attrs.out != None:
        out_segments = check_relative_path(ctx.attrs.out, "nodejs_library {}: invalid `out`".format(ctx.label))
        if "node_modules" in [s.lower() for s in out_segments]:
            fail("nodejs_library {}: invalid `out` '{}': must not contain a 'node_modules' segment".format(ctx.label, ctx.attrs.out))
    dir_name = ctx.attrs.out if ctx.attrs.out != None else package_name
    if ctx.attrs.flatten and ctx.attrs.tsconfig == None and ctx.attrs.module == None:
        fail("nodejs_library {}: `flatten` requires compilation (`tsconfig` or `module`)".format(ctx.label))
    if ctx.attrs.module != None:
        if ctx.attrs.srcs or ctx.attrs.resources or dep_infos or ctx.attrs.node_modules != None:
            fail("nodejs_library {}: `module` cannot be combined with `srcs`, `resources`, `deps` or `node_modules`".format(ctx.label))
        if ctx.attrs.tsconfig != None:
            fail("nodejs_library {}: `module` uses the module's own tsconfig.json; `tsconfig` cannot be combined with `module`".format(ctx.label))
    node_modules_tree = None
    if ctx.attrs.node_modules != None:
        node_modules_tree = get_nodejs_tree_info(ctx.attrs.node_modules, consumer_label = ctx.label).node_modules

    srcs_map = {}
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
            ctx.attrs.name,
        )
    elif ctx.attrs.tsconfig != None:
        if ctx.attrs.resources:
            fail("nodejs_library {}: `resources` cannot be combined with `tsconfig` yet".format(ctx.label))
        emit = compile_typescript(
            actions = ctx.actions,
            label = ctx.label,
            tsc = ctx.attrs._nodejs_toolchain[NodejsToolchainInfo].tsc,
            srcs = ctx.attrs.srcs,
            tsconfig = ctx.attrs.tsconfig,
            package_json = ctx.attrs.package_json,
            dep_infos = dep_infos,
            identifier = ctx.attrs.name,
            package_tree_tool = ctx.attrs._package_tree_tool[RunInfo],
            node_modules_tree = node_modules_tree,
        )
    else:
        if node_modules_tree != None:
            fail("nodejs_library {}: `node_modules` requires `tsconfig`: without compilation the tree is unused".format(ctx.label))
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
    if emit != None and ctx.attrs.flatten:
        package_dir = assemble_flat_package(
            ctx.actions,
            ctx.attrs._package_tree_tool[RunInfo],
            emit,
            ctx.attrs.package_json,
            ctx.attrs.name,
        )
    else:
        if emit != None:
            srcs_map["dist"] = emit
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

    own_package = NodejsLibraryPackage(
        label = ctx.label,
        package_dir = package_dir,
        package_name = package_name,
        runtime_dir = strip_type_declarations(ctx.actions, package_dir, "runtime_dir", ctx.attrs._exec_os_type),
        dir_name = dir_name,
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
            dir_name = dir_name,
            transitive_outputs = transitive_outputs,
        ),
    ]
