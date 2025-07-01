# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:resources.bzl",
    "ResourceInfo",
    "gather_resources",
)
load(
    "@prelude//cxx:omnibus.bzl",
    "get_excluded",
    "get_roots",
)
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
    "format_system_include_arg",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
    "create_linkable_graph_node",
)
load(
    "@prelude//third-party:build.bzl",
    "create_third_party_build_root",
    "prefix_from_label",
    "project_from_label",
)
load("@prelude//third-party:providers.bzl", "ThirdPartyBuild", "third_party_build_info")
load("@prelude//unix:providers.bzl", "UnixEnv", "create_unix_env_info")
load(":compile.bzl", "compile_manifests")
load(":manifest.bzl", "ManifestInfo", "create_manifest_for_source_dir")
load(":python.bzl", "NativeDepsInfo", "NativeDepsInfoTSet")
load(
    ":python_library.bzl",
    "create_python_library_info",
    "gather_dep_libraries",
)
load(":source_db.bzl", "create_python_source_db_info", "create_source_db_no_deps_from_manifest")

def prebuilt_python_library_impl(ctx: AnalysisContext) -> list[Provider]:
    providers = []

    # Extract prebuilt wheel and wrap in python library provider.
    entry_points = ctx.actions.declare_output("entry_points.manifest")
    entry_points_dir = ctx.actions.declare_output("__entry_points__", dir = True)
    extracted_src = ctx.actions.declare_output("{}_extracted".format(ctx.label.name), dir = True)
    cmd = cmd_args(
        ctx.attrs._extract[RunInfo],
        ctx.attrs.binary_src,
        "--output",
        extracted_src.as_output(),
        "--entry-points-manifest",
        entry_points.as_output(),
        "--entry-points",
        entry_points_dir.as_output(),
    )
    if ctx.attrs.strip_soabi_tags:
        cmd.add("--strip-soabi-tags")
    inferred_cxx_header_dirs = None
    if ctx.attrs.infer_cxx_header_dirs:
        inferred_cxx_header_dirs = ctx.actions.declare_output("__cxx_header_dirs__.txt")
        cmd.add(
            "--cxx-header-dirs",
            inferred_cxx_header_dirs.as_output(),
        )
    ctx.actions.run(cmd, category = "py_extract_prebuilt_library")
    deps, shared_deps = gather_dep_libraries(ctx.attrs.deps)
    src_manifest = create_manifest_for_source_dir(ctx, "binary_src", extracted_src, exclude = "\\.pyc$")
    bytecode = compile_manifests(ctx, [src_manifest])
    library_info = create_python_library_info(
        ctx.actions,
        ctx.label,
        srcs = src_manifest,
        src_types = src_manifest,
        bytecode = bytecode,
        deps = deps,
        shared_libraries = shared_deps,
        native_deps = ctx.actions.tset(
            NativeDepsInfoTSet,
            value = NativeDepsInfo(native_deps = {}),
            children = [],
        ),
        is_native_dep = False,
    )
    providers.append(library_info)

    entry_points_manifest = ManifestInfo(
        manifest = entry_points,
        artifacts = [(entry_points_dir, "")],
    )

    # Create, augment and provide the linkable graph.
    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            roots = get_roots(ctx.attrs.deps),
            excluded = get_excluded(deps = ctx.attrs.deps if ctx.attrs.exclude_deps_from_merged_linking else []),
        ),
        deps = ctx.attrs.deps,
    )
    providers.append(linkable_graph)

    sub_targets = {"source-db-no-deps": [create_source_db_no_deps_from_manifest(ctx, src_manifest), create_python_source_db_info(library_info.manifests)]}
    providers.append(DefaultInfo(default_output = ctx.attrs.binary_src, sub_targets = sub_targets))

    # C++ resources.
    providers.append(ResourceInfo(resources = gather_resources(
        label = ctx.label,
        deps = ctx.attrs.deps,
    )))

    # Allow third-party-build rules to depend on Python rules.
    tp_project = project_from_label(ctx.label)
    tp_prefix = prefix_from_label(ctx.label)
    providers.append(
        third_party_build_info(
            actions = ctx.actions,
            build = ThirdPartyBuild(
                project = tp_project,
                prefix = tp_prefix,
                root = create_third_party_build_root(
                    ctx = ctx,
                    paths = [
                        ("lib/python", extracted_src),
                    ],
                    manifests = [
                        ("bin", entry_points_manifest),
                    ],
                ),
                manifest = ctx.actions.write_json(
                    "third_party_build_manifest.json",
                    dict(
                        prefix = tp_prefix,
                        project = tp_project,
                        py_lib_paths = ["lib/python"],
                    ),
                ),
            ),
            deps = ctx.attrs.deps,
        ),
    )

    # Unix env provider.
    providers.append(
        create_unix_env_info(
            actions = ctx.actions,
            env = UnixEnv(
                label = ctx.label,
                python_libs = [library_info],
                binaries = [entry_points_manifest],
            ),
            deps = ctx.attrs.deps,
        ),
    )

    # If this prebuilt wheel contains headers, export them via a C++ provider.
    pp_args = []
    if ctx.attrs.cxx_header_dirs:
        for header_dir in ctx.attrs.cxx_header_dirs:
            pp_args.append(
                format_system_include_arg(
                    cmd_args(extracted_src.project(header_dir)),
                    "clang",
                ),
            )
    if inferred_cxx_header_dirs != None:
        pp_argsfile = ctx.actions.declare_output("__cxx_header_dirs__.py_cxx_header_argsfile")

        def write_argsfile(actions, header_dirs, output):
            lines = []
            for header_dir in header_dirs.read_string().splitlines():
                lines.append(format_system_include_arg(
                    cmd_args(extracted_src.project(header_dir)),
                    "clang",
                ))
            actions.write(output, lines)

        ctx.actions.dynamic_output(
            dynamic = [inferred_cxx_header_dirs],
            inputs = [],
            outputs = [pp_argsfile.as_output()],
            f = lambda ctx, artifacts, outputs: write_argsfile(
                ctx.actions,
                artifacts[inferred_cxx_header_dirs],
                outputs[pp_argsfile],
            ),
        )
        pp_args.append(
            cmd_args(
                pp_argsfile,
                format = "@{}",
                hidden = [extracted_src],
            ),
        )
    if pp_args:
        providers.append(cxx_merge_cpreprocessors(
            ctx = ctx,
            own = [
                CPreprocessor(
                    args = CPreprocessorArgs(
                        args = pp_args,
                    ),
                ),
            ],
            xs = cxx_inherited_preprocessor_infos(ctx.attrs.deps),
        ))

    return providers
