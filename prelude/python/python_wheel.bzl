# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:cxx_toolchain_types.bzl", "PicBehavior")
load(
    "@prelude//cxx:link.bzl",
    "cxx_link_shared_library",
)
load(
    "@prelude//cxx:link_types.bzl",
    "link_options",
)
load("@prelude//linking:execution_preference.bzl", "LinkExecutionPreference")
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkStrategy",
    "get_lib_output_style",
    "get_link_info",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "LinkableGraph",
    "LinkableNode",  # @unused Used as a type
    "LinkableRootInfo",
    "get_deps_for_link",
    "get_linkable_graph_node_map_func",
    get_link_info_for_node = "get_link_info",
)
load("@prelude//python:manifest.bzl", "create_manifest_for_entries")
load("@prelude//python:python.bzl", "PythonLibraryInfo")
load("@prelude//utils:expect.bzl", "expect")
load(
    "@prelude//utils:graph_utils.bzl",
    "breadth_first_traversal_by",
)
load("@prelude//decls/toolchains_common.bzl", "toolchains_common")
load("@prelude//transitions/constraint_overrides.bzl", "constraint_overrides_transition")

def _link_deps(
        link_infos: dict[Label, LinkableNode],
        deps: list[Label],
        link_strategy: LinkStrategy,
        pic_behavior: PicBehavior) -> list[Label]:
    """
    Return transitive deps required to link dynamically against the given deps.
    This will following through deps of statically linked inputs and exported
    deps of everything else (see https://fburl.com/diffusion/rartsbkw from v1).
    """

    def find_deps(node: Label):
        return get_deps_for_link(link_infos[node], link_strategy, pic_behavior)

    return breadth_first_traversal_by(link_infos, deps, find_deps)

def _impl(ctx: AnalysisContext) -> list[Provider]:
    providers = []

    cmd = cmd_args(ctx.attrs._wheel[RunInfo])

    name_parts = [
        ctx.attrs.dist or ctx.attrs.name,
        ctx.attrs.version,
        ctx.attrs.python,
        ctx.attrs.abi,
        ctx.attrs.platform,
    ]
    wheel = ctx.actions.declare_output("{}.whl".format("-".join(name_parts)))
    cmd.add(cmd_args(wheel.as_output(), format = "--output={}"))

    cmd.add("--name={}".format(ctx.attrs.dist or ctx.attrs.name))
    cmd.add("--version={}".format(ctx.attrs.version))

    if ctx.attrs.entry_points:
        cmd.add("--entry-points={}".format(json.encode(ctx.attrs.entry_points)))

    for key, val in ctx.attrs.extra_metadata.items():
        cmd.add("--metadata={}:{}".format(key, val))

    libraries = {}
    for lib in ctx.attrs.libraries:
        libraries[lib.label] = lib
    if ctx.attrs.libraries_query != None:
        for lib in ctx.attrs.libraries_query:
            if PythonLibraryInfo in lib:
                libraries[lib.label] = lib

    srcs = []
    extensions = {}
    for dep in libraries.values():
        manifests = dep[PythonLibraryInfo].manifests.value
        if manifests.srcs != None:
            srcs.append(manifests.srcs)
        if manifests.resources != None:
            expect(not manifests.resources[1])
            srcs.append(manifests.resources[0])
        if manifests.extensions != None:
            toolchain_info = get_cxx_toolchain_info(ctx)
            items = manifests.extensions.items()
            expect(len(items) == 1)
            extension = items[0][0]
            root = dep[LinkableRootInfo]

            # Add link inputs for the linkable root and any deps.
            inputs = []
            inputs.append(get_link_info(
                infos = root.link_infos,
                prefer_stripped = ctx.attrs.prefer_stripped_objects,
            ))
            link_infos = get_linkable_graph_node_map_func(dep[LinkableGraph])()
            for ext_dep in _link_deps(
                link_infos,
                root.deps,
                LinkStrategy("static_pic"),
                toolchain_info.pic_behavior,
            ):
                node = link_infos[ext_dep]
                output_style = get_lib_output_style(
                    LinkStrategy("static_pic"),
                    node.preferred_linkage,
                    toolchain_info.pic_behavior,
                )
                inputs.append(get_link_info_for_node(
                    node,
                    output_style,
                    prefer_stripped = ctx.attrs.prefer_stripped_objects,
                ))

            # link the rule
            link_result = cxx_link_shared_library(
                ctx = ctx,
                output = extension,
                opts = link_options(
                    links = [LinkArgs(infos = inputs)],
                    category_suffix = "native_extension",
                    identifier = extension,
                    link_execution_preference = LinkExecutionPreference("any"),
                ),
            )
            extensions[extension] = link_result.linked_object

    if extensions:
        srcs.append(
            create_manifest_for_entries(
                ctx,
                name = "extensions.txt",
                entries = [
                    (name, extension.output, "<unknown>")
                    for name, extension in extensions.items()
                ],
            ),
        )

    for manifest in srcs:
        cmd.add(cmd_args(manifest.manifest, format = "--srcs={}"))
        for a, _ in manifest.artifacts:
            cmd.hidden(a)

    ctx.actions.run(cmd, category = "wheel")
    providers.append(DefaultInfo(default_output = wheel))

    return providers

python_wheel = rule(
    impl = _impl,
    cfg = constraint_overrides_transition,
    attrs = dict(
        dist = attrs.option(attrs.string(), default = None),
        version = attrs.string(default = "1.0.0"),
        python = attrs.string(
            default = select({
                "ovr_config//third-party/python/constraints:3.10": "py3.10",
                "ovr_config//third-party/python/constraints:3.11": "py3.11",
                "ovr_config//third-party/python/constraints:3.12": "py3.12",
                "ovr_config//third-party/python/constraints:3.8": "py3.8",
                "ovr_config//third-party/python/constraints:3.9": "py3.9",
            }),
        ),
        entry_points = attrs.dict(
            key = attrs.string(),
            value = attrs.dict(
                key = attrs.string(),
                value = attrs.string(),
            ),
            default = {},
        ),
        extra_metadata = attrs.dict(
            key = attrs.string(),
            value = attrs.string(),
            default = {},
        ),
        abi = attrs.string(default = "none"),
        platform = attrs.string(
            default = select({
                "DEFAULT": "any",
                "ovr_config//os:linux-arm64": "linux_aarch64",
                "ovr_config//os:linux-x86_64": "linux_x86_64",
            }),
        ),
        constraint_overrides = attrs.list(attrs.string(), default = []),
        libraries = attrs.list(attrs.dep(providers = [PythonLibraryInfo]), default = []),
        libraries_query = attrs.option(attrs.query(), default = None),
        prefer_stripped_objects = attrs.default_only(attrs.bool(default = False)),
        _wheel = attrs.default_only(attrs.exec_dep(default = "prelude//python/tools:wheel")),
        _cxx_toolchain = toolchains_common.cxx(),
    ),
)
