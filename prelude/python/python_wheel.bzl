# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
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
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
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
load("@prelude//python:manifest.bzl", "ManifestInfo", "create_manifest_for_entries")
load("@prelude//python:python.bzl", "PythonLibraryInfo")
load("@prelude//python:toolchain.bzl", "PythonToolchainInfo")
load("@prelude//transitions:constraint_overrides.bzl", "constraint_overrides")
load("@prelude//utils:expect.bzl", "expect")
load(
    "@prelude//utils:graph_utils.bzl",
    "depth_first_traversal_by",
)

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

    return depth_first_traversal_by(link_infos, deps, find_deps)

def _whl_cmd(
        ctx: AnalysisContext,
        output: Artifact,
        manifests: list[ManifestInfo] = [],
        srcs: dict[str, Artifact] = {}) -> cmd_args:
    cmd = []

    cmd.append(ctx.attrs._wheel[RunInfo])

    cmd.append("--output")
    cmd.append(output.as_output())
    cmd.append("--name={}".format(ctx.attrs.dist or ctx.attrs.name))
    cmd.append("--version={}".format(ctx.attrs.version))

    if ctx.attrs.entry_points:
        cmd.append("--entry-points={}".format(json.encode(ctx.attrs.entry_points)))

    for key, val in ctx.attrs.extra_metadata.items():
        cmd.extend(["--metadata", key, val])

    version_matcher = ">=" if ctx.attrs.support_future_python_versions else "=="
    cmd.extend(["--metadata", "Requires-Python", "{}{}.*".format(version_matcher, ctx.attrs.python[2:])])

    for requires in ctx.attrs.requires:
        cmd.extend(["--metadata", "Requires-Dist", requires])

    for name, script in ctx.attrs.scripts.items():
        cmd.extend(["--data", paths.join("scripts", name), script])

    for dst, src in srcs.items():
        cmd.extend(["--src-path", dst, src])

    hidden = []
    for manifest in manifests:
        cmd.append("--manifest")
        cmd.append(manifest.manifest)
        for a, _ in manifest.artifacts:
            hidden.append(a)

    return cmd_args(cmd, hidden = hidden)

def _rpath(dst, rpath):
    """
    Relative the given `rpath` to `dst`, via `$ORIGIN`.
    """

    expect(not paths.is_absolute(dst))
    expect(not paths.is_absolute(rpath))

    base = "$ORIGIN"
    dirpath = paths.dirname(dst)
    if dirpath:
        base = paths.join(base, *[".." for _ in dirpath.split("/")])
    return paths.join(base, rpath)

def _impl(ctx: AnalysisContext) -> list[Provider]:
    providers = []
    sub_targets = {}

    libraries = {}
    for lib in ctx.attrs.libraries:
        libraries[lib.label] = lib
    if ctx.attrs.libraries_query != None:
        for lib in ctx.attrs.libraries_query:
            if PythonLibraryInfo in lib:
                libraries[lib.label] = lib

    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    toolchain_info = get_cxx_toolchain_info(ctx)

    # RPATHs to embed in the binary.
    rpaths = python_toolchain.wheel_rpaths + ctx.attrs.rpaths

    def maybe_patchelf(dst, src):
        # If there's no rpaths, there's nothing to patch.
        if not rpaths:
            return src

        # Heuristic: if we see an extension, assume it's not an ELF file (this might miss
        # DSOs embedded as resources).
        _, ext = paths.split_extension(dst)
        if ext:
            return src

        # Run the patchelf -- this will just copy if it turns out the given
        # path isn't and ELF file.
        out = ctx.actions.declare_output(paths.join("__patched__", dst))
        cmd = cmd_args(
            ctx.attrs._patchelf[RunInfo],
            "--output",
            out.as_output(),
            cmd_args([_rpath(dst, p) for p in rpaths], format = "--rpath={}"),
            src,
        )
        ctx.actions.run(cmd, category = "patchelf", identifier = dst)
        return out

    srcs = []
    extensions = {}
    for dep in libraries.values():
        manifests = dep[PythonLibraryInfo].manifests.value
        if manifests.srcs != None:
            srcs.append(manifests.srcs)
        if manifests.default_resources != None:
            expect(not manifests.default_resources[1])
            srcs.append(manifests.default_resources[0])
        if manifests.extensions != None:
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
                    links = [
                        LinkArgs(flags = python_toolchain.extension_linker_flags),
                        LinkArgs(flags = python_toolchain.wheel_linker_flags),
                        LinkArgs(flags = [
                            "-Wl,-rpath,{}".format(_rpath(extension, rpath))
                            for rpath in rpaths
                        ]),
                        LinkArgs(infos = inputs),
                    ],
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

    # Add in resources for the current rule.
    if ctx.attrs.resources:
        srcs.append(
            create_manifest_for_entries(
                ctx,
                name = "resources.txt",
                entries = [
                    (dest, maybe_patchelf(dest, resource), str(ctx.label.raw_target()))
                    for dest, resource in ctx.attrs.resources.items()
                ],
            ),
        )

    dist = ctx.attrs.dist or ctx.attrs.name
    name_parts = [
        dist,
        ctx.attrs.version,
        ctx.attrs.python,
        ctx.attrs.abi,
        ctx.attrs.platform,
    ]

    # Action to create wheel.
    wheel = ctx.actions.declare_output("{}.whl".format("-".join(name_parts)))
    whl_cmd = _whl_cmd(ctx = ctx, output = wheel, manifests = srcs)
    ctx.actions.run(whl_cmd, category = "wheel")

    # Create symlink tree for inplace module layout.
    manifest_args = []
    manifest_srcs = []
    for manifest in srcs:
        manifest_args.append(cmd_args(manifest.manifest, format = "--manifest={}"))
        for a, _ in manifest.artifacts:
            manifest_srcs.append(a)
    link_tree = ctx.actions.declare_output("__editable__/tree.d", dir = True)
    link_tree_cmd = cmd_args(
        ctx.attrs._create_link_tree[RunInfo],
        cmd_args(link_tree.as_output(), format = "--output={}"),
        manifest_args,
    )
    ctx.actions.run(link_tree_cmd, category = "link_tree")

    # Create <dist>.pth to put in the wheel and points to the symlink tree.
    pth = ctx.actions.declare_output("__editable__/{}.pth".format(dist))
    pth_cmd = cmd_args(
        "sh",
        "-c",
        # We need to embed an absolute paths to the link-tree into the `.pth` file.
        # so we'll also make sure this rule is local-only.
        'echo "$PWD/$2" > $1',
        "",
        pth.as_output(),
        # We don't care about the link tree contents, so prevent it from affecting
        # the cache key.
        cmd_args(link_tree, ignore_artifacts = True),
    )
    ctx.actions.run(pth_cmd, category = "pth", local_only = True)

    # Action to create editable wheel.
    ewheel = ctx.actions.declare_output("__editable__/{}.whl".format("-".join(name_parts)))
    ewhl_cmd = _whl_cmd(ctx = ctx, output = ewheel, srcs = {"{}.pth".format(dist): pth})
    ctx.actions.run(ewhl_cmd, category = "editable_wheel")
    sub_targets["editable"] = [
        DefaultInfo(
            default_output = ewheel,
            other_outputs = (
                manifest_srcs +
                [link_tree]
            ),
        ),
    ]

    providers.append(DefaultInfo(default_output = wheel, sub_targets = sub_targets))

    return providers

python_wheel = rule(
    impl = _impl,
    cfg = constraint_overrides.transition,
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
        requires = attrs.list(attrs.string(), default = []),
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
        libraries = attrs.list(attrs.dep(providers = [PythonLibraryInfo]), default = []),
        scripts = attrs.dict(key = attrs.string(), value = attrs.source(), default = {}),
        libraries_query = attrs.option(attrs.query(), default = None),
        prefer_stripped_objects = attrs.default_only(attrs.bool(default = False)),
        resources = attrs.dict(key = attrs.string(), value = attrs.source(), default = {}),
        rpaths = attrs.list(attrs.string(), default = []),
        support_future_python_versions = attrs.bool(default = False),
        _wheel = attrs.default_only(attrs.exec_dep(default = "prelude//python/tools:wheel")),
        _patchelf = attrs.default_only(attrs.exec_dep(default = "prelude//python/tools:patchelf")),
        _create_link_tree = attrs.default_only(attrs.exec_dep(default = "prelude//python/tools:create_link_tree")),
        _cxx_toolchain = toolchains_common.cxx(),
        _python_toolchain = toolchains_common.python(),
    ) | constraint_overrides.attributes,
)
