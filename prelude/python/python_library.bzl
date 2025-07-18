# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifacts.bzl",
    "ArtifactOutputs",  # @unused Used as a type
    "unpack_artifact_map",
)
load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//:resources.bzl",
    "ResourceInfo",
    "gather_resources",
)
load("@prelude//cxx:cxx_link_utility.bzl", "shared_libs_symlink_tree_name")
load(
    "@prelude//cxx:omnibus.bzl",
    "get_excluded",
    "get_roots",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkedObject",  # @unused Used as a type
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "LinkableRootInfo",
    "create_linkable_graph",
    "create_linkable_graph_node",
)
load("@prelude//linking:shared_libraries.bzl", "SharedLibraryInfo", "merge_shared_libraries")
load("@prelude//python:toolchain.bzl", "PythonPlatformInfo", "get_platform_attr")
load(
    "@prelude//python/linking:native_python_util.bzl",
    "merge_cxx_extension_info",
    "merge_native_deps",
)
load(
    "@prelude//third-party:build.bzl",
    "create_third_party_build_root",
    "prefix_from_label",
    "project_from_label",
)
load("@prelude//third-party:providers.bzl", "ThirdPartyBuild", "third_party_build_info")
load("@prelude//unix:providers.bzl", "UnixEnv", "create_unix_env_info")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load("@prelude//utils:expect.bzl", "expect")
load("@prelude//utils:utils.bzl", "flatten", "from_named_set")
load(":compile.bzl", "PycInvalidationMode", "compile_manifests")
load(
    ":manifest.bzl",
    "ManifestInfo",  # @unused Used as a type
    "create_manifest_for_source_map",
)
load(":needed_coverage.bzl", "PythonNeededCoverageInfo")
load(":python.bzl", "NativeDepsInfoTSet", "PythonLibraryInfo", "PythonLibraryManifests", "PythonLibraryManifestsTSet")
load(":source_db.bzl", "create_python_source_db_info", "create_source_db_no_deps")
load(":toolchain.bzl", "PythonToolchainInfo")
load(":typing.bzl", "create_per_target_type_check")
load(":versions.bzl", "VersionedDependenciesInfo", "gather_versioned_dependencies", "resolve_versions")

def dest_prefix(label: Label, base_module: [None, str]) -> str:
    """
    Find the prefix to use for placing files inside of the python link tree

    This uses the label's package path if `base_module` is `None`, or `base_module`,
    with '.' replaced by '/', if not None. If non-empty, the returned prefix will
    end with a '/'
    """
    if base_module == None:
        prefix = label.package
    else:
        prefix = base_module.replace(".", "/")

    # Add a leading slash if we need to, but don't do that for an empty base_module
    if prefix != "":
        prefix += "/"

    return prefix

def qualify_srcs(
        label: Label,
        base_module: [None, str],
        srcs: dict[str, typing.Any]) -> dict[str, typing.Any]:
    """
    Fully qualify package-relative sources with the rule's base module.

    Arguments:
        label: The label for the `python_library`. Used for errors, and to construct
               the path for each source file
        base_module: If provided, the module to prefix all files from `srcs` with in
                     the eventual binary. If `None`, use the package path.
                     Usage of this is discouraged, because it makes on-disk paths
                     not match the module in execution.
        srcs: A dictionary of {relative destination path: source file}. The derived
              base module will be prepended to the destination.
    """
    prefix = dest_prefix(label, base_module)

    # Use `path.normalize` here in case items in `srcs` contains relative paths.
    return {paths.normalize(prefix + dest): src for dest, src in srcs.items()}

def create_python_needed_coverage_info(
        label: Label,
        base_module: [None, str],
        srcs: list[str]) -> PythonNeededCoverageInfo:
    prefix = dest_prefix(label, base_module)
    return PythonNeededCoverageInfo(
        modules = {src: prefix + src for src in srcs},
    )

def create_python_library_info(
        actions: AnalysisActions,
        label: Label,
        native_deps: NativeDepsInfoTSet,
        is_native_dep: bool,
        srcs: [ManifestInfo, None] = None,
        src_types: [ManifestInfo, None] = None,
        bytecode: [dict[PycInvalidationMode, ManifestInfo], None] = None,
        default_resources: [(ManifestInfo, list[ArgLike]), None] = None,
        standalone_resources: [(ManifestInfo, list[ArgLike]), None] = None,
        extensions: [dict[str, LinkedObject], None] = None,
        deps: list[PythonLibraryInfo] = [],
        shared_libraries: list[SharedLibraryInfo] = [],
        extension_shared_libraries: list[SharedLibraryInfo] = []):
    """
    Create a `PythonLibraryInfo` for a set of sources and deps

    Arguments:
        label: The label for the `python_library`. Used for errors, and to construct
               the path for each source file
        srcs: A dictionary of {relative destination path: source file}.
        resources: A dictionary of {relative destination path: source file}.
        prebuilt_libraries: Prebuilt python libraries to include.
        deps: A list of `PythonLibraryInfo` objects from dependencies. These are merged
              into the resulting `PythonLibraryInfo`, as python needs all files present
              in the end
    Return:
        A fully merged `PythonLibraryInfo` provider, or fails if deps and/or srcs
        have destination paths that collide.
    """

    manifests = PythonLibraryManifests(
        label = label,
        srcs = srcs,
        src_types = src_types,
        default_resources = default_resources,
        standalone_resources = standalone_resources,
        bytecode = bytecode,
        extensions = extensions,
    )

    new_shared_libraries = merge_shared_libraries(
        actions,
        deps = shared_libraries + [dep.shared_libraries for dep in deps],
    )

    new_extension_shared_libraries = merge_shared_libraries(
        actions,
        deps = extension_shared_libraries + [dep.extension_shared_libraries for dep in deps],
    )

    return PythonLibraryInfo(
        manifests = actions.tset(PythonLibraryManifestsTSet, value = manifests, children = [dep.manifests for dep in deps]),
        shared_libraries = new_shared_libraries,
        extension_shared_libraries = new_extension_shared_libraries,
        is_native_dep = is_native_dep,
        native_deps = native_deps,
    )

def gather_dep_libraries(
        raw_deps: list[Dependency],
        resolve_versioned_deps: bool = True) -> (list[PythonLibraryInfo], list[SharedLibraryInfo]):
    """
    Takes a list of raw dependencies, and partitions them into python_library / shared library providers.
    If resolve_versions is True, it also collects versioned_library dependencies and uses their default version. Otherwise these are skipped, and should be handled elsewhere.
    """
    deps = []
    shared_libraries = []
    for dep in raw_deps:
        if resolve_versioned_deps and VersionedDependenciesInfo in dep:
            vdeps, vshlibs = gather_dep_libraries(resolve_versions(dep[VersionedDependenciesInfo], {}))
            deps.extend(vdeps)
            shared_libraries.extend(vshlibs)

        if PythonLibraryInfo in dep:
            deps.append(dep[PythonLibraryInfo])
        elif SharedLibraryInfo in dep:
            shared_libraries.append(dep[SharedLibraryInfo])
        else:
            # TODO(nmj): This is disabled for the moment because of:
            #                 - the 'genrule-hack' rules that are added as deps
            #                   on third-party whls. Not quite sure what's up
            #                   there, but shouldn't be necessary on v2.
            #                   (e.g. fbsource//third-party/pypi/zstandard:0.12.0-genrule-hack)
            #fail("Dependency {} is neither a python_library, nor a prebuilt_python_library".format(dep.label))
            pass
    return (deps, shared_libraries)

def _exclude_deps_from_omnibus(
        ctx: AnalysisContext,
        srcs: dict[str, Artifact]) -> bool:
    # User-specified parameter.
    if ctx.attrs.exclude_deps_from_merged_linking:
        return True

    # In some cases, Python library rules package prebuilt native extensions,
    # in which case, we can't support library merging (since we can't re-link
    # these extensions against new libraries).
    for src in srcs:
        # TODO(agallagher): Ideally, we'd prevent sources with these suffixes
        # and requires specifying them another way to make this easier to detect.
        if paths.split_extension(src)[1] in (".so", ".dll", ".pyd"):
            return True

    return False

def _attr_srcs(ctx: AnalysisContext) -> dict[str, Artifact]:
    python_platform = ctx.attrs._python_toolchain[PythonPlatformInfo]
    cxx_toolchain = ctx.attrs._cxx_toolchain
    all_srcs = {}
    all_srcs.update(from_named_set(ctx.attrs.srcs))
    for srcs in get_platform_attr(python_platform, cxx_toolchain, ctx.attrs.platform_srcs):
        all_srcs.update(from_named_set(srcs))
    return all_srcs

def _attr_resources(ctx: AnalysisContext) -> dict[str, Artifact | Dependency]:
    python_platform = ctx.attrs._python_toolchain[PythonPlatformInfo]
    cxx_toolchain = ctx.attrs._cxx_toolchain
    all_resources = {}
    all_resources.update(from_named_set(ctx.attrs.resources))

    # `python_binary` doesn't have `platform_resources`
    platform_resources = getattr(ctx.attrs, "platform_resources", [])
    for resources in get_platform_attr(python_platform, cxx_toolchain, platform_resources):
        all_resources.update(from_named_set(resources))
    return all_resources

def py_attr_resources(ctx: AnalysisContext) -> (dict[str, ArtifactOutputs], dict[str, ArtifactOutputs]):
    """
    Return the resources provided by this rule, as a map of resource name to
    a tuple of the resource artifact and any "other" outputs exposed by it.
    """
    resources = _attr_resources(ctx)
    standalone_artifacts = {}
    for key, value in resources.items():
        resource = value
        if not isinstance(value, Artifact) and DefaultInfo in value:
            if "standalone" in value[DefaultInfo].sub_targets:
                resource = value[DefaultInfo].sub_targets["standalone"][DefaultInfo].default_outputs[0]
        standalone_artifacts[key] = resource
    standalone_resources = unpack_artifact_map(standalone_artifacts)
    default_resources = unpack_artifact_map(resources)

    return default_resources, standalone_resources

def py_resources(
        ctx: AnalysisContext,
        resources: dict[str, ArtifactOutputs],
        suffix: str = "") -> (ManifestInfo, list[ArgLike]):
    """
    Generate a manifest to wrap this rules resources.
    """
    d = {name: resource.default_output for name, resource in resources.items()}
    hidden = []
    for name, resource in resources.items():
        for o in resource.nondebug_runtime_files:
            # HACK: this is a heuristic to detect shared libs emitted from cpp_binary rules.
            if (isinstance(o, Artifact) and
                o.basename == (
                    shared_libs_symlink_tree_name(
                        resource.default_output.short_path
                            .removesuffix("-prebolt")
                            .removesuffix("-stamped"),
                    )
                )):
                # Package the binary's shared libs next to the binary
                # (the path is stored in RPATH relative to the binary).
                d[paths.join(paths.dirname(name), o.basename)] = o
            else:
                hidden.append(o)
    manifest = create_manifest_for_source_map(ctx, "resources{}".format(suffix), d)
    return manifest, dedupe(hidden)

def _src_types(srcs: dict[str, Artifact], type_stubs: dict[str, Artifact]) -> dict[str, Artifact]:
    src_types = {}

    # First, add all `.py` files.
    for name, src in srcs.items():
        _, ext = paths.split_extension(name)
        if ext == ".py" or ext == ".pyi":
            src_types[name] = src

    # Override sources which have a corresponding type stub.
    for name, src in type_stubs.items():
        base, ext = paths.split_extension(name)
        expect(ext == ".pyi", "type stubs must have `.pyi` suffix: {}", name)
        src_types.pop(base + ".py", None)
        src_types[name] = src

    return src_types

def python_library_impl(ctx: AnalysisContext) -> list[Provider]:
    # Versioned params should be intercepted and converted away via the stub.
    expect(not ctx.attrs.versioned_srcs)
    expect(not ctx.attrs.versioned_resources)

    python_platform = ctx.attrs._python_toolchain[PythonPlatformInfo]
    cxx_toolchain = ctx.attrs._cxx_toolchain

    providers = []
    sub_targets = {}

    srcs = _attr_srcs(ctx)
    qualified_srcs = qualify_srcs(ctx.label, ctx.attrs.base_module, srcs)
    default_resources_map, standalone_resources_map = py_attr_resources(ctx)
    standalone_resources = qualify_srcs(ctx.label, ctx.attrs.base_module, standalone_resources_map)
    default_resources = qualify_srcs(ctx.label, ctx.attrs.base_module, default_resources_map)
    type_stubs = qualify_srcs(ctx.label, ctx.attrs.base_module, from_named_set(ctx.attrs.type_stubs))
    src_types = _src_types(qualified_srcs, type_stubs)

    src_manifest = create_manifest_for_source_map(ctx, "srcs", qualified_srcs) if qualified_srcs else None
    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    src_type_manifest = create_manifest_for_source_map(ctx, "type_stubs", src_types) if src_types else None

    # TODO(T230857912) enable pyc compilation for 3.14
    # Compile bytecode.
    bytecode = None
    py_version = ctx.attrs._python_toolchain[PythonToolchainInfo].version
    if src_manifest != None and (py_version == None or "3.14" not in py_version):
        bytecode = compile_manifests(ctx, [src_manifest])
        sub_targets["compile"] = [DefaultInfo(default_output = bytecode[PycInvalidationMode("unchecked_hash")].artifacts[0][0])]
        sub_targets["src-manifest"] = [DefaultInfo(default_output = src_manifest.manifest, other_outputs = [a for a, _ in src_manifest.artifacts])]

    raw_deps = ctx.attrs.deps
    raw_deps.extend(flatten(
        get_platform_attr(python_platform, cxx_toolchain, ctx.attrs.platform_deps),
    ))
    default_resource_manifest = py_resources(ctx, default_resources) if default_resources else None
    standalone_resource_manifest = py_resources(ctx, standalone_resources, "_standalone") if standalone_resources else None
    deps, shared_libraries = gather_dep_libraries(raw_deps, resolve_versioned_deps = False)
    providers.append(gather_versioned_dependencies(raw_deps))

    native_deps = merge_native_deps(ctx, raw_deps)

    library_info = create_python_library_info(
        ctx.actions,
        ctx.label,
        srcs = src_manifest,
        src_types = src_type_manifest,
        default_resources = default_resource_manifest,
        standalone_resources = standalone_resource_manifest,
        bytecode = bytecode,
        deps = deps,
        shared_libraries = shared_libraries,
        native_deps = native_deps,
        is_native_dep = False,
    )
    providers.append(library_info)

    providers.append(
        create_unix_env_info(
            actions = ctx.actions,
            env = UnixEnv(
                label = ctx.label,
                python_libs = [library_info],
            ),
            deps = raw_deps,
        ),
    )

    # Allow third-party-build rules to depend on Python rules.
    tp_project = project_from_label(ctx.label)
    tp_prefix = prefix_from_label(ctx.label)
    providers.append(
        third_party_build_info(
            actions = ctx.actions,
            build = ThirdPartyBuild(
                # TODO(agallagher): Figure out a way to get a unique name?
                project = tp_project,
                prefix = tp_prefix,
                root = create_third_party_build_root(
                    ctx = ctx,
                    # TODO(agallagher): use constraints to get py version.
                    manifests = (
                        [("lib/python", src_manifest)] if src_manifest != None else []
                    ) + (
                        [("lib/python", default_resource_manifest[0])] if default_resource_manifest != None else []
                    ),
                ),
                manifest = ctx.actions.write_json(
                    "third_party_build_manifest.json",
                    dict(
                        project = tp_project,
                        prefix = tp_prefix,
                        py_lib_paths = ["lib/python"],
                    ),
                ),
            ),
            deps = raw_deps,
        ),
    )

    providers.append(create_python_needed_coverage_info(ctx.label, ctx.attrs.base_module, srcs.keys()))

    # Source DBs.
    sub_targets["source-db-no-deps"] = [create_source_db_no_deps(ctx, src_types), create_python_source_db_info(library_info.manifests)]

    # Type check
    type_checker = python_toolchain.type_checker
    if type_checker != None:
        sub_targets["typecheck"] = [
            create_per_target_type_check(
                ctx,
                type_checker,
                src_type_manifest,
                deps,
                typeshed = python_toolchain.typeshed_stubs,
                py_version = ctx.attrs.py_version_for_type_checking,
                typing_enabled = ctx.attrs.typing,
                sharding_enabled = ctx.attrs.shard_typing,
            ),
        ]

    providers.append(DefaultInfo(sub_targets = sub_targets))

    # Create, augment and provide the linkable graph.
    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            # Add in any potential native root targets from our first-order deps.
            roots = get_roots(raw_deps),
            # Exclude preloaded deps from omnibus linking, to prevent preloading
            # the monolithic omnibus library.
            excluded = get_excluded(
                deps = (
                    (raw_deps if _exclude_deps_from_omnibus(ctx, qualified_srcs) else []) +
                    # We also need to exclude deps that can't be re-linked, via
                    # the `LinkableRootInfo` provider (i.e. `prebuilt_cxx_library_group`).
                    [d for d in raw_deps if LinkableRootInfo not in d]
                ),
            ),
        ),
        deps = raw_deps,
    )
    providers.append(linkable_graph)

    # Link info for native python
    providers.append(
        merge_cxx_extension_info(
            ctx.actions,
            raw_deps,
            shared_deps = raw_deps,
        ),
    )

    # C++ resources.
    providers.append(ResourceInfo(resources = gather_resources(
        label = ctx.label,
        deps = raw_deps,
    )))

    return providers
