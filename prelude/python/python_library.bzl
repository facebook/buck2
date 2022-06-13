load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo")
load(
    "@fbcode//buck2/prelude/cxx:omnibus.bzl",
    "add_omnibus_exclusions",
    "add_omnibus_roots",
)
load(
    "@fbcode//buck2/prelude/cxx:resources.bzl",
    "CxxResourceInfo",
    "gather_cxx_resources",
)
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkedObject",  # @unused Used as a type
)
load(
    "@fbcode//buck2/prelude/linking:linkable_graph.bzl",
    "create_merged_linkable_graph",
)
load("@fbcode//buck2/prelude/linking:shared_libraries.bzl", "SharedLibraryInfo", "merge_shared_libraries")
load("@fbcode//buck2/prelude/python:toolchain.bzl", "PythonPlatformInfo", "get_platform_attr")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "flatten", "from_named_set")
load(":compile.bzl", "compile_manifests")
load(
    ":manifest.bzl",
    "ManifestInfo",  # @unused Used as a type
    "create_manifest_for_source_dir",
    "create_manifest_for_source_map",
)
load(":needed_coverage.bzl", "get_python_needed_coverage_info")
load(":python.bzl", "PythonLibraryInfo", "PythonLibraryManifests", "PythonLibraryManifestsTSet")
load(":source_db.bzl", "create_source_db")

def dest_prefix(label: "label", base_module: [None, str.type]) -> str.type:
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
        label: "label",
        base_module: [None, str.type],
        srcs: {str.type: "_a"}) -> {str.type: "_a"}:
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
    return {prefix + dest: src for dest, src in srcs.items()}

def create_python_library_info(
        actions: "actions",
        label: "label",
        srcs: [ManifestInfo.type, None] = None,
        src_types: [ManifestInfo.type, None] = None,
        bytecode: [ManifestInfo.type, None] = None,
        resources: [(ManifestInfo.type, ["_arglike"]), None] = None,
        extensions: [{str.type: LinkedObject.type}, None] = None,
        deps: ["PythonLibraryInfo"] = [],
        shared_libraries: ["SharedLibraryInfo"] = []):
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
        resources = resources,
        bytecode = bytecode,
        extensions = extensions,
    )

    new_shared_libraries = merge_shared_libraries(
        actions,
        deps = shared_libraries + [dep.shared_libraries for dep in deps],
    )

    return PythonLibraryInfo(
        manifests = actions.tset(PythonLibraryManifestsTSet, value = manifests, children = [dep.manifests for dep in deps]),
        shared_libraries = new_shared_libraries,
    )

def gather_dep_libraries(raw_deps: [["dependency"]]) -> (["PythonLibraryInfo"], ["SharedLibraryInfo"]):
    """
    Takes a list of raw dependencies, and partitions them into python_library / shared library providers.
    Fails if a dependency is not one of these.
    """
    deps = []
    shared_libraries = []
    for raw in raw_deps:
        for dep in raw:
            if dep[PythonLibraryInfo]:
                deps.append(dep[PythonLibraryInfo])
            elif dep[SharedLibraryInfo]:
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
        ctx: "context",
        srcs: {str.type: "artifact"}) -> bool.type:
    # User-specified parameter.
    if ctx.attr.exclude_deps_from_merged_linking:
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

def _attr_srcs(ctx: "context") -> {str.type: "artifact"}:
    python_platform = ctx.attr._python_toolchain[PythonPlatformInfo]
    cxx_platform = ctx.attr._cxx_toolchain[CxxPlatformInfo]
    all_srcs = {}
    all_srcs.update(from_named_set(ctx.attr.srcs))
    for srcs in get_platform_attr(python_platform, cxx_platform, ctx.attr.platform_srcs):
        all_srcs.update(from_named_set(srcs))
    return all_srcs

def py_attr_resources(ctx: "context") -> {str.type: ("artifact", ["_arglike"])}:
    """
    Return the resources provided by this rule, as a map of resource name to
    a tuple of the resource artifact and any "other" outputs exposed by it.
    """

    resources = {}

    for name, resource in from_named_set(ctx.attr.resources).items():
        if type(resource) == "artifact":
            # If this is a artifact, there are no "other" artifacts.
            other = []
        else:
            # Otherwise, this is a dependency, so extract the resource and other
            # resources from the `DefaultInfo` provider.
            info = resource[DefaultInfo]
            expect(
                len(info.default_outputs) == 1,
                "expected exactly one default output from {} ({})"
                    .format(resource, info.default_outputs),
            )
            [resource] = info.default_outputs
            other = info.other_outputs
        resources[name] = (resource, other)

    return resources

def py_resources(
        ctx: "context",
        resources: {str.type: ("artifact", ["_arglike"])}) -> (ManifestInfo.type, ["_arglike"]):
    """
    Generate a manifest to wrap this rules resources.
    """
    manifest = create_manifest_for_source_map(
        ctx,
        "resources",
        {name: resource for name, (resource, _) in resources.items()},
    )
    hidden = dedupe(flatten([other for _, other in resources.values()]))
    return manifest, hidden

def _src_types(srcs: {str.type: "artifact"}, type_stubs: {str.type: "artifact"}) -> {str.type: "artifact"}:
    src_types = {}

    # First, add all `.py` files.
    src_types.update(srcs)

    # Override sources which have a corresponding type stub.
    for name, src in type_stubs.items():
        base, ext = paths.split_extension(name)
        expect(ext == ".pyi", "type stubs must have `.pyi` suffix: {}", name)
        src_types.pop(base + ".py", None)
        src_types[name] = src

    return src_types

def python_library_impl(ctx: "context") -> ["provider"]:
    # Versioned params should be intercepted and converted away via the stub.
    expect(not ctx.attr.versioned_srcs)
    expect(not ctx.attr.versioned_resources)

    python_platform = ctx.attr._python_toolchain[PythonPlatformInfo]
    cxx_platform = ctx.attr._cxx_toolchain[CxxPlatformInfo]

    providers = []
    sub_targets = {}

    srcs = qualify_srcs(ctx.label, ctx.attr.base_module, _attr_srcs(ctx))
    resources = qualify_srcs(ctx.label, ctx.attr.base_module, py_attr_resources(ctx))
    type_stubs = qualify_srcs(ctx.label, ctx.attr.base_module, from_named_set(ctx.attr.type_stubs))
    src_types = _src_types(srcs, type_stubs)

    src_manifest = create_manifest_for_source_map(ctx, "srcs", srcs) if srcs else None
    src_type_manifest = create_manifest_for_source_map(ctx, "type_stubs", src_types) if src_types else None

    # Compile bytecode.
    bytecode_manifest = None
    if src_manifest != None:
        bytecode = compile_manifests(ctx, [src_manifest])
        sub_targets["compile"] = [DefaultInfo(default_outputs = [bytecode])]
        bytecode_manifest = create_manifest_for_source_dir(ctx, "bytecode", bytecode)

    raw_deps = (
        [ctx.attr.deps] +
        get_platform_attr(python_platform, cxx_platform, ctx.attr.platform_deps)
    )
    deps, shared_libraries = gather_dep_libraries(raw_deps)
    providers.append(create_python_library_info(
        ctx.actions,
        ctx.label,
        srcs = src_manifest,
        src_types = src_type_manifest,
        resources = py_resources(ctx, resources) if resources else None,
        bytecode = bytecode_manifest,
        deps = deps,
        shared_libraries = shared_libraries,
    ))

    providers.append(get_python_needed_coverage_info(srcs.keys()))

    # Source DBs.
    sub_targets["source-db"] = [create_source_db(ctx, src_type_manifest, flatten(raw_deps))]

    providers.append(DefaultInfo(sub_targets = sub_targets))

    # Create, augment and provide the linkable graph.
    linkable_graph = create_merged_linkable_graph(ctx.label, flatten(raw_deps))
    add_omnibus_roots(linkable_graph, flatten(raw_deps))
    if _exclude_deps_from_omnibus(ctx, srcs):
        add_omnibus_exclusions(linkable_graph, flatten(raw_deps))
    providers.append(linkable_graph)

    # C++ resources.
    providers.append(CxxResourceInfo(resources = gather_cxx_resources(
        label = ctx.label,
        deps = flatten(raw_deps),
    )))

    return providers
