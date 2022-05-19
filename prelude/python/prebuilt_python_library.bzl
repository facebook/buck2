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
    "@fbcode//buck2/prelude/linking:linkable_graph.bzl",
    "create_merged_linkable_graph",
)
load(":compile.bzl", "compile_manifests")
load(":manifest.bzl", "create_manifest_for_source_dir")
load(
    ":python_library.bzl",
    "create_python_library_info",
    "gather_dep_libraries",
)

def prebuilt_python_library_impl(ctx: "context") -> ["provider"]:
    providers = []

    # Extract prebuilt wheel and wrap in python library provider.
    # TODO(nmj): Make sure all attrs are used if necessary, esp compile
    extracted_src = ctx.actions.declare_output("{}_extracted".format(ctx.label.name))
    ctx.actions.run([ctx.attr._extract[RunInfo], ctx.attr.binary_src, "--output", extracted_src.as_output()], category = "py_extract_prebuilt_library")
    deps, shared_deps = gather_dep_libraries([ctx.attr.deps])
    src_manifest = create_manifest_for_source_dir(ctx, "binary_src", extracted_src)
    providers.append(create_python_library_info(
        ctx.actions,
        ctx.label,
        srcs = src_manifest,
        src_types = src_manifest,
        bytecode = create_manifest_for_source_dir(
            ctx,
            "bytecode",
            compile_manifests(ctx, [src_manifest]),
        ),
        deps = deps,
        shared_libraries = shared_deps,
    ))

    # Create, augment and provide the linkable graph.
    linkable_graph = create_merged_linkable_graph(ctx.label, ctx.attr.deps)
    add_omnibus_roots(linkable_graph, ctx.attr.deps)
    if ctx.attr.exclude_deps_from_merged_linking:
        add_omnibus_exclusions(linkable_graph, ctx.attr.deps)
    providers.append(linkable_graph)

    providers.append(DefaultInfo(default_outputs = [ctx.attr.binary_src]))

    # C++ resources.
    providers.append(CxxResourceInfo(resources = gather_cxx_resources(
        label = ctx.label,
        deps = ctx.attr.deps,
    )))

    return providers
