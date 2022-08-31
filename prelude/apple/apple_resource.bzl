load(":apple_resource_types.bzl", "AppleResourceDestination", "AppleResourceSpec")
load(":resource_groups.bzl", "create_resource_graph")

def apple_resource_impl(ctx: "context") -> ["provider"]:
    destination = ctx.attrs.destination or "resources"
    resource_spec = AppleResourceSpec(
        files = ctx.attrs.files,
        dirs = ctx.attrs.dirs,
        content_dirs = ctx.attrs.content_dirs,
        destination = AppleResourceDestination(destination),
        variant_files = ctx.attrs.variants or [],
        named_variant_files = ctx.attrs.named_variants or {},
        codesign_files_on_copy = ctx.attrs.codesign_on_copy,
    )
    graph = create_resource_graph(
        ctx = ctx,
        labels = ctx.attrs.labels,
        deps = [],
        exported_deps = [],
        resource_spec = resource_spec,
    )
    return [DefaultInfo(), graph]
