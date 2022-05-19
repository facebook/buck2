load(":apple_resource_types.bzl", "AppleResourceDestination", "AppleResourceSpec")
load(":resource_groups.bzl", "create_resource_graph")

def apple_resource_impl(ctx: "context") -> ["provider"]:
    destination = ctx.attr.destination or "resources"
    resource_spec = AppleResourceSpec(
        files = ctx.attr.files,
        dirs = ctx.attr.dirs,
        content_dirs = ctx.attr.content_dirs,
        destination = AppleResourceDestination(destination),
        variant_files = ctx.attr.variants or [],
        named_variant_files = ctx.attr.named_variants or {},
    )
    graph = create_resource_graph(
        root = ctx.label,
        labels = ctx.attr.labels,
        deps = [],
        exported_deps = [],
        resource_spec = resource_spec,
    )
    return [DefaultInfo(), graph]
