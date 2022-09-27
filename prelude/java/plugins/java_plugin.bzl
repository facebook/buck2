load("@prelude//java:java_providers.bzl", "JavaPackagingDepTSet")
load(
    "@prelude//java/plugins:java_annotation_processor.bzl",
    "JavaProcessorsInfo",
    "JavaProcessorsType",
    "derive_transitive_deps",
)
load("@prelude//utils:utils.bzl", "filter_and_map_idx")

PluginParams = record(
    processors = field(["string"]),
    args = field({
        str.type: "cmd_args",
    }),
    deps = field(["JavaPackagingDepTSet", None]),
)

def create_plugin_params(ctx: "context", plugins: ["dependency"]) -> [PluginParams.type, None]:
    processors = []
    plugin_deps = []

    # Compiler plugin derived from `plugins` attribute
    for plugin in filter_and_map_idx(JavaProcessorsInfo, plugins):
        if plugin.type == JavaProcessorsType("plugin"):
            if len(plugin.processors) > 1:
                fail("Only 1 java compiler plugin is expected. But received: {}".format(plugin.processors))
            processors.append(plugin.processors[0])
            if plugin.deps:
                plugin_deps.append(plugin.deps)

    if not processors:
        return None

    return PluginParams(
        processors = dedupe(processors),
        deps = ctx.actions.tset(JavaPackagingDepTSet, children = plugin_deps) if plugin_deps else None,
        args = {},
    )

def java_plugin_impl(ctx: "context") -> ["provider"]:
    return [
        JavaProcessorsInfo(
            deps = derive_transitive_deps(ctx, ctx.attrs.deps),
            processors = [ctx.attrs.plugin_name],
            type = JavaProcessorsType("plugin"),
        ),
        DefaultInfo(default_outputs = []),
    ]
