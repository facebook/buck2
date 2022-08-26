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
    deps = field(["artifact"]),
)

def create_plugin_params(plugins: ["dependency"]) -> [PluginParams.type, None]:
    processors = []
    plugin_deps = []

    # Compiler plugin derived from `plugins` attribute
    for plugin in filter_and_map_idx(JavaProcessorsInfo, plugins):
        if plugin.type == JavaProcessorsType("plugin"):
            if len(plugin.processors) > 1:
                fail("Only 1 java compiler plugin is expected. But received: {}".format(plugin.processors))
            processors.append(plugin.processors[0])
            plugin_deps = plugin_deps + plugin.deps

    if not processors:
        return None

    return PluginParams(
        processors = dedupe(processors),
        deps = dedupe(plugin_deps),
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
