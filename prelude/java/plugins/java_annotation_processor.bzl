load("@fbcode//buck2/prelude/java:java_providers.bzl", "JavaLibraryInfo", "get_all_java_packaging_deps")
load("@fbcode//buck2/prelude/utils:utils.bzl", "filter_and_map_idx", "map_idx")

JavaProcessorsType = enum(
    "java_annotation_processor",
    "ksp_annotation_processor",
    "plugin",
)

JavaProcessorsInfo = provider(
    "Information about java annotation processor/ java compiler plugins and their dependencies",
    fields = [
        # Type of processor
        "type",  # "JavaProcessorsType"

        # Names of processors
        "processors",  # ["string"]

        # Java dependencies exposed to dependent targets and supposed to be used during compilation.
        "deps",  # ["artifact"]
    ],
)

AnnotationProcessorParams = record(
    processors = field(["string"]),
    params = field(["string"]),
    deps = field(["artifact"]),
)

# Every transitive java annotation processors dependency has to be included into processor classpath for AP/Java Plugin run
def derive_transitive_deps(ctx: "context", deps: ["dependency"]) -> ["artifact"]:
    for dep in map_idx(JavaLibraryInfo, deps):
        if not dep:
            fail("Dependency must have a type of `java_library` or `prebuilt_jar`. Deps: {}".format(deps))

    return [packaging_dep.jar for packaging_dep in get_all_java_packaging_deps(ctx, deps) if packaging_dep.jar]

def create_ap_params(
        ctx: "context",
        plugins: ["dependency"],
        annotation_processors: ["string"],
        annotation_processor_params: ["string"],
        annotation_processor_deps: ["dependency"]) -> [AnnotationProcessorParams.type, None]:
    # by default set APs equal to `annotation_processors` attribute
    ap_processors = annotation_processors
    ap_processor_deps = []

    # APs derived from `plugins` attribute
    for ap_plugin in filter_and_map_idx(JavaProcessorsInfo, plugins):
        if not ap_plugin:
            fail("Plugin must have a type of `java_annotation_processor` or `java_plugin`. Plugins: {}".format(plugins))
        if ap_plugin.type == JavaProcessorsType("java_annotation_processor"):
            ap_processors += ap_plugin.processors
            ap_processor_deps += ap_plugin.deps

    if not ap_processors:
        return None

    # Extend `ap_processor_deps` with java deps from `annotation_processor_deps`
    for ap_dep in map_idx(JavaLibraryInfo, annotation_processor_deps):
        if not ap_dep:
            fail("Dependency must have a type of `java_library` or `prebuilt_jar`. Deps: {}".format(annotation_processor_deps))

    # using packaging deps to have all transitive deps collected for processors classpath
    ap_processor_deps += [packaging_dep.jar for packaging_dep in get_all_java_packaging_deps(ctx, annotation_processor_deps) if packaging_dep.jar]

    return AnnotationProcessorParams(
        processors = dedupe(ap_processors),
        params = annotation_processor_params,
        deps = ap_processor_deps,
    )

def create_ksp_ap_params(plugins: ["dependency"]) -> [AnnotationProcessorParams.type, None]:
    ap_processors = []
    ap_processor_deps = []

    # APs derived from `plugins` attribute
    for ap_plugin in filter_and_map_idx(JavaProcessorsInfo, plugins):
        if not ap_plugin:
            fail("Plugin must have a type of `java_annotation_processor` or `java_plugin`. Plugins: {}".format(plugins))
        if ap_plugin.type == JavaProcessorsType("ksp_annotation_processor"):
            ap_processors += ap_plugin.processors
            ap_processor_deps += ap_plugin.deps

    if not ap_processors:
        return None

    return AnnotationProcessorParams(
        processors = dedupe(ap_processors),
        params = [],
        deps = ap_processor_deps,
    )

def _get_processor_type(processor_class: str.type) -> JavaProcessorsType.type:
    if processor_class.startswith("KSP:"):
        return JavaProcessorsType("ksp_annotation_processor")

    return JavaProcessorsType("java_annotation_processor")

def java_annotation_processor_impl(ctx: "context") -> ["provider"]:
    return [
        JavaProcessorsInfo(
            deps = derive_transitive_deps(ctx, ctx.attr.deps),
            processors = [ctx.attr.processor_class],
            type = _get_processor_type(ctx.attr.processor_class),
        ),
        DefaultInfo(default_outputs = []),
    ]
