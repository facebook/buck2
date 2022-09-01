def _artifacts(value: "artifact"):
    return value

TransitiveOutputsTSet = transitive_set(args_projections = {"artifacts": _artifacts})

JsLibraryInfo = provider(
    fields = [
        "output",  # "artifact"
        "transitive_outputs",  # "TransitiveOutputsTSet"
    ],
)

JsBundleInfo = provider(
    fields = [
        "bundle_name",  # str.type
        # Directory containing the built JavaScript.
        "built_js",  # "artifact",
        # Source map belonging to the built JavaScript.
        "source_map",  # "artifact",
        # Directory containing the resources (or assets) used by the bundled JavaScript source code.
        "res",  # ["artifact", None]
        # Directory containing various metadata that can be used by dependent rules but are not
        # meant to be shipped with the application.
        "misc",  # "artifact"
        # Dependencies graph file associated with the built JavaScript.
        "dependencies_file",  # "artifact"
    ],
)

JsToolchainInfo = provider(
    fields = [
        "command_args_fixup",  # RunInfo.
    ],
)

def get_transitive_outputs(
        actions: "actions",
        value: ["artifact", None] = None,
        deps: [JsLibraryInfo.type] = []) -> TransitiveOutputsTSet.type:
    kwargs = {}
    if value:
        kwargs["value"] = value
    if deps:
        kwargs["children"] = [js_library_info.transitive_outputs for js_library_info in deps]

    return actions.tset(TransitiveOutputsTSet, **kwargs)
