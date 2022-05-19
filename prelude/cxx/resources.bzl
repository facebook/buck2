# C++ resources for transitive deps.
CxxResourceInfo = provider(fields = [
    # A map containing all resources from transitive dependencies.  The keys
    # are rule labels and the values are maps of resource names (the name used
    # to lookup the resource at runtime) and the actual resource artifact.
    "resources",  # {"label": {str.type, ("artifact", ["hidden"])}}
])

def gather_cxx_resources(
        label: "label",
        resources: {str.type: ("artifact", ["_arglike"])} = {},
        deps: ["dependency"] = []) -> {"label": {str.type: ("artifact", ["_arglike"])}}:
    """
    Return the resources for this rule and it's transitive deps.
    """

    all_resources = {}

    # Resources for self.
    if resources:
        all_resources[label] = resources

    # Merge in resources for deps.
    for dep in deps:
        if dep[CxxResourceInfo]:
            all_resources.update(dep[CxxResourceInfo].resources)

    return all_resources

def create_resource_db(
        ctx: "context",
        binary: "artifact",
        resources: {str.type: ("artifact", ["_arglike"])}) -> "artifact":
    """
    Generate a resource DB for resources for the given binary, relativized to
    the binary's working directory.
    """

    lines = []
    lines.append("{")
    for idx, (name, (resource, _other)) in enumerate(resources.items()):
        fmt = "  \"{}\": \"{{}}\"".format(name)
        if idx < len(resources) - 1:
            fmt += ","

        # We relativize the resource to the binary's parent directory.
        lines.append(cmd_args(resource, format = fmt).relative_to(binary, parent = 1))
    lines.append("}")

    return ctx.actions.write(
        # The resources helper library expects to find the json file at
        # "$0.reosurces.json".
        binary.basename + ".resources.json",
        lines,
    )
