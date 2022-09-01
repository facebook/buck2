# Resources for transitive deps, shared by C++ and Rust.
ResourceInfo = provider(fields = [
    # A map containing all resources from transitive dependencies.  The keys
    # are rule labels and the values are maps of resource names (the name used
    # to lookup the resource at runtime) and the actual resource artifact.
    "resources",  # {"label": {str.type, ("artifact", ["hidden"])}}
])

def gather_resources(
        label: "label",
        resources: {str.type: ("artifact", ["_arglike"])} = {},
        deps: ["dependency"] = []) -> {"label": {str.type: ("artifact", ["_arglike"])}}:
    """
    Return the resources for this rule and its transitive deps.
    """

    all_resources = {}

    # Resources for self.
    if resources:
        all_resources[label] = resources

    # Merge in resources for deps.
    for dep in deps:
        if dep[ResourceInfo]:
            all_resources.update(dep[ResourceInfo].resources)

    return all_resources

def create_resource_db(
        ctx: "context",
        name: str.type,
        binary: "artifact",
        resources: {str.type: ("artifact", ["_arglike"])}) -> "artifact":
    """
    Generate a resource DB for resources for the given binary, relativized to
    the binary's working directory.
    """

    db = {
        name: cmd_args(resource, delimiter = "").relative_to(binary, parent = 1)
        for (name, (resource, _other)) in resources.items()
    }
    return ctx.actions.write_json(name, db)
