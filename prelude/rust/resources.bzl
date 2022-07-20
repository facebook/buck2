# Room for improvement: this is where you'd handle resources exposed by
# (potentially transitive) dependencies, similar to C++.
# See https://fburl.com/diff/orni14p5 for more.

def gather_rust_resources(
        label: "label",
        resources: {str.type: ("artifact", ["_arglike"])} = {}) -> {"label": {str.type: ("artifact", ["_arglike"])}}:
    """
    Return the resources for this rule
    """

    all_resources = {}

    # Resources for self.
    if resources:
        all_resources[label] = resources

    return all_resources

def create_resource_db(
        ctx: "context",
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
    return ctx.actions.write_json(
        binary.basename + ".resources.json",
        db,
    )
