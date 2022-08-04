# Room for improvement: we should unify the provider type representing C++
# resources and Rust resources. Currently this implementation only supports Rust
# transitively depending on C++ resources, not C++ depending on Rust or even
# Rust depending on Rust.
#
# See https://fburl.com/diff/orni14p5 for thoughts from a buck2 dev.

load("@fbcode//buck2/prelude/cxx:resources.bzl", "CxxResourceInfo")

def gather_rust_resources(
        label: "label",
        resources: {str.type: ("artifact", ["_arglike"])} = {},
        deps: ["dependency"] = []) -> {"label": {str.type: ("artifact", ["_arglike"])}}:
    """
    Return the resources for this rule
    """

    all_resources = {}

    # Resources for self.
    if resources:
        all_resources[label] = resources

    # Merge in resources from any C++ dependencies.
    for dep in deps:
        if dep[CxxResourceInfo]:
            all_resources.update(dep[CxxResourceInfo].resources)

    return all_resources

def create_resource_db(
        name: str.type,
        actions: "actions",
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
    return actions.write_json(name, db)
