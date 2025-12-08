# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:artifacts.bzl", "ArtifactOutputs")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type

# Resources for transitive deps, shared by C++ and Rust.
ResourceInfo = provider(fields = {
    # A map containing all resources from transitive dependencies.  The keys
    # are rule labels and the values are maps of resource names (the name used
    # to lookup the resource at runtime) and the actual resource artifact.
    "resources": provider_field(dict[Label, dict[str, ArtifactOutputs]]),
})

def gather_resources(
        label: Label,
        resources: dict[str, ArtifactOutputs] = {},
        deps: list[Dependency] = []) -> dict[Label, dict[str, ArtifactOutputs]]:
    """
    Return the resources for this rule and its transitive deps.
    """

    all_resources = {}

    # Resources for self.
    if resources:
        all_resources[label] = resources

    # Merge in resources for deps.
    for dep in deps:
        if ResourceInfo in dep:
            all_resources.update(dep[ResourceInfo].resources)

    return all_resources

def create_resource_db(
        ctx: AnalysisContext,
        name: str,
        binary: Artifact,
        resources: dict[str, ArtifactOutputs]) -> (
    Artifact,
    Artifact,
    list[ArgLike],
):
    """
    Generate a resource DB (a JSON map of resource to its relative path) for resources
    for the given binary. Paths in the DB are relative to the working dir of the binary,
    and point to a local dir that contains symlinks to the resources.

    Resources are packaged in a structure like:
      binary_name                                 (main executable)
      binary_name.resources.json                  (JSON DB pointing to the resources)
      __binary_name__resources/                   (dir containing resource symlinks)
        resource_name_1/resource_file_1 -> ../../foo
        resource_name_2/resource_file_2 -> ../../bar

    Returns a 3-tuple of:
      1. The packaged resource DB JSON file
      2. Artifact for the resources dir pointed to by the JSON file
      3. List of deps for the binary (incl. the resources dir and the JSON file) that
      must be included when the binary is materialized to resources can be executed
    """

    resources_dir_mapping = {}
    packaged_resource_name_to_json = {}

    # The name of the single resources directory for this binary
    resources_dir_name = "__{}__resources".format(binary.basename)
    hidden_deps = []
    for resource_name, resource_output in resources.items():
        # Sanitize resource name to create a valid subdir
        sanitized_resource_name = resource_name.replace("/", "__")

        # Create path within resources directory: <resource_name>/<filename>
        resource_filename = resource_output.default_output.basename
        resource_subdir_path = "{}/{}".format(sanitized_resource_name, resource_filename)

        # Detect collisions to prevent silent overwrites
        if resource_subdir_path in resources_dir_mapping:
            existing_resource_out = resources_dir_mapping[resource_subdir_path]
            fail(
                ("Resource path collision: '{}' and resource '{}' both map to '{}'. " +
                 "This can happen when resource names differ only by '/' vs '__'.")
                    .format(resource_name, existing_resource_out, resource_subdir_path),
            )

        resources_dir_mapping[resource_subdir_path] = resource_output.default_output

        # Update JSON to point to the packaged location, where path is relative to the
        # binary: (i.e. ./__binary_name__resources__/<resource_name>/<filename>)
        packaged_resource_name_to_json[resource_name] = "{}/{}".format(
            resources_dir_name,
            resource_subdir_path,
        )
        hidden_deps.append(resource_output.default_output)
        hidden_deps.extend(resource_output.nondebug_runtime_files)
        hidden_deps.extend(resource_output.other_outputs)

    resources_dir = ctx.actions.symlinked_dir(
        resources_dir_name,
        resources_dir_mapping,
    )

    packaged_resources_json = ctx.actions.write_json(
        name,
        packaged_resource_name_to_json,
    )
    hidden_deps.append(resources_dir)
    hidden_deps.append(packaged_resources_json)

    return (packaged_resources_json, resources_dir, hidden_deps)
