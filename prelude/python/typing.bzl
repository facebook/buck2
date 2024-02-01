# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//python:python.bzl", "PythonLibraryInfo")
load(
    ":manifest.bzl",
    "ManifestInfo",  # @unused Used as a type
)
load(":python.bzl", "PythonLibraryManifestsTSet")

def create_per_target_type_check(
        actions: AnalysisActions,
        executable: RunInfo,
        srcs: ManifestInfo | None,
        deps: list[PythonLibraryInfo],
        py_version: str | None,
        typing_enabled: bool) -> DefaultInfo:
    output_file_name = "type_check_result.json"
    if not typing_enabled:
        # Use empty dict to signal that no type checking was performed.
        output_file = actions.write_json(output_file_name, {})
    else:
        cmd = cmd_args(executable)
        cmd.add(cmd_args("check"))

        # Source artifacts
        source_manifests = []
        if srcs != None:
            source_manifests = [srcs.manifest]
            cmd.hidden([a for a, _ in srcs.artifacts])

        # Dep artifacts
        dep_manifest_tset = actions.tset(PythonLibraryManifestsTSet, children = [d.manifests for d in deps])
        dep_manifests = dep_manifest_tset.project_as_args("source_type_manifests")
        cmd.hidden(dep_manifest_tset.project_as_args("source_type_artifacts"))

        # Create input configs
        input_config = {
            "dependencies": dep_manifests,
            "py_version": py_version,
            "sources": source_manifests,
        }

        input_file = actions.write_json("type_check_config.json", input_config, with_inputs = True)
        output_file = actions.declare_output(output_file_name)
        cmd.add(cmd_args(input_file))
        cmd.add(cmd_args(output_file.as_output(), format = "--output={}"))

        actions.run(cmd, category = "type_check")

    return DefaultInfo(default_output = output_file)
