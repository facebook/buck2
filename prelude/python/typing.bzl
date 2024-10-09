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

DEFAULT_PY_VERSION = "3.10"

def _create_batched_type_check(
        ctx: AnalysisContext,
        executable: RunInfo,
        typeshed_manifest: Artifact,
        py_version: str | None,
        source_manifests: list[Artifact],
        dep_manifests: typing.Any,
        hidden: typing.Any) -> Artifact:
    cmd = [executable]

    # Create input configs
    input_config = {
        "dependencies": dep_manifests,
        "py_version": py_version or DEFAULT_PY_VERSION,
        "sources": source_manifests,
        "typeshed": typeshed_manifest,
    }

    input_file = ctx.actions.write_json(
        "type_check_config.json",
        input_config,
        with_inputs = True,
    )
    output_file = ctx.actions.declare_output("type_check_result.json")
    cmd.append(cmd_args(input_file))
    cmd.append(cmd_args(output_file.as_output(), format = "--output={}"))

    ctx.actions.run(cmd_args(cmd, hidden = hidden), category = "type_check")

    return output_file

def create_per_target_type_check(
        ctx: AnalysisContext,
        executable: RunInfo,
        srcs: ManifestInfo | None,
        deps: list[PythonLibraryInfo],
        typeshed: ManifestInfo | None,
        py_version: str | None,
        typing_enabled: bool) -> DefaultInfo:
    if not typing_enabled:
        # Use empty dict to signal that no type checking was performed.
        output_file = ctx.actions.write_json("type_check_result.json", {})
        return DefaultInfo(default_output = output_file)

    hidden = []

    # Dep artifacts
    dep_manifest_tset = ctx.actions.tset(
        PythonLibraryManifestsTSet,
        children = [d.manifests for d in deps],
    )
    dep_manifests = dep_manifest_tset.project_as_args("source_type_manifests")
    hidden.append(dep_manifest_tset.project_as_args("source_type_artifacts"))

    # Typeshed artifacts
    if typeshed != None:
        hidden.extend([a for a, _ in typeshed.artifacts])
        typeshed_manifest = typeshed.manifest
    else:
        typeshed_manifest = None

    # Source artifacts
    source_manifests = []
    if srcs != None:
        source_manifests.append(srcs.manifest)
        hidden.extend([a for a, _ in srcs.artifacts])

    return DefaultInfo(
        default_output = _create_batched_type_check(
            ctx,
            executable,
            typeshed_manifest,
            py_version,
            source_manifests,
            dep_manifests,
            hidden,
        ),
    )
