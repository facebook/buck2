# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//python:python.bzl", "PythonLibraryInfo")
load(
    ":manifest.bzl",
    "ManifestInfo",  # @unused Used as a type
    "create_manifest_for_entries",
)
load(":python.bzl", "PythonLibraryManifestsTSet")

DEFAULT_PY_VERSION = "3.10"

# Best-effort guess on what the host sys.platform is
def get_default_sys_platform() -> str | None:
    os_info = host_info().os
    if os_info.is_linux:
        return "linux"
    elif os_info.is_macos:
        return "darwin"
    elif os_info.is_windows:
        return "win32"
    return None

def _create_all_dep_manifests(
        source_manifests: list[Artifact],
        dep_manifests: typing.Any) -> typing.Any:
    return source_manifests + [dep for dep in dep_manifests.traverse() if dep]

def _create_batched_type_check(
        ctx: AnalysisContext,
        executable: RunInfo,
        typeshed_manifest: Artifact,
        py_version: str | None,
        source_manifests: list[Artifact],
        dep_manifests: typing.Any,
        hidden: typing.Any,
        is_sharded_fallback: bool) -> Artifact:
    # Create input configs
    input_config = {
        "dependencies": dep_manifests,
        "py_version": py_version or DEFAULT_PY_VERSION,
        "sources": source_manifests,
        "system_platform": get_default_sys_platform(),
        "typeshed": typeshed_manifest,
    }

    file_suffix = "_sharding_fallback" if is_sharded_fallback else ""

    input_file = ctx.actions.write_json(
        "type_check_config{}.json".format(file_suffix),
        input_config,
        with_inputs = True,
    )
    output_file = ctx.actions.declare_output("type_check_result{}.json".format(file_suffix))
    cmd = cmd_args(
        executable,
        input_file,
        "--output",
        output_file.as_output(),
        hidden = hidden,
    )

    identifier = "_sharding_fallback" if is_sharded_fallback else "batched"
    ctx.actions.run(cmd, category = "type_check", identifier = identifier)

    return output_file

def _create_sharded_type_check(
        ctx: AnalysisContext,
        executable: RunInfo,
        typeshed_manifest: Artifact,
        py_version: str | None,
        source_manifests: list[Artifact],
        source_artifacts: list[typing.Any],
        dep_manifests: typing.Any,
        hidden: typing.Any,
        sharding_enabled: bool | None) -> dict[str, list[DefaultInfo]]:
    if not sharding_enabled:
        return {
            "shard_default": [DefaultInfo(default_output = _create_batched_type_check(
                ctx,
                executable,
                typeshed_manifest,
                py_version,
                source_manifests,
                dep_manifests,
                hidden,
                True,
            ))],
        }

    commands = {}
    output_files = []
    all_dep_manifests = _create_all_dep_manifests(source_manifests, dep_manifests)
    for shard in source_artifacts:
        artifact_project_path, artifact_cell_path = shard
        sanitized_path = artifact_cell_path.replace("/", "+")
        shard_name = "shard_{}".format(sanitized_path)

        shard_manifest = create_manifest_for_entries(
            ctx,
            shard_name,
            [(artifact_cell_path, artifact_project_path, "sharding")],
        )

        # Create input configs
        input_config = {
            "dependencies": all_dep_manifests,
            "py_version": py_version or DEFAULT_PY_VERSION,
            "sources": [shard_manifest.manifest],
            "system_platform": get_default_sys_platform(),
            "typeshed": typeshed_manifest,
        }

        input_file_name = "type_checking_config_shard_{}.json".format(sanitized_path)
        output_file_name = "type_check_result_shard_{}.json".format(sanitized_path)
        input_file = ctx.actions.write_json(
            input_file_name,
            input_config,
            with_inputs = True,
        )
        output_file = ctx.actions.declare_output(output_file_name)
        output_files.append(output_file)

        cmd = cmd_args(
            executable,
            input_file,
            "--output",
            output_file.as_output(),
            hidden = hidden,
        )
        ctx.actions.run(cmd, category = "type_check", identifier = shard_name)
        commands[shard_name] = [DefaultInfo(default_output = output_file)]

    return commands

def create_per_target_type_check(
        ctx: AnalysisContext,
        executable: RunInfo,
        srcs: ManifestInfo | None,
        deps: list[PythonLibraryInfo],
        typeshed: ManifestInfo | None,
        py_version: str | None,
        typing_enabled: bool,
        sharding_enabled: bool | None = None) -> DefaultInfo:
    if not typing_enabled:
        # Use empty dict to signal that no type checking was performed.
        output_file = ctx.actions.write_json("type_check_result.json", {})
        sharded_output_file = ctx.actions.write_json(
            "sharded_type_check_result.json",
            {},
        )
        return DefaultInfo(
            default_output = output_file,
            sub_targets = {
                "shard_default": [DefaultInfo(default_output = sharded_output_file)],
            },
        )

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
    source_artifacts = []
    if srcs != None:
        source_manifests = [srcs.manifest]
        source_artifacts = srcs.artifacts
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
            False,
        ),
        sub_targets = _create_sharded_type_check(
            ctx,
            executable,
            typeshed_manifest,
            py_version,
            source_manifests,
            source_artifacts,
            dep_manifests,
            hidden,
            sharding_enabled,
        ),
    )
