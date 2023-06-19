# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":manifest.bzl", "ManifestInfo")
load(":toolchain.bzl", "PythonToolchainInfo")

def compile_manifests(
        ctx: "context",
        manifests: [ManifestInfo.type],
        ignore_errors: bool.type = False) -> ("artifact", ManifestInfo.type):
    output = ctx.actions.declare_output("bytecode", dir = True)
    bytecode_manifest = ctx.actions.declare_output("bytecode.manifest")
    cmd = cmd_args(ctx.attrs._python_toolchain[PythonToolchainInfo].host_interpreter)
    cmd.add(ctx.attrs._python_toolchain[PythonToolchainInfo].compile)
    cmd.add(cmd_args(output.as_output(), format = "--output={}"))
    cmd.add(cmd_args(bytecode_manifest.as_output(), format = "--bytecode-manifest={}"))
    if ignore_errors:
        cmd.add("--ignore-errors")
    for manifest in manifests:
        cmd.add(manifest.manifest)
        cmd.hidden([a for a, _ in manifest.artifacts])
    ctx.actions.run(
        cmd,
        # On some platforms (e.g. linux), python hash code randomness can cause
        # the bytecode to be non-deterministic, so pin via the `PYTHONHASHSEED`
        # env var.
        env = {"PYTHONHASHSEED": "7"},
        category = "py_compile",
    )
    bytecode_manifest_info = ManifestInfo(manifest = bytecode_manifest, artifacts = [(output, "bytecode")])
    return output, bytecode_manifest_info
