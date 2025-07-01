# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":manifest.bzl", "ManifestInfo")
load(":toolchain.bzl", "PythonToolchainInfo")

PycInvalidationMode = enum(
    "unchecked_hash",
    "checked_hash",
    # timestamp isn't supported at the moment
    # "timestamp",
)

def compile_manifests(
        ctx: AnalysisContext,
        manifests: list[ManifestInfo]) -> dict[PycInvalidationMode, ManifestInfo]:
    return {
        mode: compile_manifests_for_mode(ctx, manifests, mode)
        for mode in [PycInvalidationMode("unchecked_hash"), PycInvalidationMode("checked_hash")]
    }

def compile_manifests_for_mode(
        ctx: AnalysisContext,
        manifests: list[ManifestInfo],
        invalidation_mode: PycInvalidationMode = PycInvalidationMode("unchecked_hash")) -> ManifestInfo:
    mode = invalidation_mode.value.upper()
    output = ctx.actions.declare_output("bytecode_{}".format(mode), dir = True)
    bytecode_manifest = ctx.actions.declare_output("bytecode_{}.manifest".format(mode))
    cmd = [
        ctx.attrs._python_toolchain[PythonToolchainInfo].host_interpreter,
        ctx.attrs._python_toolchain[PythonToolchainInfo].compile,
        cmd_args(output.as_output(), format = "--output={}"),
        cmd_args(bytecode_manifest.as_output(), format = "--bytecode-manifest={}"),
        "--invalidation-mode={}".format(mode),
    ]

    env = {
        # On some platforms (e.g. linux), python hash code randomness can cause
        # the bytecode to be non-deterministic, so pin via the `PYTHONHASHSEED`
        # env var.
        "PYTHONHASHSEED": "7",
    }

    # support invalidating cached pyc compile actions by bumping the env var.
    # the value itself is meaningless, just the fact it changes is meaningful.
    # using the PYC magic number for *convenience* only
    version = ctx.attrs._python_toolchain[PythonToolchainInfo].version
    if version and "cinder" in version:
        # for Cinder, this is a workaround...
        # this action *should* use the bundled (in-repo) runtime for compilation
        # (and then the change in the Cinder codebase would be sufficient to invalidate caches)
        # currently though, the action uses the platform Cinder for PYC compilation,
        # and these are deployed in-place (no change to toolchain paths),
        # so we need to force cache invalidation when needed (e.g. for S411091)
        env["CINDER_DUMMY_PYC_CACHE_BUSTER"] = "3451"
    elif version and "3.12" in version:
        # for CPython, the magic number *shouldn't* change during the lifetime of a feature release
        # but internally we do make more signifcant changes (rarely),
        # so for those cases we support forced invalidation using this env var
        env["PYTHON312_DUMMY_PYC_CACHE_BUSTER"] = "3532"

    hidden = []
    for manifest in manifests:
        cmd.append(manifest.manifest)
        hidden.extend([a for a, _ in manifest.artifacts])
    ctx.actions.run(
        cmd_args(cmd, hidden = hidden),
        env = env,
        category = "py_compile",
        identifier = mode,
        error_handler = ctx.attrs._python_toolchain[PythonToolchainInfo].python_error_handler,
    )
    return ManifestInfo(manifest = bytecode_manifest.without_associated_artifacts(), artifacts = [(output, "bytecode")])
