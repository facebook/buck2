# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load(":manifest.bzl", "ManifestInfo")
load(":toolchain.bzl", "PythonToolchainInfo")

PycInvalidationMode = enum(
    "unchecked_hash",
    "checked_hash",
    # timestamp isn't supported at the moment
    # "TIMESTAMP",
)

CompiledPycInfo = record(
    manifest = field(Artifact),
    output = field(Artifact),
)

_AnonCompileResult = provider(fields = {
    "result": provider_field(None | CompiledPycInfo.type, default = None),
})

def compile_manifests_for_mode(
        ctx: AnalysisContext,
        manifests: list[ManifestInfo],
        invalidation_mode: PycInvalidationMode = PycInvalidationMode("unchecked_hash"),
        anonymous: bool = True) -> ManifestInfo:
    input_manifests = []
    input_artifacts = []
    for manifest in manifests:
        input_manifests.append(manifest.manifest)
        input_artifacts.extend([a for a, _ in manifest.artifacts])
    if anonymous:
        anon_providers = ctx.actions.anon_target(
            _anon_compile_rule,
            dict(
                _python_toolchain = ctx.attrs._python_toolchain,
                pyc_invalidation_mode = invalidation_mode.value,
                input_manifests = input_manifests,
                input_artifacts = input_artifacts,
            ),
        )
        manifest = anon_providers.artifact("manifest")
        output = anon_providers.artifact("output")
        return ManifestInfo(manifest = manifest, artifacts = [(output, "bytecode")])
    compiled = _compile(ctx, invalidation_mode, input_manifests, input_artifacts)
    return ManifestInfo(manifest = compiled.manifest, artifacts = [(compiled.output, "bytecode")])

def _compile(
        ctx: AnalysisContext,
        invalidation_mode: PycInvalidationMode,
        input_manifests: list[Artifact],
        input_artifacts: list[[Artifact, ArgLike]]) -> CompiledPycInfo:
    output = ctx.actions.declare_output("bytecode_{}".format(invalidation_mode.value), dir = True)
    bytecode_manifest = ctx.actions.declare_output("bytecode_{}.manifest".format(invalidation_mode.value))
    cmd = cmd_args(ctx.attrs._python_toolchain[PythonToolchainInfo].host_interpreter)
    cmd.add(ctx.attrs._python_toolchain[PythonToolchainInfo].compile)
    cmd.add(cmd_args(output.as_output(), format = "--output={}"))
    cmd.add(cmd_args(bytecode_manifest.as_output(), format = "--bytecode-manifest={}"))
    cmd.add("--invalidation-mode={}".format(invalidation_mode.value.upper()))
    cmd.add(input_manifests)
    cmd.hidden(input_artifacts)
    ctx.actions.run(
        cmd,
        # On some platforms (e.g. linux), python hash code randomness can cause
        # the bytecode to be non-deterministic, so pin via the `PYTHONHASHSEED`
        # env var.
        env = {"PYTHONHASHSEED": "7"},
        category = "py_compile",
        identifier = invalidation_mode.value,
    )
    return CompiledPycInfo(manifest = bytecode_manifest, output = output)

ANON_COMPILE_ATTRS = {
    "input_artifacts": attrs.list(attrs.one_of(attrs.source(), attrs.arg())),
    "input_manifests": attrs.list(attrs.source()),
    "pyc_invalidation_mode": attrs.enum(PycInvalidationMode.values()),
    "_python_toolchain": attrs.dep(providers = [PythonToolchainInfo]),
}

def _anon_compile_impl(ctx: AnalysisContext) -> list[Provider]:
    manifest = _compile(ctx, PycInvalidationMode(ctx.attrs.pyc_invalidation_mode), ctx.attrs.input_manifests, ctx.attrs.input_artifacts)
    return [DefaultInfo(), _AnonCompileResult(result = manifest)]

_anon_compile_rule = anon_rule(
    impl = _anon_compile_impl,
    attrs = ANON_COMPILE_ATTRS,
    artifact_promise_mappings = {
        "manifest": lambda p: p[_AnonCompileResult].result.manifest,
        "output": lambda p: p[_AnonCompileResult].result.output,
    },
)
