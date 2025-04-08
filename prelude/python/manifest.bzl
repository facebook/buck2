# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:artifact_tset.bzl", "project_artifacts")
load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",
    "gen_shared_libs_action",
)
load("@prelude//utils:arglike.bzl", "ArgLike")

# Manifests are files containing information how to map sources into a package.
# The files are JSON lists with an entry per source, where each source is 3-tuple
# of relative destination path, artifact path, and a short description of the
# origin of this source (used for error messages in tooling that uses these).
ManifestInfo = record(
    # The actual manifest file (in the form of a JSON file).
    manifest = field(Artifact),
    # All artifacts that are referenced in the manifest.
    artifacts = field(list[[Artifact, ArgLike]]),
)

def _write_manifest(
        ctx: AnalysisContext,
        name: str,
        entries: list[(str, Artifact, str)]) -> Artifact:
    """
    Serialize the given source manifest entries to a JSON file.
    """
    return ctx.actions.write_json(name + ".manifest", entries)

def create_manifest_for_entries(
        ctx: AnalysisContext,
        name: str,
        entries: list[(str, Artifact, str)]) -> ManifestInfo:
    """
    Generate a source manifest for the given list of sources.
    """
    return ManifestInfo(
        manifest = _write_manifest(ctx, name, entries),
        artifacts = [(a, dest) for dest, a, _ in entries],
    )

def create_manifest_for_source_map(
        ctx: AnalysisContext,
        param: str,
        srcs: dict[str, Artifact]) -> ManifestInfo:
    """
    Generate a source manifest for the given map of sources from the given rule.
    """
    origin = "{} {}".format(ctx.label.raw_target(), param)
    return create_manifest_for_entries(
        ctx,
        param,
        [(dest, artifact, origin) for dest, artifact in srcs.items()],
    )

def get_srcs_from_manifest(
        src_manifest: [ManifestInfo, None]) -> list[Artifact]:
    return [a for (a, _) in src_manifest.artifacts] if src_manifest else []

def create_manifest_for_shared_libs(
        actions: AnalysisActions,
        name: str,
        shared_libs: list[SharedLibrary],
        lib_dir: str = "") -> ManifestInfo:
    """
    Generate a source manifest for the given list of sources.
    """
    return ManifestInfo(
        manifest = gen_shared_libs_action(
            actions = actions,
            out = name + ".manifest",
            shared_libs = shared_libs,
            gen_action = lambda actions, output, shared_libs: actions.write_json(
                output,
                [
                    (paths.join(lib_dir, soname), shlib.lib.output, name)
                    for soname, shlib in shared_libs.items()
                ],
            ),
        ),
        artifacts = [(shlib.lib.output, "") for shlib in shared_libs],
    )

def create_manifest_for_source_dir(
        ctx: AnalysisContext,
        param: str,
        extracted: Artifact,
        exclude: [str, None]) -> ManifestInfo:
    """
    Generate a source manifest for the given directory of sources from the given
    rule.
    """
    manifest = ctx.actions.declare_output(param + ".manifest")
    cmd = cmd_args(ctx.attrs._create_manifest_for_source_dir[RunInfo])
    cmd.add("--origin={}".format(ctx.label.raw_target()))
    cmd.add(cmd_args(manifest.as_output(), format = "--output={}"))
    cmd.add(extracted)
    if exclude != None:
        cmd.add("--exclude={}".format(exclude))
    ctx.actions.run(cmd, category = "py_source_manifest", identifier = param)

    # TODO: enumerate directory?
    return ManifestInfo(manifest = manifest, artifacts = [(extracted, param)])

def create_manifest_for_extensions(
        ctx: AnalysisContext,
        extensions: dict[str, (typing.Any, Label)],
        # Whether to include DWP files.
        dwp: bool = False) -> ManifestInfo:
    entries = []
    for dest, (lib, label) in extensions.items():
        entries.append((dest, lib.output, str(label.raw_target())))
        if dwp and lib.dwp != None:
            entries.append((dest + ".dwp", lib.dwp, str(label.raw_target()) + ".dwp"))
    manifest = create_manifest_for_entries(ctx, "extensions", entries)

    # Include external debug paths, even though they're not explicitly listed
    # in the manifest, as python packaging may also consume debug paths which
    # were referenced in native code.
    for name, (lib, _) in extensions.items():
        for dbginfo in project_artifacts(ctx.actions, [lib.external_debug_info]):
            manifest.artifacts.append((dbginfo, name))

    return manifest
