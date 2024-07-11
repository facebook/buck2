load("@prelude//:artifacts.bzl", "ArtifactExt", "artifact_ext")
load("@prelude//:paths.bzl", path_utils = "paths")
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",
    "gen_shared_libs_action",
)
load(
    "@prelude//python:manifest.bzl",
    "ManifestInfo",  # @unused Used as a type
)

def create_third_party_build_root(
        ctx: AnalysisContext,
        out: str = "__third_party_build__",
        manifests: list[(str, ManifestInfo)] = [],
        shared_libs: list[SharedLibrary] = [],
        paths: list[(str, Artifact)] = []) -> ArtifactExt:
    cmd = cmd_args()
    cmd.add(ctx.attrs._create_third_party_build_root[RunInfo])

    for dst, manifest in manifests:
        cmd.add(
            "--manifest",
            dst,
            cmd_args(manifest.manifest, hidden = [a for a, _ in manifest.artifacts]),
        )

    for dst, path in paths:
        cmd.add("--path", dst, path)

    def gen(actions, output, shared_libs):
        lines = []
        for soname, shared_lib in shared_libs.items():
            lines.append(cmd_args("--path", path_utils.join("lib", soname), shared_lib.lib.output))
        return actions.write(output.as_output(), lines)

    # Add shlibs via argfsile.
    argsfile = gen_shared_libs_action(
        actions = ctx.actions,
        out = "shared_libs_args.txt",
        shared_libs = shared_libs,
        gen_action = gen,
    )
    cmd.add(cmd_args(argsfile, format = "@{}", hidden = [s.lib.output for s in shared_libs]))

    out = ctx.actions.declare_output(out, dir = True)
    cmd.add(out.as_output())

    ctx.actions.run(cmd, category = "third_party_build_root")

    return artifact_ext(out)

def project_from_label(label: Label) -> str:
    """
    Generate a unique third-party project name for the given label.
    """
    return str(label.raw_target())

def prefix_from_label(label: Label, prefix: str = "/usr/local") -> str:
    """
    Generate a unique third-party prefix for the given label.
    """
    return path_utils.join(prefix, label.cell, label.package, label.name)
