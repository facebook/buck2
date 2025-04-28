load("@prelude//:artifacts.bzl", "ArtifactExt", "artifact_ext")
load("@prelude//:paths.bzl", path_utils = "paths")
load("@prelude//cxx:cxx_context.bzl", "get_opt_cxx_toolchain_info")
load("@prelude//cxx:preprocessor.bzl", "CPreprocessorInfo")
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",
    "gen_shared_libs_action",
)
load(
    "@prelude//python:manifest.bzl",
    "ManifestInfo",  # @unused Used as a type
)
load(":providers.bzl", "ThirdPartyBuild", "ThirdPartyBuildInfo", "third_party_build_info")

def project_from_label(label: Label) -> str:
    """
    Generate a unique third-party project name for the given label.
    """
    return str(label.raw_target())

def prefix_from_label(label: Label, prefix: str = "/usr/local") -> str:
    """
    Generate a unique third-party prefix for the given label.
    """
    return path_utils.join(prefix, "{}-{}".format(label.name, sha256(str(label.raw_target()))[:7]))

def _get_sh_ext(ctx: AnalysisContext) -> str | None:
    toolchain_info = get_opt_cxx_toolchain_info(ctx)
    if toolchain_info:
        return toolchain_info.linker_info.shared_library_name_format.format("")
    return None

def create_third_party_build_root(
        ctx: AnalysisContext,
        out: str = "__third_party_build__",
        manifests: list[(str, ManifestInfo)] = [],
        shared_libs: list[SharedLibrary] = [],
        cxx_headers: list[CPreprocessorInfo] = [],
        paths: list[(str, Artifact)] = []) -> ArtifactExt:
    """
    Installs components into a unix-y install dir which can by used by other
    third-party builds.
    """

    cmd = cmd_args()
    cmd.add(ctx.attrs._create_third_party_build_root[RunInfo])

    for dst, manifest in manifests:
        cmd.add(
            "--manifest",
            dst,
            cmd_args(manifest.manifest, hidden = [a for a, _ in manifest.artifacts]),
        )

    for pps in cxx_headers:
        for pp in pps.set.value:
            for hdr in pp.headers:
                cmd.add("--path", path_utils.join("include", hdr.namespace, hdr.name), hdr.artifact)

    for dst, path in paths:
        cmd.add("--path", dst, path)

    sh_ext = _get_sh_ext(ctx)

    def gen(actions, output, shared_libs):
        lines = []
        if shared_libs:
            for soname, shared_lib in shared_libs.items():
                lines.append(cmd_args("--path", path_utils.join("lib", soname), shared_lib.lib.output))

                # Linker link `-l<name>` dynamically (by default) by looking for `lib<name>.so`,
                # so make sure this exists by creating it as a symlink (to the versioned name)
                # if it doesn't already.
                if sh_ext in soname and not soname.endswith(sh_ext):
                    idx = soname.index(sh_ext)
                    link_name = soname[:idx + 3]
                    lines.append(cmd_args("--symlink", path_utils.join("lib", link_name), soname))
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

def create_third_party_build_info(
        ctx: AnalysisContext,
        project: str | None = None,
        prefix: str | None = None,
        out: str = "__third_party_build__",
        manifests: list[(str, ManifestInfo)] = [],
        shared_libs: list[SharedLibrary] = [],
        cxx_headers: list[CPreprocessorInfo] = [],
        cxx_header_dirs: list[str] = [],
        paths: list[(str, Artifact)] = [],
        deps: list[Dependency] = []) -> ThirdPartyBuildInfo:
    if project == None:
        project = project_from_label(ctx.label)
    if prefix == None:
        prefix = prefix_from_label(ctx.label)

    root = create_third_party_build_root(
        ctx = ctx,
        out = out,
        manifests = manifests,
        cxx_headers = cxx_headers,
        shared_libs = shared_libs,
        paths = paths,
    )

    sh_ext = _get_sh_ext(ctx)

    # Build manifest.
    def gen_manifest(actions, output, shared_libs):
        manifest = {}
        manifest["project"] = project
        manifest["prefix"] = prefix
        if cxx_header_dirs:
            manifest["c_include_paths"] = cxx_header_dirs
            manifest["cxx_include_paths"] = cxx_header_dirs
        if shared_libs:
            manifest["runtime_lib_paths"] = ["lib"]
            libs = []
            for soname in shared_libs:
                if sh_ext in soname:
                    lib = soname.split(sh_ext)[0].removeprefix("lib")
                    libs.append("-l{}".format(lib))
            manifest["libs"] = libs
        return actions.write_json(output.as_output(), manifest, pretty = True)

    manifest = gen_shared_libs_action(
        actions = ctx.actions,
        out = out + ".json",
        shared_libs = shared_libs,
        gen_action = gen_manifest,
    )

    return third_party_build_info(
        actions = ctx.actions,
        build = ThirdPartyBuild(
            project = project,
            prefix = prefix,
            root = root,
            manifest = manifest,
        ),
        deps = deps,
    )
