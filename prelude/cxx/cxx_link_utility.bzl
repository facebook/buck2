# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:artifact_tset.bzl", "project_artifacts")
load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//cxx:debug.bzl", "SplitDebugMode")
load("@prelude//cxx:linker.bzl", "get_rpath_origin")
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkOrdering",  # @unused Used as a type
    "LinkedObject",  # @unused Used as a type
    "unpack_link_args",
    "unpack_link_args_filelist",
)
load("@prelude//linking:lto.bzl", "LtoMode")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type

def generates_split_debug(toolchain: CxxToolchainInfo):
    """
    Whether linking generates split debug outputs.
    """

    if toolchain.split_debug_mode == SplitDebugMode("none"):
        return False

    if toolchain.linker_info.lto_mode == LtoMode("none"):
        return False

    return True

def linker_map_args(toolchain: CxxToolchainInfo, linker_map) -> LinkArgs:
    linker_type = toolchain.linker_info.type
    if linker_type == "darwin":
        flags = [
            "-Xlinker",
            "-map",
            "-Xlinker",
            linker_map,
        ]
    elif linker_type == "gnu":
        flags = [
            "-Xlinker",
            "-Map",
            "-Xlinker",
            linker_map,
        ]
    else:
        fail("Linker type {} not supported".format(linker_type))
    return LinkArgs(flags = flags)

LinkArgsOutput = record(
    link_args = ArgLike,
    hidden = list[typing.Any],
    pdb_artifact = [Artifact, None],
    # The filelist artifact which contains the list of all object files.
    # Only present for Darwin linkers. Note that object files referenced
    # _inside_ the filelist are _not_ part of the `hidden` field above.
    # That's by design - we do not want to materialise _all_ object files
    # to inspect the filelist. Intended to be used for debugging.
    filelist = [Artifact, None],
)

def make_link_args(
        actions: AnalysisActions,
        cxx_toolchain_info: CxxToolchainInfo,
        links: list[LinkArgs],
        suffix = None,
        output_short_path: [str, None] = None,
        is_shared: [bool, None] = None,
        link_ordering: [LinkOrdering, None] = None) -> LinkArgsOutput:
    """
    Merges LinkArgs. Returns the args, files that must be present for those
    args to work when passed to a linker, and optionally an artifact where DWO
    outputs will be written to.
    """
    suffix = "" if suffix == None else "-" + suffix
    args = cmd_args()
    hidden = []

    linker_info = cxx_toolchain_info.linker_info
    linker_type = linker_info.type

    # On Apple platforms, DWARF data is contained in the object files
    # and executables contains paths to the object files (N_OSO stab).
    #
    # By default, ld64 will use absolute file paths in N_OSO entries
    # which machine-dependent executables. Such executables would not
    # be debuggable on any host apart from the host which performed
    # the linking. Instead, we want produce machine-independent
    # hermetic executables, so we need to relativize those paths.
    #
    # This is accomplished by passing the `oso-prefix` flag to ld64,
    # which will strip the provided prefix from the N_OSO paths.
    #
    # The flag accepts a special value, `.`, which means it will
    # use the current workding directory. This will make all paths
    # relative to the parent of `buck-out`.
    #
    # Because all actions in Buck2 are run from the project root
    # and `buck-out` is always inside the project root, we can
    # safely pass `.` as the `-oso_prefix` without having to
    # write a wrapper script to compute it dynamically.
    if linker_type == "darwin":
        args.add(["-Wl,-oso_prefix,."])

    pdb_artifact = None
    if linker_info.is_pdb_generated and output_short_path != None:
        pdb_filename = paths.replace_extension(output_short_path, ".pdb")
        pdb_artifact = actions.declare_output(pdb_filename)
        hidden.append(pdb_artifact.as_output())

    for link in links:
        args.add(unpack_link_args(link, is_shared, link_ordering = link_ordering))

    filelists = filter(None, [unpack_link_args_filelist(link) for link in links])
    hidden.extend(filelists)
    filelist_file = None
    if filelists:
        if linker_type == "gnu":
            fail("filelist populated for gnu linker")
        elif linker_type == "darwin":
            # On Darwin, filelist args _must_ come last as there's semantical difference
            # of the position.
            path = actions.write("filelist%s.txt" % suffix, filelists)
            args.add(["-Xlinker", "-filelist", "-Xlinker", path])
            filelist_file = path
        else:
            fail("Linker type {} not supported".format(linker_type))

    return LinkArgsOutput(
        link_args = args,
        hidden = [args] + hidden,
        pdb_artifact = pdb_artifact,
        filelist = filelist_file,
    )

def shared_libs_symlink_tree_name(output: Artifact) -> str:
    return "__{}__shared_libs_symlink_tree".format(output.short_path)

ExecutableSharedLibArguments = record(
    extra_link_args = field(list[ArgLike], []),
    # Files that must be present for the executable to run successfully. These
    # are always materialized, whether the executable is the output of a build
    # or executed as a host tool.
    runtime_files = field(list[ArgLike], []),
    # Files needed to debug the executable. These need to be materialized when
    # this executable is the output of a build, but not when it is used by other
    # rules.
    external_debug_info = field(list[TransitiveSetArgsProjection], []),
    # Optional shared libs symlink tree symlinked_dir action.
    shared_libs_symlink_tree = field(list[Artifact] | Artifact | None, None),
)

CxxSanitizerRuntimeArguments = record(
    extra_link_args = field(list[ArgLike], []),
    sanitizer_runtime_files = field(list[Artifact], []),
)

# @executable_path/Frameworks

def cxx_sanitizer_runtime_arguments(
        ctx: AnalysisContext,
        cxx_toolchain: CxxToolchainInfo,
        output: Artifact) -> CxxSanitizerRuntimeArguments:
    linker_info = cxx_toolchain.linker_info
    target_sanitizer_runtime_enabled = ctx.attrs.sanitizer_runtime_enabled if hasattr(ctx.attrs, "sanitizer_runtime_enabled") else None
    sanitizer_runtime_enabled = target_sanitizer_runtime_enabled if target_sanitizer_runtime_enabled != None else linker_info.sanitizer_runtime_enabled
    if not sanitizer_runtime_enabled:
        return CxxSanitizerRuntimeArguments()

    if not linker_info.sanitizer_runtime_files:
        fail("C++ sanitizer runtime enabled but there are no runtime files")

    if linker_info.type == "darwin":
        runtime_rpath = cmd_args()
        runtime_files = linker_info.sanitizer_runtime_files
        for runtime_shared_lib in runtime_files:
            # Rpath-relative dylibs have an install name of `@rpath/libName.dylib`,
            # which means we need to add the parent dir of the dylib as an rpath.
            runtime_shared_lib_dir = cmd_args(runtime_shared_lib).parent()

            # The parent dir of the runtime shared lib must appear as a path
            # relative to the parent dir of the binary. `@executable_path`
            # represents the parent dir of the binary, not the binary itself.
            runtime_shared_lib_rpath = cmd_args(runtime_shared_lib_dir, format = "-Wl,-rpath,@executable_path/{}").relative_to(output, parent = 1)
            runtime_rpath.add(runtime_shared_lib_rpath)

        # Ignore_artifacts() as the runtime directory is not required at _link_ time
        runtime_rpath = runtime_rpath.ignore_artifacts()
        return CxxSanitizerRuntimeArguments(
            extra_link_args = [
                runtime_rpath,
                # Add rpaths in case the binary gets bundled and the app bundle is expected to be standalone.
                # Not all transitive callers have `CxxPlatformInfo`, so just add both iOS and macOS rpaths.
                # There's no downsides to having both, except dyld would check in both locations (and it won't
                # find anything for the non-current platform).
                "-Wl,-rpath,@loader_path/Frameworks",  # iOS
                "-Wl,-rpath,@executable_path/Frameworks",  # iOS
                "-Wl,-rpath,@loader_path/../Frameworks",  # macOS
                "-Wl,-rpath,@executable_path/../Frameworks",  # macOS
            ],
            sanitizer_runtime_files = runtime_files,
        )

    return CxxSanitizerRuntimeArguments()

def executable_shared_lib_arguments(
        ctx: AnalysisContext,
        cxx_toolchain: CxxToolchainInfo,
        output: Artifact,
        shared_libs: dict[str, LinkedObject]) -> ExecutableSharedLibArguments:
    extra_link_args = []
    runtime_files = []
    shared_libs_symlink_tree = None

    # External debug info is materialized only when the executable is the output
    # of a build. Do not add to runtime_files.
    external_debug_info = project_artifacts(
        actions = ctx.actions,
        tsets = [shlib.external_debug_info for shlib in shared_libs.values()],
    )

    linker_type = cxx_toolchain.linker_info.type

    if len(shared_libs) > 0:
        if linker_type == "windows":
            shared_libs_symlink_tree = [ctx.actions.symlink_file(
                shlib.output.basename,
                shlib.output,
            ) for _, shlib in shared_libs.items()]
            runtime_files.extend(shared_libs_symlink_tree)
            # Windows doesn't support rpath.

        else:
            shared_libs_symlink_tree = ctx.actions.symlinked_dir(
                shared_libs_symlink_tree_name(output),
                {name: shlib.output for name, shlib in shared_libs.items()},
            )
            runtime_files.append(shared_libs_symlink_tree)
            rpath_reference = get_rpath_origin(linker_type)

            # We ignore_artifacts() here since we don't want the symlink tree to actually be there for the link.
            rpath_arg = cmd_args(shared_libs_symlink_tree, format = "-Wl,-rpath,{}/{{}}".format(rpath_reference)).relative_to(output, parent = 1).ignore_artifacts()
            extra_link_args.append(rpath_arg)

    return ExecutableSharedLibArguments(
        extra_link_args = extra_link_args,
        runtime_files = runtime_files,
        external_debug_info = external_debug_info,
        shared_libs_symlink_tree = shared_libs_symlink_tree,
    )

LinkCmdParts = record(
    linker = [RunInfo, cmd_args],
    linker_flags = cmd_args,
    post_linker_flags = cmd_args,
    # linker + linker_flags, for convenience
    link_cmd = cmd_args,
)

def cxx_link_cmd_parts(toolchain: CxxToolchainInfo) -> LinkCmdParts:
    # `toolchain_linker_flags` can either be a list of strings, `cmd_args` or `None`,
    # so we need to do a bit more work to satisfy the type checker
    toolchain_linker_flags = toolchain.linker_info.linker_flags
    toolchain_post_linker_flags = toolchain.linker_info.post_linker_flags
    if toolchain_linker_flags == None:
        toolchain_linker_flags = cmd_args()
    elif not type(toolchain_linker_flags) == "cmd_args":
        toolchain_linker_flags = cmd_args(toolchain_linker_flags)

    if toolchain_post_linker_flags == None:
        toolchain_post_linker_flags = cmd_args()
    elif not type(toolchain_post_linker_flags) == "cmd_args":
        toolchain_post_linker_flags = cmd_args(toolchain_post_linker_flags)

    link_cmd = cmd_args(toolchain.linker_info.linker)
    link_cmd.add(toolchain_linker_flags)

    return LinkCmdParts(
        linker = toolchain.linker_info.linker,
        linker_flags = toolchain_linker_flags,
        post_linker_flags = toolchain_post_linker_flags,
        link_cmd = link_cmd,
    )
