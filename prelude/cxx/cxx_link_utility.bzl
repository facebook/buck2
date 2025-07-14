# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:artifact_tset.bzl", "project_artifacts")
load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "CxxToolchainInfo",
    "LinkerType",
)
load("@prelude//cxx:debug.bzl", "SplitDebugMode")
load("@prelude//cxx:linker.bzl", "get_rpath_origin")
load("@prelude//cxx:target_sdk_version.bzl", "get_target_sdk_version_flags")
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkOrdering",  # @unused Used as a type
    "unpack_link_args",
    "unpack_link_args_excluding_object_files_and_lazy_archives",
    "unpack_link_args_object_files_and_lazy_archives_only",
)
load("@prelude//linking:lto.bzl", "LtoMode")
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibrary",  # @unused Used as a type
    "create_shlib_dwp_tree",
    "create_shlib_symlink_tree",
)
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load(
    "@prelude//utils:utils.bzl",
    "map_val",
)

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
    if linker_type == LinkerType("darwin"):
        flags = [
            "-Xlinker",
            "-map",
            "-Xlinker",
            linker_map,
        ]
    elif linker_type == LinkerType("gnu"):
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
    pdb_artifact = Artifact | None,
)

def get_extra_darwin_linker_flags() -> cmd_args:
    """
    Returns a cmd_args object filled with hard coded linker flags that should be used for all links with a Darwin toolchain.
    """
    return cmd_args("-Wl,-oso_prefix,.")

def make_link_args(
        ctx: AnalysisContext,
        actions: AnalysisActions,
        cxx_toolchain_info: CxxToolchainInfo,
        links: list[LinkArgs],
        output_short_path: [str, None] = None,
        link_ordering: [LinkOrdering, None] = None) -> LinkArgsOutput:
    """
    Merges LinkArgs. Returns the args, files that must be present for those
    args to work when passed to a linker, and optionally an artifact where DWO
    outputs will be written to.
    """
    args = cmd_args()
    hidden = []

    linker_info = cxx_toolchain_info.linker_info
    linker_type = linker_info.type

    link_ordering = link_ordering or map_val(LinkOrdering, linker_info.link_ordering)

    if linker_type == LinkerType("darwin"):
        # Darwin requires a target triple specified to
        # control the deployment target being linked for.
        args.add(get_target_sdk_version_flags(ctx))

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
        args.add(get_extra_darwin_linker_flags())

    pdb_artifact = None
    if linker_info.is_pdb_generated and output_short_path != None:
        pdb_filename = paths.replace_extension(output_short_path, ".pdb")
        pdb_artifact = actions.declare_output(pdb_filename)
        hidden.append(pdb_artifact.as_output())

    if linker_type == LinkerType("darwin"):
        # We order inputs partially for binary size, and partially for historical
        # reasons. It is important that lazy object file (non link-whole archives)
        # be listed last on the command line so they are not unnecessarily loaded.
        # Some applications implicitly rely upon this ordering, so it is difficult
        # to change.  Note link_ordering configuration is not respected. The
        # link_metadata_flag is also ignored as that is a flag to the GNU linker wrapper,
        # which does not apply to Darwin.
        args.add(filter(None, [unpack_link_args_excluding_object_files_and_lazy_archives(link) for link in links]))
        args.add(filter(None, [unpack_link_args_object_files_and_lazy_archives_only(link) for link in links]))
    else:
        args.add(filter(None, [unpack_link_args(link, link_ordering = link_ordering, link_metadata_flag = linker_info.link_metadata_flag) for link in links]))

    return LinkArgsOutput(
        link_args = args,
        hidden = [args] + hidden,
        pdb_artifact = pdb_artifact,
    )

def shared_libs_symlink_tree_name(output: Artifact | str) -> str:
    if isinstance(output, Artifact):
        output = output.short_path
    return "__{}__shared_libs_symlink_tree".format(output)

def _dwp_symlink_tree_name(output: Artifact) -> str:
    return "__{}__dwp_symlink_tree".format(output.short_path)

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
    dwp_symlink_tree = field(list[Artifact] | Artifact | None, None),
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

    if linker_info.type == LinkerType("darwin"):
        # ignore_artifacts as the runtime directory is not required at _link_ time
        runtime_rpath = cmd_args(ignore_artifacts = True)
        runtime_files = linker_info.sanitizer_runtime_files
        for runtime_shared_lib in runtime_files:
            # Rpath-relative dylibs have an install name of `@rpath/libName.dylib`,
            # which means we need to add the parent dir of the dylib as an rpath.
            runtime_shared_lib_dir = cmd_args(runtime_shared_lib, parent = 1)

            # The parent dir of the runtime shared lib must appear as a path
            # relative to the parent dir of the binary. `@executable_path`
            # represents the parent dir of the binary, not the binary itself.
            runtime_shared_lib_rpath = cmd_args(runtime_shared_lib_dir, format = "-Wl,-rpath,@executable_path/{}", relative_to = (output, 1))
            runtime_rpath.add(runtime_shared_lib_rpath)

        return CxxSanitizerRuntimeArguments(
            extra_link_args = [runtime_rpath],
            sanitizer_runtime_files = runtime_files,
        )

    return CxxSanitizerRuntimeArguments()

def executable_shared_lib_arguments(
        ctx: AnalysisContext,
        cxx_toolchain: CxxToolchainInfo,
        output: Artifact,
        shared_libs: list[SharedLibrary]) -> ExecutableSharedLibArguments:
    extra_link_args = []
    runtime_files = []
    shared_libs_symlink_tree = None

    # External debug info is materialized only when the executable is the output
    # of a build. Do not add to runtime_files.
    external_debug_info = project_artifacts(
        actions = ctx.actions,
        tsets = [shlib.lib.external_debug_info for shlib in shared_libs],
    )

    linker_type = cxx_toolchain.linker_info.type

    dwp_symlink_tree = None
    if len(shared_libs) > 0:
        if linker_type == LinkerType("windows"):
            shared_libs_symlink_tree = [ctx.actions.symlink_file(
                shlib.lib.output.basename,
                shlib.lib.output,
            ) for shlib in shared_libs]
            runtime_files.extend(shared_libs_symlink_tree)
            # Windows doesn't support rpath.

        else:
            shared_libs_symlink_tree = create_shlib_symlink_tree(
                actions = ctx.actions,
                out = shared_libs_symlink_tree_name(output),
                shared_libs = shared_libs,
            )
            dwp_symlink_tree = create_shlib_dwp_tree(ctx.actions, _dwp_symlink_tree_name(output), shared_libs)
            runtime_files.append(shared_libs_symlink_tree)
            rpath_reference = get_rpath_origin(linker_type)

            # We ignore_artifacts here since we don't want the symlink tree to actually be there for the link.
            rpath_arg = cmd_args(
                shared_libs_symlink_tree,
                format = "-Wl,-rpath,{}/{{}}".format(rpath_reference),
                ignore_artifacts = True,
                relative_to = (output, 1),
            )
            extra_link_args.append(rpath_arg)

    return ExecutableSharedLibArguments(
        extra_link_args = extra_link_args,
        runtime_files = runtime_files,
        external_debug_info = external_debug_info,
        shared_libs_symlink_tree = shared_libs_symlink_tree,
        dwp_symlink_tree = dwp_symlink_tree,
    )

LinkCmdParts = record(
    linker = [RunInfo, cmd_args],
    linker_flags = cmd_args,
    post_linker_flags = cmd_args,
    # linker + linker_flags, for convenience
    link_cmd = cmd_args,
)

def cxx_link_cmd_parts(toolchain: CxxToolchainInfo, executable: bool) -> LinkCmdParts:
    # `toolchain_linker_flags` can either be a list of strings, `cmd_args` or `None`,
    # so we need to do a bit more work to satisfy the type checker
    toolchain_linker_flags = toolchain.linker_info.linker_flags
    toolchain_post_linker_flags = toolchain.linker_info.post_linker_flags
    if toolchain_linker_flags == None:
        toolchain_linker_flags = cmd_args()
    elif not type(toolchain_linker_flags) == "cmd_args":
        toolchain_linker_flags = cmd_args(toolchain_linker_flags)

    if executable:
        toolchain_linker_flags = cmd_args(
            toolchain_linker_flags,
            toolchain.linker_info.executable_linker_flags,
        )

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
