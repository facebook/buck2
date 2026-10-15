# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils:argfile.bzl", "at_argfile")
load(":csharp_providers.bzl", "DllDepTSet", "DllReference", "DotNetLibraryInfo", "generate_target_tset_children")
load(":toolchain.bzl", "CSharpToolchainInfo")

def _csharp_library_or_exe_artifact(ctx: AnalysisContext, library_or_exe_name: str, target_type: str) -> (Artifact, list[DllDepTSet]):
    toolchain = ctx.attrs._csharp_toolchain[CSharpToolchainInfo]

    # Declare that this rule will produce a dll or exe.
    library_or_exe_artifact = ctx.actions.declare_output(library_or_exe_name, has_content_based_path = False)

    # Create a command invoking a wrapper script that calls csc.exe to compile the .dll or the .exe.
    cmd = [toolchain.csc]

    # Arguments written to a response file to avoid command line too long errors.
    response_file_args = []

    # Add caller specified compiler flags.
    response_file_args.append(ctx.attrs.compiler_flags)

    # Set the output target as a .NET library.
    response_file_args.append("/target:" + target_type)
    response_file_args.append(
        cmd_args(
            library_or_exe_artifact.as_output(),
            format = "/out:{}",
        )
    )

    # NOTE: These are passed directly on the command line since /noconfig is not allowed in a
    # response file.
    if ctx.attrs.add_hermetic_arguments:
        # Don't include any default .NET framework assemblies like "mscorlib" or "System" unless
        # explicitly requested with `/reference:{}`. This flag also stops injection of other
        # default compiler flags.
        cmd.append("/noconfig")

        # Don't reference mscorlib.dll unless asked for. This is required for targets that target
        # embedded platforms such as Silverlight or WASM. (Originally for Buck1 compatibility.)
        cmd.append("/nostdlib")

        # Don't search any paths for .NET libraries unless explicitly referenced with `/lib:{}`.
        cmd.append("/nosdkpath")

    # Let csc know the directory path where it can find system assemblies. This is the path
    # that is searched by `/reference:{libname}` if `libname` is just a DLL name.
    response_file_args.append(cmd_args(toolchain.framework_dirs[ctx.attrs.framework_ver], format = "/lib:{}"))

    # Add a `/reference:{name}` argument for each dependency.
    # Buck target refs should be absolute paths and system assemblies just the DLL name.
    child_deps = generate_target_tset_children(ctx.attrs.deps, ctx)
    deps_tset = ctx.actions.tset(DllDepTSet, children = child_deps)

    response_file_args.append(deps_tset.project_as_args("reference"))

    # Specify the C# source code files that should be compiled into this target.
    # NOTE: This must happen after /out and /target!
    response_file_args.append(ctx.attrs.srcs)

    cmd.append(at_argfile(
        actions = ctx.actions,
        name = "csc_args.rsp",
        args = cmd_args(response_file_args, quote = "shell"),
        allow_args = True,
    ))

    # Run the C# compiler to produce the output artifact.
    ctx.actions.run(cmd, category = "csharp_compile")

    return library_or_exe_artifact, child_deps

def csharp_library_impl(ctx: AnalysisContext) -> list[Provider]:
    # Automatically set the output dll_name to this target's name if the caller did not specify a
    # custom name.
    dll_name = "{}.dll".format(ctx.attrs.name) if not ctx.attrs.dll_name else ctx.attrs.dll_name

    library_or_exe_artifact, child_deps = _csharp_library_or_exe_artifact(ctx, dll_name, "library")

    return [
        DefaultInfo(default_output = library_or_exe_artifact),
        DotNetLibraryInfo(
            name = library_or_exe_artifact.basename,
            object = library_or_exe_artifact,
            dll_deps = ctx.actions.tset(DllDepTSet, value = DllReference(reference = library_or_exe_artifact), children = child_deps),
        ),
    ]

def csharp_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    exe_name = "{}.exe".format(ctx.attrs.name) if not ctx.attrs.exe_name else ctx.attrs.exe_name

    library_or_exe_artifact, child_deps = _csharp_library_or_exe_artifact(ctx, exe_name, "exe")

    new_runtime_files = []
    for child in child_deps:
        new_runtime_files.extend([ctx.actions.symlink_file(dll_dep.reference.basename, dll_dep.reference) for dll_dep in child.traverse()])

    return [
        DefaultInfo(default_output = library_or_exe_artifact, other_outputs = new_runtime_files),
        RunInfo(args = cmd_args(library_or_exe_artifact, hidden = new_runtime_files)),
    ]

def prebuilt_dotnet_library_impl(ctx: AnalysisContext) -> list[Provider]:
    # Prebuilt libraries are just passed through since they are already built.
    return [
        DefaultInfo(default_output = ctx.attrs.assembly),
        DotNetLibraryInfo(
            name = ctx.attrs.name,
            object = ctx.attrs.assembly,
            dll_deps = ctx.actions.tset(DllDepTSet, value = DllReference(reference = ctx.attrs.assembly)),
        ),
    ]
