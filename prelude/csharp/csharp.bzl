# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":toolchain.bzl", "CSharpToolchainInfo")

DotNetLibraryInfo = provider(fields = [
    "name",  # The name of the library.
    "object",  # The generated .dll artifact that will need to be linked into an .exe.
])

def csharp_library_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain = ctx.attrs._csharp_toolchain[CSharpToolchainInfo]

    # Automatically set the output dll_name to this target's name if the caller did not specify a
    # custom name.
    dll_name = "{}.dll".format(ctx.attrs.name) if not ctx.attrs.dll_name else ctx.attrs.dll_name

    # Declare that this rule will produce a dll.
    library = ctx.actions.declare_output(dll_name)

    # Create a command invoking a wrapper script that calls csc.exe to compile the .dll.
    cmd = cmd_args(toolchain.csc)

    # Add caller specified compiler flags.
    cmd.add(ctx.attrs.compiler_flags)

    # Set the output target as a .NET library.
    cmd.add("/target:library")
    cmd.add(cmd_args(
        library.as_output(),
        format = "/out:{}",
    ))

    # Add dependencies to caller specified .NET dlls and Buck targets.
    if ctx.attrs.deps:
        for dep in ctx.attrs.deps:
            if isinstance(dep, str):
                # .NET DLL dependency (eg "System.dll").
                dep_ref_arg = "/reference:{}".format(dep)
            else:
                # Buck target dependency.
                dep_ref_arg = cmd_args(dep.get(DotNetLibraryInfo).object, format = "/reference:{}")

            cmd.add(dep_ref_arg)

    # Specify the C# source code files that should be compiled into this target.
    # NOTE: This must happen after /out and /target!
    cmd.add(ctx.attrs.srcs)

    # Run the C# compiler to produce the output artifact.
    ctx.actions.run(cmd, category = "csharp_compile")

    return [
        DefaultInfo(default_output = library),
        DotNetLibraryInfo(
            name = ctx.attrs.dll_name,
            object = library,
        ),
    ]

def prebuilt_dotnet_library_impl(ctx: AnalysisContext) -> list[Provider]:
    # Prebuilt libraries are just passed through since they are already built.
    return [
        DefaultInfo(default_output = ctx.attrs.assembly),
        DotNetLibraryInfo(
            name = ctx.attrs.name,
            object = ctx.attrs.assembly,
        ),
    ]
