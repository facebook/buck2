# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":toolchain.bzl", "CSharpToolchainInfo")

# Describes either a reference to a Buck .NET target or a .NET framework DLL.
DllReference = record(
    # `str` -> Path to a .NET framework DLL on the local machine.
    # `Artifact` -> Buck target dependency.
    reference = field([Artifact, str]),
)

def _args_for_dll_reference(dllref: DllReference) -> cmd_args:
    """Projects values in a `DllDepTSet` to csc.exe /reference arguments."""
    return cmd_args(dllref.reference, format = "/reference:{}")

# A transitive set of DLL references required to build a .NET library.
#
# The transitive set attribute `value` references the outputting assembly, and the children are a
# list of the dependencies required to build it.
DllDepTSet = transitive_set(
    args_projections = {
        # Projects "/reference:{}" arguments for `csc.exe`.
        "reference": _args_for_dll_reference,
    },
)

def generate_target_tset_children(deps: list[typing.Any], ctx: AnalysisContext) -> list[DllDepTSet]:
    """Convert a target's dependencies list into an array of transitive dependencies."""

    tset_children = []

    if deps:
        for dep in deps:
            if isinstance(dep, str):
                # Name of a .NET framework DLL (eg "System.Drawing.dll").
                tset_children.append(
                    ctx.actions.tset(DllDepTSet, value = DllReference(reference = dep)),
                )
            else:
                # Buck target dependency (eg "//buck/path/to:foobar").
                tset_children.append(dep.get(DotNetLibraryInfo).dll_deps)

    return tset_children

DotNetLibraryInfo = provider(
    doc = "Information about a .NET library and its dependencies",
    fields = {
        # A tset of DLLs (System or Buck targets) this library depends on. The
        # `.value` is a reference to the outputting assembly artifact, and the
        # children are the dependencies required to build it.
        "dll_deps": provider_field(DllDepTSet),
        # The output file name of the library.
        "name": provider_field(str),
        # The generated .dll artifact that will need to be linked into an .exe.
        "object": provider_field(Artifact),
    },
)

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

    # Add a `/reference:{name}` argument for each dependency.
    child_deps = generate_target_tset_children(ctx.attrs.deps, ctx)
    deps_tset = ctx.actions.tset(DllDepTSet, children = child_deps)

    cmd.add(deps_tset.project_as_args("reference"))

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
            dll_deps = ctx.actions.tset(DllDepTSet, value = DllReference(reference = library), children = child_deps),
        ),
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
