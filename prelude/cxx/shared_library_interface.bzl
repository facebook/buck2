# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:artifact_tset.bzl", "ArtifactTSet", "make_artifact_tset", "project_artifacts")
load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:preprocessor.bzl", "CPreprocessor", "CPreprocessorInfo")
load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(":cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(":headers.bzl", "CHeader")

# The transitive artifacts of partial shared interface for a library.
# These need to be collected and merged to produce the final shared interface.
SharedInterfaceInfo = provider(fields = {
    "interfaces": provider_field(ArtifactTSet),
})

def _shared_library_interface(
        ctx: AnalysisContext,
        output: str,
        identifier: str,
        shared_lib: [Artifact, Promise]) -> Artifact:
    """
    Convert the given shared library into an interface used for linking.
    """
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    args = cmd_args(linker_info.mk_shlib_intf[RunInfo])
    args.add(shared_lib)
    output = ctx.actions.declare_output(output)
    args.add(output.as_output())
    ctx.actions.run(
        args,
        category = "generate_shared_library_interface",
        identifier = identifier,
    )
    return output

_InterfaceInfo = provider(fields = {
    "artifact": provider_field(typing.Any, default = None),  # "artifact"
})

def _anon_shared_library_interface_impl(ctx):
    output = _shared_library_interface(
        ctx = ctx,
        output = ctx.attrs.output,
        shared_lib = ctx.attrs.shared_lib,
        identifier = ctx.attrs.identifier,
    )
    return [DefaultInfo(), _InterfaceInfo(artifact = output)]

# Anonymous wrapper for `extract_symbol_names`.
_anon_shared_library_interface = anon_rule(
    impl = _anon_shared_library_interface_impl,
    attrs = {
        "identifier": attrs.option(attrs.string(), default = None),
        "output": attrs.string(),
        "shared_lib": attrs.source(),
        "_cxx_toolchain": attrs.dep(providers = [CxxToolchainInfo]),
    },
    artifact_promise_mappings = {
        "shared_library_interface": lambda p: p[_InterfaceInfo].artifact,
    },
)

def shared_library_interface(
        ctx: AnalysisContext,
        shared_lib: Artifact,
        anonymous: bool = False) -> Artifact:
    output = paths.join("__shlib_intfs__", shared_lib.short_path)

    if anonymous:
        shared_lib_interface_artifact = ctx.actions.anon_target(
            _anon_shared_library_interface,
            dict(
                _cxx_toolchain = ctx.attrs._cxx_toolchain,
                output = output,
                shared_lib = shared_lib,
                identifier = shared_lib.short_path,
            ),
        ).artifact("shared_library_interface")
        return ctx.actions.assert_short_path(shared_lib_interface_artifact, short_path = output)
    else:
        return _shared_library_interface(
            ctx = ctx,
            output = output,
            shared_lib = shared_lib,
            identifier = shared_lib.short_path,
        )

def create_tbd(ctx: AnalysisContext, exported_headers: list[CHeader], exported_preprocessor: CPreprocessor, transitive_preprocessor: list[CPreprocessorInfo], target: str) -> Artifact:
    # Use the c++ compiler to correctly generate c++ symbols.
    compiler_info = get_cxx_toolchain_info(ctx).cxx_compiler_info

    # Collect the exported headers for this library and create a filelist for them.
    # The exported headers are possibly hidden behind a modulemap,
    # so cannot be fetched directly from exported_preprocessor.
    filelist_headers = []
    for h in exported_headers:
        filelist_headers.append({
            "path": h.artifact,
            "type": "public",
        })
    filelist_contents = {
        "headers": filelist_headers,
        "version": "2",
    }
    filelist = ctx.actions.write_json(
        paths.join("__tbd__", ctx.attrs.name + "_exported_headers.json"),
        filelist_contents,
        with_inputs = True,
    )

    # Run the shlib interface tool with the filelist and required args
    tbd_file = ctx.actions.declare_output(
        paths.join("__tbd__", ctx.attrs.name + ".tbd"),
    )
    args = cmd_args(get_cxx_toolchain_info(ctx).linker_info.mk_shlib_intf[RunInfo])
    args.add([
        "installapi",
        cmd_args(filelist, format = "--filelist={}"),
        "-o",
        tbd_file.as_output(),
        "-ObjC++",
        "--target=" + target,
        "-install_name",
        ctx.attrs.name,
    ])
    args.add(cmd_args(compiler_info.preprocessor_flags, prepend = "-Xparser"))
    args.add(cmd_args(compiler_info.compiler_flags, prepend = "-Xparser"))
    args.add(cmd_args(exported_preprocessor.relative_args.args, prepend = "-Xparser"))
    for ppinfo in transitive_preprocessor:
        args.add(cmd_args(ppinfo.set.project_as_args("args"), prepend = "-Xparser"))

    ctx.actions.run(
        args,
        category = "generate_tbd",
        identifier = ctx.attrs.name,
    )

    return tbd_file

def merge_tbds(ctx: AnalysisContext, soname: str, tbd_set: ArtifactTSet) -> Artifact:
    # Run the shlib interface tool with the merge command
    tbd_file = ctx.actions.declare_output(
        paths.join("__tbd__", ctx.attrs.name + ".merged.tbd"),
    )
    args = cmd_args(get_cxx_toolchain_info(ctx).linker_info.mk_shlib_intf[RunInfo])
    args.add([
        "merge",
        "-install_name",
        "@rpath/" + soname,
        project_artifacts(ctx.actions, [tbd_set]),
        "-o",
        tbd_file.as_output(),
    ])
    ctx.actions.run(
        args,
        category = "merge_tbd",
        identifier = ctx.attrs.name,
    )
    return tbd_file

def create_shared_interface_info(ctx: AnalysisContext, tbd_outputs: list[Artifact], deps: list[Dependency]) -> [SharedInterfaceInfo, None]:
    children = [d[SharedInterfaceInfo].interfaces for d in deps if SharedInterfaceInfo in d]
    if len(tbd_outputs) == 0 and len(children) == 0:
        return None

    return SharedInterfaceInfo(
        interfaces = make_artifact_tset(
            actions = ctx.actions,
            label = ctx.label,
            artifacts = tbd_outputs,
            children = children,
        ),
    )
