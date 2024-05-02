# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:artifact_tset.bzl", "ArtifactTSet", "make_artifact_tset", "project_artifacts")
load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:preprocessor.bzl", "CPreprocessor", "CPreprocessorInfo")
load("@prelude//utils:arglike.bzl", "ArgLike")  # @unused Used as a type
load("@prelude//utils:lazy.bzl", "lazy")
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

def generate_exported_symbols(ctx: AnalysisContext, exported_headers: list[CHeader], exported_preprocessor: CPreprocessor, transitive_preprocessor: list[CPreprocessorInfo], target: str) -> Artifact:
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

    # We need to collect all raw_headers that belong in a public include dir
    include_dirs = ctx.attrs.public_include_directories + ctx.attrs.public_system_include_directories
    include_dirs = [d if d.endswith("/") else d + "/" for d in include_dirs]
    if len(include_dirs) > 0:
        filelist_headers.extend([
            {
                "path": h,
                "type": "public",
            }
            for h in exported_preprocessor.raw_headers
            if lazy.is_any(lambda d: h.short_path.startswith(d), include_dirs)
        ])

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
    output_file = ctx.actions.declare_output(
        paths.join("__tbd__", ctx.attrs.name + ".exported_symbols.txt"),
    )
    args = cmd_args(get_cxx_toolchain_info(ctx).linker_info.mk_shlib_intf[RunInfo])
    args.add([
        "installapi",
        "--filelist",
        filelist,
        "-o",
        output_file.as_output(),
        "--target",
        target,
    ])
    args.add(cmd_args(compiler_info.preprocessor_flags, prepend = "-Xparser"))
    args.add(cmd_args(compiler_info.compiler_flags, prepend = "-Xparser"))
    args.add(cmd_args(exported_preprocessor.args.args, prepend = "-Xparser"))
    for ppinfo in transitive_preprocessor:
        args.add(cmd_args(ppinfo.set.project_as_args("args"), prepend = "-Xparser"))
        args.add(cmd_args(ppinfo.set.project_as_args("include_dirs"), prepend = "-Xparser"))

    # We need the targets compiler flags to pick up base flags that are applied
    # in the macros instead of the toolchain for historical reasons.
    args.add(cmd_args(ctx.attrs.compiler_flags, prepend = "-Xparser"))

    ctx.actions.run(
        args,
        category = "exported_symbols",
        identifier = ctx.attrs.name,
    )

    return output_file

def generate_tbd_with_symbols(ctx: AnalysisContext, soname: str, exported_symbol_inputs: ArtifactTSet, links: list[ArgLike], target: str) -> Artifact:
    # Use arglists for the inputs, otherwise we will overflow ARGMAX
    symbol_args = project_artifacts(ctx.actions, [exported_symbol_inputs])
    input_argfile, _ = ctx.actions.write("__tbd__/" + ctx.attrs.name + ".symbols.filelist", symbol_args, allow_args = True)

    # Run the shlib interface tool with the merge command
    tbd_file = ctx.actions.declare_output(
        paths.join("__tbd__", ctx.attrs.name + ".merged.tbd"),
    )
    args = cmd_args(get_cxx_toolchain_info(ctx).linker_info.mk_shlib_intf[RunInfo])
    args.add([
        "merge",
        "-install_name",
        "@rpath/" + soname,
        "--symbols-filelist",
        input_argfile,
        "--target",
        target,
        "-o",
        tbd_file.as_output(),
    ]).hidden(symbol_args)

    # Pass through the linker args as we need to honour any flags
    # related to exported or unexported symbols.
    for link_args in links:
        args.add(cmd_args(link_args, prepend = "-Xparser"))

    ctx.actions.run(
        args,
        category = "generate_tbd",
        identifier = ctx.attrs.name,
    )
    return tbd_file

def create_shared_interface_info(ctx: AnalysisContext, symbol_artifacts: list[Artifact], deps: list[Dependency]) -> [SharedInterfaceInfo, None]:
    children = [d[SharedInterfaceInfo].interfaces for d in deps if SharedInterfaceInfo in d]
    if len(symbol_artifacts) == 0 and len(children) == 0:
        return None

    return SharedInterfaceInfo(
        interfaces = make_artifact_tset(
            actions = ctx.actions,
            label = ctx.label,
            artifacts = symbol_artifacts,
            children = children,
        ),
    )

def create_shared_interface_info_with_children(ctx: AnalysisContext, symbol_artifacts: list[Artifact], children: list[SharedInterfaceInfo]) -> [SharedInterfaceInfo, None]:
    children = [d.interfaces for d in children]
    if len(symbol_artifacts) == 0 and len(children) == 0:
        return None

    return SharedInterfaceInfo(
        interfaces = make_artifact_tset(
            actions = ctx.actions,
            label = ctx.label,
            artifacts = symbol_artifacts,
            children = children,
        ),
    )
