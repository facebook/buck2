# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:target_sdk_version.bzl", "get_target_triple")
load("@prelude//linking:link_info.bzl", "LinkArgs", "unpack_link_args_for_tbd_creation")
load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(":cxx_toolchain_types.bzl", "CxxToolchainInfo")

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

def shared_library_interface_from_linkables(
        ctx: AnalysisContext,
        link_args: list[LinkArgs],
        shared_lib: Artifact,
        install_name: str,
        extension_safe: bool) -> Artifact:
    """
    Produces a shared library interface from the given link args, without actually linking.
    The interface is derived from the linker inputs (ie. object files, archives, etc).
    """

    shared_library_interface = ctx.actions.declare_output(shared_lib.short_path + ".tbd")
    shared_library_interface_from_linkable_generation_filelist = ctx.actions.declare_output(shared_lib.short_path + ".tbd-generation-filelist")
    interface_generation_cmd_hidden_deps = cmd_args()

    input_files = cmd_args()
    input_files.add(filter(None, [unpack_link_args_for_tbd_creation(link_arg) for link_arg in link_args]))

    interface_generation_cmd = cmd_args()
    interface_generation_cmd.add(cmd_args(hidden = input_files))
    ctx.actions.write(shared_library_interface_from_linkable_generation_filelist, input_files, allow_args = True)
    interface_generation_tool = get_cxx_toolchain_info(ctx).binary_utilities_info.custom_tools.get("llvm-tbd-gen", None)
    interface_generation_cmd.add(interface_generation_tool)
    interface_generation_cmd.add("--objects-file-list", shared_library_interface_from_linkable_generation_filelist)
    interface_generation_cmd.add("--install-name", install_name)
    interface_generation_cmd.add("--target", get_target_triple(ctx))
    interface_generation_cmd.add("-o", shared_library_interface.as_output())
    interface_generation_cmd.add("--app-extension-safe={}".format("true" if extension_safe else "false"))

    interface_generation_cmd.add(cmd_args(hidden = interface_generation_cmd_hidden_deps))
    ctx.actions.run(interface_generation_cmd, category = "generate_shared_library_interface_from_objects", identifier = shared_lib.short_path)
    return shared_library_interface
