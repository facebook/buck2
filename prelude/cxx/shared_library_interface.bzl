# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @starlark-rust: allow_string_literals_in_type_expr

load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(":cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    ":linker.bzl",
    "get_shared_library_name",
)

def _shared_library_interface(
        ctx: AnalysisContext,
        output: str,
        identifier: str,
        shared_lib: [Artifact, "promise"]) -> Artifact:
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

_InterfaceInfo = provider(fields = [
    "artifact",  # "artifact"
])

def _anon_shared_library_interface_impl(ctx):
    output = _shared_library_interface(
        ctx = ctx,
        output = ctx.attrs.output,
        shared_lib = ctx.attrs.shared_lib,
        identifier = ctx.attrs.identifier,
    )
    return [DefaultInfo(), _InterfaceInfo(artifact = output)]

# Anonymous wrapper for `extract_symbol_names`.
_anon_shared_library_interface = rule(
    impl = _anon_shared_library_interface_impl,
    attrs = {
        "identifier": attrs.option(attrs.string(), default = None),
        "output": attrs.string(),
        "shared_lib": attrs.source(),
        "_cxx_toolchain": attrs.dep(providers = [CxxToolchainInfo]),
    },
)

def shared_library_interface(
        ctx: AnalysisContext,
        name: str,
        anonymous: bool = False,
        **kwargs) -> [Artifact, "promise_artifact"]:
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    output = get_shared_library_name(linker_info, name + "-interface")

    if anonymous:
        anon_providers = ctx.actions.anon_target(
            _anon_shared_library_interface,
            dict(
                _cxx_toolchain = ctx.attrs._cxx_toolchain,
                output = output,
                identifier = name,
                **kwargs
            ),
        )
        return ctx.actions.artifact_promise(
            anon_providers.map(lambda p: p[_InterfaceInfo].artifact),
            short_path = output,
        )
    else:
        return _shared_library_interface(
            ctx = ctx,
            output = output,
            identifier = name,
            **kwargs
        )
