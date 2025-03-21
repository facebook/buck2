# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_toolchain_types.bzl", "LinkerInfo")
load("@prelude//utils:argfile.bzl", "at_argfile")
load("@prelude//utils:utils.bzl", "value_or")
load(":cxx_context.bzl", "get_cxx_toolchain_info")

BitcodeBundle = record(
    artifact = field(Artifact),
)

BitcodeTSet = transitive_set()

BitcodeBundleInfo = provider(fields = {
    "bitcode": provider_field(typing.Any, default = None),
    "bitcode_bundle": provider_field(typing.Any, default = None),
})

def _bundle_locally(ctx: AnalysisContext, linker_info: LinkerInfo) -> bool:
    archive_locally = linker_info.archive_objects_locally
    if hasattr(ctx.attrs, "_archive_objects_locally_override"):
        return value_or(ctx.attrs._archive_objects_locally_override, archive_locally)
    return archive_locally

def _bundle(ctx: AnalysisContext, name: str, args: cmd_args, prefer_local: bool) -> Artifact:
    llvm_link = get_cxx_toolchain_info(ctx).llvm_link
    if llvm_link == None:
        fail("Bitcode generation not supported when no LLVM linker, the `cxx_toolchain` has no `llvm_link`.")

    bundle_output = ctx.actions.declare_output(name)

    command = at_argfile(
        actions = ctx.actions,
        name = name + ".cxx_bitcode_argsfile",
        args = args,
        allow_args = True,
    )
    llvm_cmd = cmd_args(
        llvm_link,
        command,
        "-v",
        "-o",
        bundle_output.as_output(),
    )

    ctx.actions.run(llvm_cmd, category = "bitcode_bundle", identifier = name, prefer_local = prefer_local)
    return bundle_output

# Creates a static library given a list of object files.
def make_bitcode_bundle(
        ctx: AnalysisContext,
        name: str,
        objects: list[Artifact],
        ignore_native: bool = False,
        override: bool = False) -> [BitcodeBundle, None]:
    if len(objects) == 0:
        fail("no objects to archive")

    llvm_link = get_cxx_toolchain_info(ctx).llvm_link
    if llvm_link == None:
        return None

    linker_info = get_cxx_toolchain_info(ctx).linker_info

    args = cmd_args(format = "\"{}\"")
    if ignore_native:
        args.add("--ignore-non-bitcode")

    if override and len(objects) > 1:
        args.add(objects[0])
        overrides = cmd_args(objects[1:], format = "--override={}")
        args.add(overrides)
    else:
        args.add(objects)

    bundle = _bundle(ctx, name, args, _bundle_locally(ctx, linker_info))

    return BitcodeBundle(artifact = bundle)

def llvm_link_bitcode_impl(ctx: AnalysisContext) -> list[Provider]:
    llvm_link = get_cxx_toolchain_info(ctx).llvm_link
    if llvm_link == None:
        fail("llvm-link is not provided by toolchain.")

    result = make_bitcode_bundle(ctx, ctx.attrs.name, ctx.attrs.srcs)
    if result != None:
        return [DefaultInfo(default_output = result.artifact), BitcodeBundleInfo(bitcode_bundle = result)]
    else:
        return [DefaultInfo()]
