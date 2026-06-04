# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_is_gnu",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",  # @unused Used as a type
    "dedupe_dep_metadata",
    "get_link_info",
    "truncate_dep_metadata",
)

PRE_STAMPED_SUFFIX = "-pre_stamped"

def cxx_stamp_build_info(ctx: AnalysisContext) -> bool:
    return hasattr(ctx.attrs, "_build_info") and bool(ctx.attrs._build_info) and cxx_is_gnu(ctx)

def _get_library_versions(links: list[LinkArgs] | None) -> str:
    if not links:
        return ""

    metadatas = []
    for args in links:
        if args.tset != None:
            for info in args.tset.infos.traverse():
                metadatas.extend(get_link_info(info).metadata)
        elif args.infos != None:
            for info in args.infos:
                metadatas.extend(info.metadata)

    versions = [metadata.version for metadata in truncate_dep_metadata(dedupe_dep_metadata(metadatas))]
    return ";".join(versions)

def stamp_build_info(
    ctx: AnalysisContext, obj: Artifact, stamped_output: Artifact | None = None, has_content_based_path: bool = False, links: list[LinkArgs] | None = None
) -> Artifact:
    """
    If necessary, add fb_build_info section to binary via late-stamping
    """
    if cxx_stamp_build_info(ctx):
        build_info = dict(ctx.attrs._build_info)
        build_info["late_stamping"] = True
        library_versions = _get_library_versions(links)
        if library_versions:
            build_info["library_versions"] = library_versions
        build_info_json = ctx.actions.write_json(obj.short_path + "-build-info.json", build_info, has_content_based_path = has_content_based_path)
        stem, ext = paths.split_extension(obj.short_path)
        if not stamped_output:
            name = stem.removesuffix(PRE_STAMPED_SUFFIX) if stem.endswith(PRE_STAMPED_SUFFIX) else stem + "-stamped"
            stamped_output = ctx.actions.declare_output(name + ext, has_content_based_path = has_content_based_path)

        # This can be run remotely, but it's often cheaper to do this locally for large
        # binaries, especially on CI using limited hybrid
        prefer_local = not getattr(ctx.attrs, "optimize_for_action_throughput", False)
        toolchain = get_cxx_toolchain_info(ctx)

        ctx.actions.run(
            cmd_args([
                toolchain.binary_utilities_info.objcopy,
                "--add-section",
                cmd_args(build_info_json, format = "fb_build_info={}"),
                obj,
                stamped_output.as_output(),
            ]),
            identifier = obj.short_path,
            category = "stamp_build_info",
            # This can be run remotely, but it's often cheaper to do this locally for large
            # binaries, especially on CI using limited hybrid.
            prefer_local = prefer_local,
            prefer_remote = not prefer_local,
            allow_cache_upload = toolchain.cxx_compiler_info.allow_cache_upload,
        )
        return stamped_output
    return obj
