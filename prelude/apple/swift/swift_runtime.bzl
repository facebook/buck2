# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple/swift:swift_types.bzl", "SWIFT_EXTENSION")
load("@prelude//cxx:cxx_sources.bzl", "get_srcs_with_flags")
load(
    "@prelude//linking:link_info.bzl",
    "LinkInfo",  # @unused Used as a type
    "SwiftRuntimeLinkable",
)

def create_swift_runtime_linkable(ctx: AnalysisContext) -> [SwiftRuntimeLinkable, None]:
    for s in get_srcs_with_flags(ctx):
        if s.file.extension == SWIFT_EXTENSION:
            return SwiftRuntimeLinkable(runtime_required = True)
    return None

def get_swift_runtime_linker_flags(ctx: AnalysisContext, linkable: [SwiftRuntimeLinkable, None]) -> cmd_args:
    _ = ctx
    _ = linkable
    return cmd_args()

def extract_swift_runtime_linkables(link_infos: [list[LinkInfo], None]) -> list[SwiftRuntimeLinkable]:
    linkables = []
    for info in link_infos:
        for linkable in info.linkables:
            if isinstance(linkable, SwiftRuntimeLinkable):
                linkables.append(linkable)

    return linkables
