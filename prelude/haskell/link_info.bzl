# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "CxxToolchainInfo",
)
load(
    "@prelude//haskell:library_info.bzl",
    "HaskellLibraryInfoTSet",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)

# A list of `HaskellLibraryInfo`s.
HaskellLinkInfo = provider(
    # Contains a list of HaskellLibraryInfo records.
    fields = {
        "info": provider_field(dict[LinkStyle, HaskellLibraryInfoTSet]),
        "prof_info": provider_field(dict[LinkStyle, HaskellLibraryInfoTSet]),
    },
)

# HaskellProfLinkInfo exposes the MergedLinkInfo of a target and all of its
# dependencies built for profiling. This allows top-level targets (e.g.
# `haskell_binary`) to be defined with profiling enabled by default.
HaskellProfLinkInfo = provider(
    fields = {
        "prof_infos": provider_field(typing.Any, default = None),  # MergedLinkInfo
    },
)

def cxx_toolchain_link_style(ctx: AnalysisContext) -> LinkStyle:
    return ctx.attrs._cxx_toolchain[CxxToolchainInfo].linker_info.link_style

def attr_link_style(ctx: AnalysisContext) -> LinkStyle:
    if ctx.attrs.link_style != None:
        return LinkStyle(ctx.attrs.link_style)
    else:
        return cxx_toolchain_link_style(ctx)
