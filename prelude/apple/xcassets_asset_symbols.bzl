# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# @oss-disable[end= ]: # Asset symbol hooks implemented in meta_only; open source gets no-op stubs.

# @oss-disable[end= ]: load("@prelude//apple/meta_only:meta_xcassets_asset_symbol_spec.bzl", _MetaXcassetsAssetSymbolSpec = "MetaXcassetsAssetSymbolSpec")
# @oss-disable[end= ]: load(
    # @oss-disable[end= ]: "@prelude//apple/meta_only:meta_xcassets_asset_symbol_usage.bzl",
    # @oss-disable[end= ]: _usage_providers_and_subtargets = "meta_xcassets_asset_symbol_usage_providers_and_subtargets",
# @oss-disable[end= ]: )
load("@prelude//cxx:cxx_sources.bzl", "CxxSrcWithFlags")

# @oss-disable[end= ]: MetaXcassetsAssetSymbolSpec = _MetaXcassetsAssetSymbolSpec
MetaXcassetsAssetSymbolSpec = record() # @oss-enable

def meta_xcassets_asset_symbol_usage_providers_and_subtargets(
    ctx: AnalysisContext, cxx_srcs: list[CxxSrcWithFlags], swift_srcs: list[CxxSrcWithFlags]
) -> (list[Provider], dict[str, list[Provider]]):
    # @oss-disable[end= ]: return _usage_providers_and_subtargets(ctx, cxx_srcs, swift_srcs)
    return [], {} # @oss-enable
