# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")

INDEX_STORE_SUBTARGET = "index-store"

def _get_merge_index_store_tool(ctx: AnalysisContext) -> RunInfo | None:
    apple_toolchain = getattr(ctx.attrs, "_apple_toolchain", None)
    if apple_toolchain == None:
        return None
    return apple_toolchain[AppleToolchainInfo].merge_index_store

def merge_index_store(ctx: AnalysisContext, index_stores: list[Artifact]) -> Artifact | None:
    if len(index_stores) == 0:
        return None

    if len(index_stores) == 1:
        return index_stores[0]

    merge_index_store_tool = _get_merge_index_store_tool(ctx)
    if merge_index_store_tool == None:
        return None
    merge_output_dir_name = paths.join("__indexstore__", ctx.attrs.name, "index_store")
    merged_index_store = ctx.actions.declare_output(merge_output_dir_name)
    cmd = cmd_args([merge_index_store_tool])
    cmd.add(["--dest", merged_index_store.as_output()])
    cmd.add(["--sources"] + index_stores)

    ctx.actions.run(cmd, category = "merge_index_store", identifier = merge_output_dir_name, allow_cache_upload = True)
    return merged_index_store
