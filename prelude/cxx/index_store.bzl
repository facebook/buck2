# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")

INDEX_STORE_SUBTARGET = "index-store"
FULL_INDEX_STORE_SUBTARGET = "full-index-store"

def _index_store_args(artifact: Artifact) -> Artifact:
    return artifact

IndexStoreTSet = transitive_set(
    args_projections = {
        "args": _index_store_args,
    },
)

IndexStoreInfo = provider(
    fields = {
        # The name of the target.
        "name": provider_field(str),
        # A tset with this target's index store and all of its dependency's index stores in the children.
        "tset": provider_field(IndexStoreTSet),
    },
)

def _get_merge_index_store_tool(ctx: AnalysisContext) -> RunInfo | None:
    apple_toolchain = getattr(ctx.attrs, "_apple_toolchain", None)
    if apple_toolchain == None:
        return None
    return apple_toolchain[AppleToolchainInfo].merge_index_store

def _merge_index_store(ctx: AnalysisContext, index_stores: list[Artifact] | TransitiveSet, merge_output_dir_name: str | None = None) -> Artifact | None:
    if isinstance(index_stores, list):
        if len(index_stores) == 0:
            return None

        if len(index_stores) == 1:
            return index_stores[0]

    merge_index_store_tool = _get_merge_index_store_tool(ctx)
    if merge_index_store_tool == None:
        return None
    if merge_output_dir_name == None:
        merge_output_dir_name = paths.join("__indexstore__", ctx.attrs.name, "index_store")
    merged_index_store = ctx.actions.declare_output(merge_output_dir_name)
    cmd = cmd_args([merge_index_store_tool])
    cmd.add(["--dest", merged_index_store.as_output()])
    cmd.add(["--sources"])
    if isinstance(index_stores, list):
        cmd.add(index_stores)
    else:
        cmd.add(index_stores.project_as_args("args"))

    # use prefer_remote = True here, it would have two following advantages
    # 1. Each bucket will perform a merge on RE,  which will fully utilize the high-speed network for materalizaion
    # and utalize the resource to mergre parallel.
    # 2. After merging for each bucket, the index store will be smaller, which makes it less to materialize locally
    # and speeds up the local merge, thus accelerating the overall process.
    ctx.actions.run(cmd, category = "merge_index_store", identifier = merge_output_dir_name, allow_cache_upload = True, prefer_remote = True)
    return merged_index_store

def _gather_deps_index_store_tsets(deps: list[Dependency]) -> list[IndexStoreTSet]:
    deps_indexstore_infos = filter(None, [dep.get(IndexStoreInfo) for dep in deps])
    return [info.tset for info in deps_indexstore_infos]

def create_index_store_subtargets_and_provider(ctx: AnalysisContext, current_target_index_stores: list[Artifact], deps: list[Dependency]) -> (dict[str, list[Provider]], IndexStoreInfo):
    # Create a subtarget for the current target's index store
    sub_targets = {}
    merged_index_store = _merge_index_store(ctx, current_target_index_stores)
    sub_targets[INDEX_STORE_SUBTARGET] = [DefaultInfo(default_output = merged_index_store)]

    # Crate a subtarget for the merged  all deps' and itself's index store
    deps_indexstore_tsets = _gather_deps_index_store_tsets(deps)
    if merged_index_store:
        index_store_tset = ctx.actions.tset(IndexStoreTSet, value = merged_index_store, children = deps_indexstore_tsets)
    else:
        index_store_tset = ctx.actions.tset(IndexStoreTSet, children = deps_indexstore_tsets)
    index_store_info = IndexStoreInfo(name = ctx.attrs.name, tset = index_store_tset)

    output = _merge_index_store(ctx, index_store_tset, paths.join("__indexstore__", ctx.attrs.name, "full_index_stores"))
    sub_targets[FULL_INDEX_STORE_SUBTARGET] = [DefaultInfo(default_output = output)]

    return (sub_targets, index_store_info)
