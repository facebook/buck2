# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")

def _index_store_args(artifact: Artifact) -> Artifact:
    return artifact

# A transitive set (tset) where we don't need to format the items to use them
# as CLI arguments.
#
# https://www.internalfb.com/intern/staticdocs/buck2/docs/rule_authors/transitive_sets/#projections-using-transitive-sets-in-command-lines
IndexStoreTSet = transitive_set(
    args_projections = {
        "args": _index_store_args,
    },
)

# A struct representing information about the index store.
#
# https://www.internalfb.com/intern/staticdocs/buck2/docs/rule_authors/writing_rules/#providers
IndexStoreInfo = provider(
    fields = {
        # The name of the target.
        "name": provider_field(str),
        # A tset (transitive set) with this target's index store and the index store
        # of all the dependencies as children.
        "tset": provider_field(IndexStoreTSet),
    },
)

def _get_merge_index_store_tool(ctx: AnalysisContext) -> RunInfo | None:
    apple_toolchain = getattr(ctx.attrs, "_apple_toolchain", None)
    if apple_toolchain == None:
        return None
    return apple_toolchain[AppleToolchainInfo].merge_index_store

def _merge_index_store(ctx: AnalysisContext, merge_index_store_tool: RunInfo, index_stores: list[Artifact] | TransitiveSet, merge_output_dir_name: str) -> Artifact:
    if isinstance(index_stores, list):
        if len(index_stores) == 1:
            return index_stores[0]

    merged_index_store = ctx.actions.declare_output(merge_output_dir_name)

    cmd = cmd_args([merge_index_store_tool])
    cmd.add(["--dest", merged_index_store.as_output()])

    txt_args = cmd_args()
    if isinstance(index_stores, list):
        txt_args.add(index_stores)
    else:
        txt_args.add(index_stores.project_as_args("args"))

    argsfile_path = merge_output_dir_name + ".files.txt"
    argsfile = ctx.actions.write(argsfile_path, txt_args)
    argsfile = cmd_args(argsfile, format = "@{}", hidden = txt_args)

    cmd.add(["--sources"])
    cmd.add(argsfile)

    # Merge all the individual index store directories into a single
    # directory containing all the unit and record files.
    #
    # We use `prefer_remote` to maximize the work done on RE, because
    # we assume that RE hosts have the fastest network connections,
    # especially if the buck command is running on a local
    # laptop. There can also be some duplication between index stores,
    # so we expect the final output to be smaller and less to
    # download.
    #
    # We use `allow_cache_upload` because the output should be
    # deterministic, so we can reuse results from previous runs.
    ctx.actions.run(cmd, category = "merge_index_store", identifier = merge_output_dir_name, allow_cache_upload = True, prefer_remote = True)
    return merged_index_store

def _gather_deps_index_store_tsets(deps: list[Dependency]) -> list[IndexStoreTSet]:
    deps_indexstore_infos = filter(None, [dep.get(IndexStoreInfo) for dep in deps])
    return [info.tset for info in deps_indexstore_infos]

def create_index_store_subtargets_and_provider(ctx: AnalysisContext, current_target_index_stores: list[Artifact], deps: list[Dependency]) -> (dict[str, list[Provider]], IndexStoreInfo):
    merge_index_store_tool = _get_merge_index_store_tool(ctx)

    if merge_index_store_tool:
        merged_index_store = _merge_index_store(ctx, merge_index_store_tool, current_target_index_stores, paths.join("__indexstore__", ctx.attrs.name, "index_store"))

        deps_indexstore_tsets = _gather_deps_index_store_tsets(deps)
        index_store_tset = ctx.actions.tset(IndexStoreTSet, value = merged_index_store, children = deps_indexstore_tsets)
        merged_full_index_store = _merge_index_store(ctx, merge_index_store_tool, index_store_tset, paths.join("__indexstore__", ctx.attrs.name, "full_index_stores"))

        sub_targets = {
            # Create a subtarget foo//bar:baz[full-index-store] that builds and merges
            # the index stores from foo//bar:baz plus all its transitive dependencies.
            "full-index-store": [DefaultInfo(default_output = merged_full_index_store)],
            # Create a subtarget foo//bar:baz[index-store] that builds the
            # index store for foo//bar:baz.
            "index-store": [DefaultInfo(default_output = merged_index_store)],
        }

        index_store_info = IndexStoreInfo(name = ctx.attrs.name, tset = index_store_tset)
    else:
        sub_targets = {
            "full-index-store": [DefaultInfo(default_output = None)],
            "index-store": [DefaultInfo(default_output = None)],
        }

        index_store_info = IndexStoreInfo(name = ctx.attrs.name, tset = ctx.actions.tset(IndexStoreTSet))

    return (sub_targets, index_store_info)
