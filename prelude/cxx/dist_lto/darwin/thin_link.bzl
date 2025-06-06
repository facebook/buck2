# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//linking:link_info.bzl",
    "append_linkable_args",
)
load(":common_types.bzl", "DThinLTOLinkData", "LinkDataType", "execute_link_actions_locally")

def thin_link(
        ctx: AnalysisContext,
        output: Artifact,
        sorted_index_link_data: list[DThinLTOLinkData],
        common_link_flags: cmd_args,
        linker: RunInfo,
        lto_planner: RunInfo,
        post_linker_flags: cmd_args,
        index_argsfile_out: Artifact,
        final_link_index_out: Artifact,
        link_plan_out: Artifact,
        identifier: str | None,
        premerger_enabled: bool,
        plan_inputs: list[Artifact],
        plan_outputs: list[OutputArtifact],
        make_cat,
        make_id):
    def plan(ctx: AnalysisContext, artifacts, outputs, link_plan):
        # See comments in dist_lto_planner.py for semantics on the values that are pushed into index_meta.
        index_meta_records = []

        index_cat = make_cat("thin_lto_index")
        index_file_out = ctx.actions.declare_output(make_id(index_cat) + "/index")
        index_out_dir = cmd_args(index_file_out.as_output(), parent = 1)

        index_args = cmd_args(common_link_flags)
        index_args.add(cmd_args(index_file_out.as_output(), format = "-Wl,--thinlto-index-only={}"))
        index_args.add("-Wl,--thinlto-emit-imports-files")
        index_args.add("-Wl,--thinlto-full-index")

        # By default the linker will write artifacts (import files and sharded indices) next to input bitcode files with
        # a different suffix. This can be problematic if you are running two distributed links on the same machine at the # same time consuming the same input bitcode files. That is the links would overwrite each other's artifacts. This
        # flag allows you to write all these artifacts into a unique directory per link to avoid this problem.
        index_args.add(cmd_args(index_out_dir, format = "-Wl,--thinlto-prefix-replace=;{}/"))

        # buildifier: disable=uninitialized
        for idx, artifact in enumerate(sorted_index_link_data):
            link_data = artifact.link_data

            if artifact.data_type == LinkDataType("eager_bitcode") or artifact.data_type == LinkDataType("lazy_bitcode"):
                if artifact.data_type == LinkDataType("lazy_bitcode") and link_data.archive_start:
                    index_args.add("-Wl,--start-lib")

                index_args.add(link_data.input_object_file)

                if artifact.data_type == LinkDataType("lazy_bitcode") and link_data.archive_end:
                    index_args.add("-Wl,--end-lib")

                object_file_record = {
                    "input_object_file_path": link_data.input_object_file,
                    "output_index_shard_file_path": outputs[link_data.output_index_shard_file].as_output(),
                    "output_plan_file_path": outputs[link_data.plan].as_output(),
                    "record_type": "OBJECT_FILE",
                    "starlark_array_index": idx,
                }
                if premerger_enabled:
                    object_file_record["output_premerged_bitcode_file_path"] = outputs[link_data.merged_bc].as_output()

                index_meta_records.append(object_file_record)

            elif artifact.data_type == LinkDataType("archive"):
                manifest = artifacts[link_data.manifest].read_json()

                if not manifest["objects"]:
                    # Despite not having any objects (and thus not needing a plan), we still need to bind the plan output.
                    ctx.actions.write(outputs[link_data.plan].as_output(), "{}")
                    make_indexes_dir_cmd = cmd_args(["/bin/sh", "-c", "mkdir", "-p", outputs[link_data.output_index_shard_files_dir].as_output()])
                    ctx.actions.run(make_indexes_dir_cmd, category = make_cat("thin_lto_mkdir"), identifier = link_data.name + "_indexes_dir")
                    if premerger_enabled:
                        make_merged_bc_dir_cmd = cmd_args(["/bin/sh", "-c", "mkdir", "-p", outputs[link_data.merged_bc_dir].as_output()])
                        ctx.actions.run(make_merged_bc_dir_cmd, category = make_cat("thin_lto_mkdir"), identifier = link_data.name + "_merged_bc_dir")
                    continue

                index_args.add(cmd_args(hidden = link_data.input_object_files_dir))

                if not link_data.link_whole:
                    index_args.add("-Wl,--start-lib")

                for obj in manifest["objects"]:
                    lazy_object_file_record = {
                        "input_object_file_path": obj,
                        "output_index_shards_directory_path": outputs[link_data.output_index_shard_files_dir].as_output(),
                        "output_plan_file_path": outputs[link_data.plan].as_output(),
                        "record_type": "ARCHIVE_MEMBER",
                        "starlark_array_index": idx,  # Each object shares the same index in the stalark array pointing to the archive link data
                    }
                    if premerger_enabled:
                        lazy_object_file_record["output_premerged_bitcode_directory_path"] = outputs[link_data.merged_bc_dir].as_output()

                    index_meta_records.append(lazy_object_file_record)

                    index_args.add(obj)

                if not link_data.link_whole:
                    index_args.add("-Wl,--end-lib")

            elif artifact.data_type == LinkDataType("dynamic_library"):
                append_linkable_args(index_args, link_data.linkable)

            else:
                fail("Unhandled data type: {}".format(str(artifact.data_type)))

        output_as_string = cmd_args(output, ignore_artifacts = True)
        index_args.add("-o", output_as_string)
        index_args.add(post_linker_flags)
        index_cmd = cmd_args(linker)
        index_argfile, _ = ctx.actions.write(
            outputs[index_argsfile_out].as_output(),
            index_args,
            allow_args = True,
        )
        index_cmd.add(cmd_args(index_argfile, format = "@{}"))

        index_meta_file = ctx.actions.write_json(
            output.basename + ".thinlto.meta.json",
            index_meta_records,
            with_inputs = True,
        )

        plan_cmd = cmd_args([lto_planner, "--meta", index_meta_file, "--index", index_out_dir, "--link-plan", outputs[link_plan].as_output(), "--final-link-index", outputs[final_link_index_out].as_output()])
        if premerger_enabled:
            plan_cmd.add("--enable-premerger")
        plan_cmd.add("--", index_cmd)

        plan_cmd.add(cmd_args(hidden = [
            index_args,
        ]))
        ctx.actions.run(plan_cmd, category = index_cat, identifier = identifier, local_only = execute_link_actions_locally())

    # TODO(T117513091) - dynamic_output does not allow for an empty list of dynamic inputs. If we have no archives
    # to process, we will have no dynamic inputs, and the plan action can be non-dynamic.
    #
    # However, buck2 disallows `dynamic_output` with a empty input list. We also can't call our `plan` function
    # directly, since it uses `ctx.outputs` to bind its outputs. Instead of doing Starlark hacks to work around
    # the lack of `ctx.outputs`, we declare an empty file as a dynamic input.
    plan_inputs.append(ctx.actions.write(output.basename + ".plan_hack.txt", ""))
    plan_outputs.extend([link_plan_out.as_output(), index_argsfile_out.as_output(), final_link_index_out.as_output()])
    ctx.actions.dynamic_output(dynamic = plan_inputs, inputs = [], outputs = plan_outputs, f = lambda ctx, artifacts, outputs: plan(ctx, artifacts, outputs, link_plan_out))
