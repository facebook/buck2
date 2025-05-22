# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "make_artifact_tset",
)
load("@prelude//:paths.bzl", "paths")
load("@prelude//apple:apple_utility.bzl", "get_apple_architecture")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load(
    "@prelude//cxx:cxx_link_utility.bzl",
    "CxxSanitizerRuntimeArguments",
    "cxx_link_cmd_parts",
    "get_extra_darwin_linker_flags",
    "linker_map_args",
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "LinkerType")
load("@prelude//cxx:link_types.bzl", "LinkOptions")
load("@prelude//cxx:target_sdk_version.bzl", "get_target_sdk_version_flags")
load(
    "@prelude//linking:link_info.bzl",
    "ArchiveLinkable",
    "ExtraLinkerOutputs",
    "FrameworksLinkable",  # @unused Used as a type
    "LinkInfo",
    "LinkedObject",
    "ObjectsLinkable",
    "SharedLibLinkable",  # @unused Used as a type
    "SwiftmoduleLinkable",  # @unused Used as a type
    "append_linkable_args",
    "map_to_link_infos",
    "unpack_external_debug_info",
)
load("@prelude//linking:strip.bzl", "strip_object")
load("@prelude//utils:argfile.bzl", "at_argfile")
load("@prelude//utils:lazy.bzl", "lazy")
load(":thin_link_record_defs.bzl", "ArchiveMemberOptimizationPlan", "ArchiveOptimizationPlan", "BitcodeMergeState", "ObjectFileOptimizationPlan")

_EagerBitcodeLinkData = record(
    name = str,
    initial_object = Artifact,
    bc_file = Artifact,
    plan = Artifact,
    opt_object = Artifact,
    merged_bc = field([Artifact, None]),
)

_LazyBitcodeLinkData = record(
    name = str,
    initial_object = Artifact,
    bc_file = Artifact,
    plan = Artifact,
    opt_object = Artifact,
    merged_bc = field([Artifact, None]),
    archive_start = bool,
    archive_end = bool,
)

_ArchiveLinkData = record(
    name = str,
    manifest = Artifact,
    objects_dir = Artifact,
    opt_objects_dir = Artifact,
    indexes_dir = Artifact,
    plan = Artifact,
    link_whole = bool,
    merged_bc_dir = field([Artifact, None]),
)

_DynamicLibraryLinkData = record(
    linkable = SharedLibLinkable,
)

_DataType = enum(
    "eager_bitcode",
    "lazy_bitcode",
    "archive",
    "dynamic_library",
)

_IndexLinkData = record(
    data_type = _DataType,
    link_data = field([_LazyBitcodeLinkData, _EagerBitcodeLinkData, _ArchiveLinkData, _DynamicLibraryLinkData]),
)

def identity_projection(value: cmd_args):
    return value

# This tset exists for the sole purpose of saving memory. When action inputs
# are hashed, the memory cost for hashing tset projection generated input
# tree nodes is paid once per unique node. Without a tset projection every
# input is hashed once per action it is included in. The opt actions created
# here share a tremendous number of inputs, so we might end up hashing a
# given input 1000s of times, and paying dearly for it in memory consumption.
# This is not ideal either, the memory allocated to hash these tset projection
# generated inputs will not be freed for the duration of the build, but in
# testing that is a worth while tradeoff.
IdentityTSet = transitive_set(args_projections = {"identity": identity_projection})

# TODO(nuriamari) Delete once link actions over RE measured
def _execute_link_actions_locally() -> bool:
    return read_root_config("user", "dthin_lto_link_actions_locally", "false") in ("True", "true")

def cxx_darwin_dist_link(
        ctx: AnalysisContext,
        # The destination for the link output.
        output: Artifact,
        opts: LinkOptions,
        premerger_enabled: bool,
        executable_link: bool,
        sanitizer_runtime_args: CxxSanitizerRuntimeArguments,
        linker_map: Artifact | None = None) -> (LinkedObject, dict[str, list[DefaultInfo]]):
    """
    Perform a distributed thin-lto link into the supplied output

    Distributed thinlto splits the link into three stages:
    1. global "indexing" step
    2. many individual compilation unit optimization steps
    3. final global link step

    The 2nd and 3rd of those are done just by constructing compiler/linker commands (in dynamic_output
    sections) using the output of the first.

    For the first, we need to post-process the linker index output to get it into a form
    that is easy for us to consume from within bzl.
    """

    links = opts.links

    # A category suffix that will be added to the category of the link action that is generated.
    category_suffix = opts.category_suffix

    # An identifier that will uniquely name this link action in the context of a category. Useful for
    # differentiating multiple link actions in the same rule.
    identifier = opts.identifier

    def make_cat(c: str) -> str:
        """ Used to make sure categories for our actions include the provided suffix """
        if category_suffix != None:
            return c + "_" + category_suffix
        return c

    def make_id(i: str) -> str:
        """ Used to make sure identifiers for our actions include the provided identifier """
        if identifier != None:
            return identifier + "_" + i
        return i

    recorded_outputs = {}

    def name_for_obj(link_name: str, object_artifact: Artifact) -> str:
        """ Creates a unique name/path we can use for a particular object file input """
        prefix = "{}/{}".format(link_name, object_artifact.short_path)

        # it's possible (though unlikely) that we can get duplicate name/short_path, so just uniquify them
        if prefix in recorded_outputs:
            recorded_outputs[prefix] += 1
            extra = recorded_outputs[prefix]
            prefix = "{}-{}".format(prefix, extra)
        else:
            recorded_outputs[prefix] = 1
        return prefix

    names = {}

    def name_for_link(info: LinkInfo) -> str:
        """ Creates a unique name for a LinkInfo that we are consuming """
        name = info.name or "unknown"
        if name not in names:
            names[name] = 1
        else:
            names[name] += 1
            name += "-{}".format(names[name])
        return make_id(name)

    link_infos = map_to_link_infos(links)

    cxx_toolchain = get_cxx_toolchain_info(ctx)
    lto_planner = cxx_toolchain.internal_tools.dist_lto.planner[LinkerType("darwin")]
    lto_opt = cxx_toolchain.internal_tools.dist_lto.opt[LinkerType("darwin")]
    lto_prepare = cxx_toolchain.internal_tools.dist_lto.prepare[LinkerType("darwin")]
    lto_copy = cxx_toolchain.internal_tools.dist_lto.copy

    # The process to optimize and codegen a given bitcode file may depend on other bitcode
    # files (ie. to inline function calls between bitcode files). Which bitcode files depend
    # on one another is not known until thin link (sometimes called the dynamic plan) is run.
    # For each input bitcode file, thin link will produce a plan JSON document that will identify
    # which other bitcode files (or archive of bitcode files) it should be optimized along side.
    # Thin link idenitifies these depended upon bitcode files by their index in the sorted version of this array.
    unsorted_index_link_data = []
    linker_flags = []
    common_link_flags = cmd_args(get_target_sdk_version_flags(ctx), get_extra_darwin_linker_flags())
    common_link_flags.add(sanitizer_runtime_args.extra_link_args)
    extra_codegen_flags = get_target_sdk_version_flags(ctx)

    # Information used to construct the dynamic plan:
    plan_inputs = []
    plan_outputs = []

    prepare_cat = make_cat("thin_lto_prepare")

    for link in link_infos:
        link_name = name_for_link(link)

        linker_flags.append(link.pre_flags)
        linker_flags.append(link.post_flags)

        for linkable in link.linkables:
            if isinstance(linkable, ObjectsLinkable):
                for obj in linkable.objects:
                    name = name_for_obj(link_name, obj)
                    bc_output = ctx.actions.declare_output(name + ".thinlto.bc")
                    plan_output = ctx.actions.declare_output(name + ".opt.plan")
                    opt_output = ctx.actions.declare_output(name + ".opt.o")
                    merged_bc_output = None
                    if premerger_enabled:
                        merged_bc_output = ctx.actions.declare_output(name + ".merged.bc")
                        plan_outputs.append(merged_bc_output.as_output())

                    data = _IndexLinkData(
                        data_type = _DataType("eager_bitcode"),
                        link_data = _EagerBitcodeLinkData(
                            name = name,
                            initial_object = obj,
                            bc_file = bc_output,
                            plan = plan_output,
                            opt_object = opt_output,
                            merged_bc = merged_bc_output,
                        ),
                    )
                    unsorted_index_link_data.append(data)
                    plan_outputs.extend([bc_output.as_output(), plan_output.as_output()])

            elif isinstance(linkable, ArchiveLinkable) and linkable.archive.external_objects:
                for virtual_archive_index, obj in enumerate(linkable.archive.external_objects):
                    name = name_for_obj(link_name, obj)
                    bc_output = ctx.actions.declare_output(name + ".thinlto.bc")
                    plan_output = ctx.actions.declare_output(name + ".opt.plan")
                    opt_output = ctx.actions.declare_output(name + ".opt.o")
                    merged_bc_output = None
                    if premerger_enabled:
                        merged_bc_output = ctx.actions.declare_output(name + ".merged.bc")
                        plan_outputs.append(merged_bc_output.as_output())

                    # The first member in a non force loaded virtual archive gets --start-lib prended on the command line
                    archive_start = (not linkable.link_whole) and virtual_archive_index == 0
                    archive_end = (not linkable.link_whole) and virtual_archive_index == len(linkable.archive.external_objects) - 1
                    data = _IndexLinkData(
                        data_type = _DataType("lazy_bitcode"),
                        link_data = _LazyBitcodeLinkData(
                            name = name,
                            initial_object = obj,
                            bc_file = bc_output,
                            plan = plan_output,
                            opt_object = opt_output,
                            merged_bc = merged_bc_output,
                            archive_start = archive_start,
                            archive_end = archive_end,
                        ),
                    )
                    unsorted_index_link_data.append(data)
                    plan_outputs.extend([bc_output.as_output(), plan_output.as_output()])

            elif isinstance(linkable, ArchiveLinkable):
                # Our implementation of Distributed ThinLTO operates on individual objects, not archives. Since these
                # archives might still contain LTO-able bitcode, we first extract the objects within the archive into
                # another directory and write a "manifest" containing the list of objects that the archive contained.
                #
                # Later actions in the LTO compilation pipeline will read this manifest and dynamically dispatch
                # actions on the objects that the manifest reports.

                name = name_for_obj(link_name, linkable.archive.artifact)
                archive_manifest = ctx.actions.declare_output("%s/%s/manifest.json" % (prepare_cat, name))
                archive_objects = ctx.actions.declare_output("%s/%s/objects" % (prepare_cat, name), dir = True)
                archive_opt_objects = ctx.actions.declare_output("%s/%s/opt_objects" % (prepare_cat, name), dir = True)
                archive_indexes = ctx.actions.declare_output("%s/%s/indexes" % (prepare_cat, name), dir = True)
                archive_plan = ctx.actions.declare_output("%s/%s/plan.json" % (prepare_cat, name))
                archive_merged_bc_files = None
                if premerger_enabled:
                    archive_merged_bc_files = ctx.actions.declare_output("%s/%s/merged_bc_files" % (prepare_cat, name), dir = True)
                    plan_outputs.append(archive_merged_bc_files.as_output())

                prepare_args = cmd_args([
                    lto_prepare,
                    "--manifest-out",
                    archive_manifest.as_output(),
                    "--objects-out",
                    archive_objects.as_output(),
                    "--ar",
                    cxx_toolchain.linker_info.archiver,
                    "--archive",
                    linkable.archive.artifact,
                    "--name",
                    name,
                    "--target-architecture",
                    get_apple_architecture(ctx),
                    "--lipo",
                    cxx_toolchain.lipo,
                ])
                ctx.actions.run(prepare_args, category = make_cat("thin_lto_prepare"), identifier = name)

                data = _IndexLinkData(
                    data_type = _DataType("archive"),
                    link_data = _ArchiveLinkData(
                        name = name,
                        manifest = archive_manifest,
                        objects_dir = archive_objects,
                        opt_objects_dir = archive_opt_objects,
                        indexes_dir = archive_indexes,
                        plan = archive_plan,
                        link_whole = linkable.link_whole,
                        merged_bc_dir = archive_merged_bc_files,
                    ),
                )
                unsorted_index_link_data.append(data)
                plan_inputs.extend([archive_manifest, archive_objects])
                plan_outputs.extend([archive_indexes.as_output(), archive_plan.as_output()])
            elif isinstance(linkable, SharedLibLinkable):
                data = _IndexLinkData(
                    data_type = _DataType("dynamic_library"),
                    link_data = _DynamicLibraryLinkData(linkable = linkable),
                )
                unsorted_index_link_data.append(data)
            elif isinstance(linkable, FrameworksLinkable) or isinstance(linkable, SwiftmoduleLinkable):
                # These linkables are handled separately for flag deduplication purposes, as in append_linkable_args:
                # https://www.internalfb.com/code/fbsource/[c6d2c820b394]/fbcode/buck2/prelude/linking/link_info.bzl?lines=271-278
                pass
            else:
                fail("Unhandled linkable type: {}".format(str(linkable)))

    def sort_index_link_data(input_list: list[_IndexLinkData]) -> list[_IndexLinkData]:
        # Sort link datas to reduce binary size. The idea is to encourage the linker to load the minimal number of object files possible. We load force loaded archives first (since they will be loaded no matter what), then non lazy object files (which will also be loaded no matter what), then shared libraries (to share as many symbols as possible), then finally regular archives
        force_loaded_archives = []
        regular_archives = []
        object_files = []
        dynamic_libraries = []
        for link_data in input_list:
            if link_data.data_type == _DataType("eager_bitcode"):
                object_files.append(link_data)
            elif link_data.data_type == _DataType("lazy_bitcode"):
                regular_archives.append(link_data)
            elif link_data.data_type == _DataType("archive"):
                if link_data.link_data.link_whole:
                    force_loaded_archives.append(link_data)
                else:
                    regular_archives.append(link_data)
            elif link_data.data_type == _DataType("dynamic_library"):
                dynamic_libraries.append(link_data)

        return force_loaded_archives + object_files + dynamic_libraries + regular_archives

    sorted_index_link_data = sort_index_link_data(unsorted_index_link_data)

    index_argsfile_out = ctx.actions.declare_output(output.basename + ".thinlto_index_argsfile")
    final_link_index = ctx.actions.declare_output(output.basename + ".final_link_index")

    def prepare_index_flags(include_inputs: bool, index_args_out: cmd_args, index_meta_records_out: list, ctx: AnalysisContext, artifacts, outputs):
        for flag in linker_flags:
            index_args_out.add(flag)

        if include_inputs:
            # buildifier: disable=uninitialized
            for idx, artifact in enumerate(sorted_index_link_data):
                link_data = artifact.link_data

                if artifact.data_type == _DataType("eager_bitcode") or artifact.data_type == _DataType("lazy_bitcode"):
                    if artifact.data_type == _DataType("lazy_bitcode") and link_data.archive_start:
                        index_args_out.add("-Wl,--start-lib")

                    index_args_out.add(link_data.initial_object)

                    if artifact.data_type == _DataType("lazy_bitcode") and link_data.archive_end:
                        index_args_out.add("-Wl,--end-lib")

                    object_file_record = {
                        "input_object_file_path": link_data.initial_object,
                        "output_index_shard_file_path": outputs[link_data.bc_file].as_output(),
                        "output_plan_file_path": outputs[link_data.plan].as_output(),
                        "record_type": "OBJECT_FILE",
                        "starlark_array_index": idx,
                    }
                    if premerger_enabled:
                        object_file_record["output_premerged_bitcode_file_path"] = outputs[link_data.merged_bc].as_output()

                    index_meta_records_out.append(object_file_record)

                elif artifact.data_type == _DataType("archive"):
                    manifest = artifacts[link_data.manifest].read_json()

                    if not manifest["objects"]:
                        # Despite not having any objects (and thus not needing a plan), we still need to bind the plan output.
                        ctx.actions.write(outputs[link_data.plan].as_output(), "{}")
                        make_indexes_dir_cmd = cmd_args(["/bin/sh", "-c", "mkdir", "-p", outputs[link_data.indexes_dir].as_output()])
                        ctx.actions.run(make_indexes_dir_cmd, category = make_cat("thin_lto_mkdir"), identifier = link_data.name + "_indexes_dir")
                        if premerger_enabled:
                            make_merged_bc_dir_cmd = cmd_args(["/bin/sh", "-c", "mkdir", "-p", outputs[link_data.merged_bc_dir].as_output()])
                            ctx.actions.run(make_merged_bc_dir_cmd, category = make_cat("thin_lto_mkdir"), identifier = link_data.name + "_merged_bc_dir")
                        continue

                    index_args_out.add(cmd_args(hidden = link_data.objects_dir))

                    if not link_data.link_whole:
                        index_args_out.add("-Wl,--start-lib")

                    for obj in manifest["objects"]:
                        lazy_object_file_record = {
                            "input_object_file_path": obj,
                            "output_index_shards_directory_path": outputs[link_data.indexes_dir].as_output(),
                            "output_plan_file_path": outputs[link_data.plan].as_output(),
                            "record_type": "ARCHIVE_MEMBER",
                            "starlark_array_index": idx,  # Each object shares the same index in the stalark array pointing to the archive link data
                        }
                        if premerger_enabled:
                            lazy_object_file_record["output_premerged_bitcode_directory_path"] = outputs[link_data.merged_bc_dir].as_output()

                        index_meta_records_out.append(lazy_object_file_record)

                        index_args_out.add(obj)

                    if not link_data.link_whole:
                        index_args_out.add("-Wl,--end-lib")

                elif artifact.data_type == _DataType("dynamic_library"):
                    append_linkable_args(index_args_out, link_data.linkable)

                else:
                    fail("Unhandled data type: {}".format(str(artifact.data_type)))

        output_as_string = cmd_args(output, ignore_artifacts = True)
        index_args_out.add("-o", output_as_string)

    # The flags used for the thin-link action. Unlike index_args, this does not include input files, and
    # is only used for debugging and testing, and can be determined without dynamic output.
    index_flags_for_debugging = cmd_args()
    index_cmd_parts = cxx_link_cmd_parts(cxx_toolchain, executable_link)
    index_flags_for_debugging.add(index_cmd_parts.linker_flags)
    index_flags_for_debugging.add(common_link_flags)
    index_flags_for_debugging.add(index_cmd_parts.post_linker_flags)
    prepare_index_flags(include_inputs = False, index_args_out = index_flags_for_debugging, index_meta_records_out = [], ctx = ctx, artifacts = None, outputs = None)
    index_flags_for_debugging_argsfile, _ = ctx.actions.write(output.basename + ".thinlto_index_debugging_argsfile", index_flags_for_debugging, allow_args = True)

    def plan(ctx: AnalysisContext, artifacts, outputs, link_plan):
        # index link command args
        index_args = cmd_args()

        # See comments in dist_lto_planner.py for semantics on the values that are pushed into index_meta.
        index_meta_records = []

        prepare_index_flags(include_inputs = True, index_args_out = index_args, index_meta_records_out = index_meta_records, ctx = ctx, artifacts = artifacts, outputs = outputs)

        index_argfile, _ = ctx.actions.write(
            outputs[index_argsfile_out].as_output(),
            index_args,
            allow_args = True,
        )

        index_cat = make_cat("thin_lto_index")
        index_file_out = ctx.actions.declare_output(make_id(index_cat) + "/index")
        index_out_dir = cmd_args(index_file_out.as_output(), parent = 1)

        index_cmd_parts = cxx_link_cmd_parts(cxx_toolchain, executable_link)

        index_cmd = index_cmd_parts.link_cmd
        index_cmd.add(common_link_flags)
        index_cmd.add(cmd_args(index_argfile, format = "@{}"))

        index_cmd.add(cmd_args(index_file_out.as_output(), format = "-Wl,--thinlto-index-only={}"))
        index_cmd.add("-Wl,--thinlto-emit-imports-files")
        index_cmd.add("-Wl,--thinlto-full-index")

        # By default the linker will write artifacts (import files and sharded indices) next to input bitcode files with
        # a different suffix. This can be problematic if you are running two distributed links on the same machine at the # same time consuming the same input bitcode files. That is the links would overwrite each other's artifacts. This
        # flag allows you to write all these artifacts into a unique directory per link to avoid this problem.
        index_cmd.add(cmd_args(index_out_dir, format = "-Wl,--thinlto-prefix-replace=;{}/"))
        index_cmd.add(index_cmd_parts.post_linker_flags)

        index_meta_file = ctx.actions.write_json(
            output.basename + ".thinlto.meta.json",
            index_meta_records,
            with_inputs = True,
        )

        plan_cmd = cmd_args([lto_planner, "--meta", index_meta_file, "--index", index_out_dir, "--link-plan", outputs[link_plan].as_output(), "--final-link-index", outputs[final_link_index].as_output()])
        if premerger_enabled:
            plan_cmd.add("--enable-premerger")
        plan_cmd.add("--", index_cmd)

        plan_cmd.add(cmd_args(hidden = [
            index_args,
        ]))
        ctx.actions.run(plan_cmd, category = index_cat, identifier = identifier, local_only = _execute_link_actions_locally())

    def dynamic_plan(link_plan: Artifact, index_argsfile_out: Artifact, final_link_index: Artifact):
        # TODO(T117513091) - dynamic_output does not allow for an empty list of dynamic inputs. If we have no archives
        # to process, we will have no dynamic inputs, and the plan action can be non-dynamic.
        #
        # However, buck2 disallows `dynamic_output` with a empty input list. We also can't call our `plan` function
        # directly, since it uses `ctx.outputs` to bind its outputs. Instead of doing Starlark hacks to work around
        # the lack of `ctx.outputs`, we declare an empty file as a dynamic input.
        plan_inputs.append(ctx.actions.write(output.basename + ".plan_hack.txt", ""))
        plan_outputs.extend([link_plan.as_output(), index_argsfile_out.as_output(), final_link_index.as_output()])
        ctx.actions.dynamic_output(dynamic = plan_inputs, inputs = [], outputs = plan_outputs, f = lambda ctx, artifacts, outputs: plan(ctx, artifacts, outputs, link_plan))

    link_plan_out = ctx.actions.declare_output(output.basename + ".link-plan.json")
    dynamic_plan(link_plan = link_plan_out, index_argsfile_out = index_argsfile_out, final_link_index = final_link_index)

    def prepare_opt_flags(link_infos: list[LinkInfo]) -> cmd_args:
        opt_flags = cmd_args(cxx_toolchain.linker_info.dist_thin_lto_codegen_flags)
        opt_flags.add(extra_codegen_flags)
        for link in link_infos:
            opt_flags.add(link.dist_thin_lto_codegen_flags)
        return opt_flags

    common_opt_cmd = prepare_opt_flags(link_infos)

    # Create an argsfile and dump all the flags to be processed later by lto_opt.
    # These flags are common to all opt actions, we don't need an argfile for each action, one
    # for the entire link unit will do.
    opt_argsfile = ctx.actions.declare_output(output.basename + ".lto_opt_argsfile")
    ctx.actions.write(opt_argsfile.as_output(), common_opt_cmd, allow_args = True)

    def optimize_object(ctx: AnalysisContext, artifacts, outputs, name, initial_object, bc_file, plan, opt_object, merged_bc):
        optimization_plan = ObjectFileOptimizationPlan(**artifacts[plan].read_json())

        # If the object was not compiled with thinlto flags, then there
        # won't be valid outputs for it from the indexing, but we still
        # need to bind the artifact. Similarily, if a bitcode file is not
        # loaded by the indexing phase, or is absorbed by another module,
        # there is no point optimizing it.
        if not optimization_plan.loaded_by_linker or not optimization_plan.is_bitcode or optimization_plan.merge_state == BitcodeMergeState("ABSORBED").value:
            ctx.actions.write(outputs[opt_object].as_output(), "")
            return

        opt_cmd = cmd_args(lto_opt)
        if premerger_enabled:
            if optimization_plan.merge_state == BitcodeMergeState("STANDALONE").value:
                opt_cmd.add("--input", initial_object)
            elif optimization_plan.merge_state == BitcodeMergeState("ROOT").value:
                opt_cmd.add("--input", merged_bc)
            else:
                fail("Invalid merge state {} for bitcode file: {}".format(optimization_plan.merge_state, bc_file))
        else:
            opt_cmd.add("--input", initial_object)

        opt_cmd.add("--index", bc_file)

        opt_cmd.add(cmd_args(hidden = common_opt_cmd))
        opt_cmd.add("--args", opt_argsfile)
        opt_cmd.add("--compiler", cxx_toolchain.cxx_compiler_info.compiler)

        imported_input_bitcode_files = [sorted_index_link_data[idx].link_data.initial_object for idx in optimization_plan.imports]
        imported_archives_input_bitcode_files_directory = [sorted_index_link_data[idx].link_data.objects_dir for idx in optimization_plan.archive_imports]

        if premerger_enabled:
            imported_merged_input_bitcode_files = [sorted_index_link_data[idx].link_data.merged_bc for idx in optimization_plan.imports]
            imported_archives_merged_bitcode_files_directory = [sorted_index_link_data[idx].link_data.merged_bc_dir for idx in optimization_plan.archive_imports]
            opt_cmd.add(cmd_args(hidden = imported_merged_input_bitcode_files + imported_archives_merged_bitcode_files_directory))

        opt_cmd.add(cmd_args(hidden = imported_input_bitcode_files + imported_archives_input_bitcode_files_directory))
        projected_opt_cmd = cmd_args(ctx.actions.tset(IdentityTSet, value = opt_cmd).project_as_args("identity"))

        # We have to add outputs after wrapping the cmd_args in a projection
        projected_opt_cmd.add("--out", outputs[opt_object].as_output())
        ctx.actions.run(projected_opt_cmd, category = make_cat("thin_lto_opt_object"), identifier = name)

    # We declare a separate dynamic_output for every object file. It would
    # maybe be simpler to have a single dynamic_output that produced all the
    # opt actions, but an action needs to re-run whenever the analysis that
    # produced it re-runs. And so, with a single dynamic_output, we'd need to
    # re-run all actions when any of the plans changed.
    def dynamic_optimize(name: str, initial_object: Artifact, bc_file: Artifact, plan: Artifact, opt_object: Artifact, merged_bc: Artifact | None):
        ctx.actions.dynamic_output(dynamic = [plan], inputs = [], outputs = [opt_object.as_output()], f = lambda ctx, artifacts, outputs: optimize_object(ctx, artifacts, outputs, name, initial_object, bc_file, plan, opt_object, merged_bc))

    def optimize_archive(ctx: AnalysisContext, artifacts, outputs, archive):
        plan_json = artifacts[archive.plan].read_json()
        archive_optimization_plan = ArchiveOptimizationPlan(
            object_plans = [
                ArchiveMemberOptimizationPlan(**object_plan_json)
                for object_plan_json in plan_json["object_plans"]
            ],
            base_dir = plan_json["base_dir"],
        )
        if lazy.is_all(lambda e: not e.is_bitcode, archive_optimization_plan.object_plans):
            # Nothing in this directory was lto-able; let's just copy the archive.
            ctx.actions.copy_file(outputs[archive.opt_objects_dir], archive.objects_dir)
            return

        output_dir = {}
        for object_optimization_plan in archive_optimization_plan.object_plans:
            if not object_optimization_plan.loaded_by_linker:
                continue

            if premerger_enabled and object_optimization_plan.merge_state == BitcodeMergeState("ABSORBED").value:
                continue

            base_dir = archive_optimization_plan.base_dir
            source_path = paths.relativize(object_optimization_plan.path, base_dir)
            if not object_optimization_plan.is_bitcode:
                opt_object = ctx.actions.declare_output("%s/%s" % (make_cat("thin_lto_opt_copy"), source_path))
                copy_cmd = cmd_args([
                    lto_copy,
                    "--to",
                    opt_object.as_output(),
                    "--from",
                    object_optimization_plan.path,
                ], hidden = archive.objects_dir)
                ctx.actions.run(copy_cmd, category = make_cat("thin_lto_opt_copy"), identifier = source_path)
                output_dir[source_path] = opt_object
                continue

            opt_object = ctx.actions.declare_output("%s/%s" % (make_cat("thin_lto_opt_archive"), source_path))
            output_dir[source_path] = opt_object
            opt_cmd = cmd_args(lto_opt)
            if premerger_enabled and object_optimization_plan.merge_state == BitcodeMergeState("ROOT").value:
                opt_cmd.add("--input", object_optimization_plan.merged_bitcode_path)
            else:
                opt_cmd.add("--input", object_optimization_plan.path)
            opt_cmd.add("--index", object_optimization_plan.index_shard_file_path)

            opt_cmd.add(cmd_args(hidden = common_opt_cmd))
            opt_cmd.add("--args", opt_argsfile)
            opt_cmd.add("--compiler", cxx_toolchain.cxx_compiler_info.compiler)

            imported_input_bitcode_files = [sorted_index_link_data[idx].link_data.initial_object for idx in object_optimization_plan.imports]
            imported_archives_input_bitcode_files_directory = [sorted_index_link_data[idx].link_data.objects_dir for idx in object_optimization_plan.archive_imports]
            if premerger_enabled:
                imported_merged_input_bitcode_files = [sorted_index_link_data[idx].link_data.merged_bc for idx in object_optimization_plan.imports]
                imported_archives_merged_bitcode_files_directory = [sorted_index_link_data[idx].link_data.merged_bc_dir for idx in object_optimization_plan.archive_imports]
                opt_cmd.add(cmd_args(hidden = imported_merged_input_bitcode_files + imported_archives_merged_bitcode_files_directory + [archive.merged_bc_dir]))

            opt_cmd.add(cmd_args(
                hidden = imported_input_bitcode_files + imported_archives_input_bitcode_files_directory + [archive.indexes_dir, archive.objects_dir],
            ))

            projected_opt_cmd = cmd_args(ctx.actions.tset(IdentityTSet, value = opt_cmd).project_as_args("identity"))
            projected_opt_cmd.add("--out", opt_object.as_output())
            ctx.actions.run(
                projected_opt_cmd,
                category = make_cat("thin_lto_opt_archive"),
                identifier = source_path,
            )

        ctx.actions.symlinked_dir(outputs[archive.opt_objects_dir], output_dir)

    def dynamic_optimize_archive(archive: _ArchiveLinkData):
        archive_opt_inputs = [archive.plan]
        archive_opt_outputs = [archive.opt_objects_dir.as_output()]
        ctx.actions.dynamic_output(dynamic = archive_opt_inputs, inputs = [], outputs = archive_opt_outputs, f = lambda ctx, artifacts, outputs: optimize_archive(ctx, artifacts, outputs, archive))

    for artifact in sorted_index_link_data:
        link_data = artifact.link_data
        if artifact.data_type == _DataType("eager_bitcode") or artifact.data_type == _DataType("lazy_bitcode"):
            dynamic_optimize(
                name = link_data.name,
                initial_object = link_data.initial_object,
                bc_file = link_data.bc_file,
                plan = link_data.plan,
                opt_object = link_data.opt_object,
                merged_bc = link_data.merged_bc,
            )
        elif artifact.data_type == _DataType("archive"):
            dynamic_optimize_archive(link_data)

    linker_argsfile_out = ctx.actions.declare_output(output.basename + ".thinlto_link_argsfile")

    # Declare any extra outputs here so we can look them up in the final_link closure
    extra_outputs = opts.extra_linker_outputs_factory(ctx) if opts.extra_linker_outputs_factory else ExtraLinkerOutputs()

    def thin_lto_final_link(ctx: AnalysisContext, artifacts, outputs):
        plan = artifacts[link_plan_out].read_json()
        link_args = cmd_args()

        # non_lto_objects are the ones that weren't compiled with thinlto
        # flags. In that case, we need to link against the original object.
        non_lto_objects = {int(k): 1 for k in plan["non_lto_objects"]}
        opt_objects = []
        for flag in linker_flags:
            link_args.add(flag)

        for idx, artifact in enumerate(sorted_index_link_data):
            if artifact.data_type == _DataType("dynamic_library"):
                append_linkable_args(link_args, artifact.link_data.linkable)
            elif artifact.data_type == _DataType("eager_bitcode") or artifact.data_type == _DataType("lazy_bitcode"):
                if idx in non_lto_objects:
                    opt_objects.append(artifact.link_data.initial_object)
                else:
                    opt_objects.append(artifact.link_data.opt_object)

        link_cmd_parts = cxx_link_cmd_parts(cxx_toolchain, executable_link)
        link_cmd = link_cmd_parts.link_cmd
        link_cmd.add(common_link_flags)
        link_cmd_hidden = []

        if opts.extra_linker_outputs_flags_factory != None:
            # We need the inner artifacts here
            mapped_outputs = {output_type: outputs[artifact] for output_type, artifact in extra_outputs.artifacts.items()}
            link_cmd.add(opts.extra_linker_outputs_flags_factory(ctx, mapped_outputs))

        # buildifier: disable=uninitialized
        for artifact in sorted_index_link_data:
            if artifact.data_type == _DataType("archive"):
                link_cmd_hidden.append(artifact.link_data.opt_objects_dir)
                link_cmd_hidden.append(artifact.link_data.objects_dir)

        link_cmd.add(at_argfile(
            actions = ctx.actions,
            name = outputs[linker_argsfile_out],
            args = link_args,
            allow_args = True,
        ))
        link_cmd.add(cmd_args(final_link_index, format = "@{}"))
        link_cmd.add("-o", outputs[output].as_output())
        if linker_map:
            link_cmd.add(linker_map_args(cxx_toolchain, outputs[linker_map].as_output()).flags)
        link_cmd_hidden.extend([
            link_args,
            opt_objects,
        ])
        link_cmd.add(link_cmd_parts.post_linker_flags)
        link_cmd.add(cmd_args(hidden = link_cmd_hidden))

        ctx.actions.run(link_cmd, category = make_cat("thin_lto_link"), identifier = identifier, local_only = _execute_link_actions_locally())

    final_link_inputs = [link_plan_out, final_link_index]
    final_link_outputs = [output.as_output(), linker_argsfile_out.as_output()]
    if linker_map:
        final_link_outputs.append(linker_map.as_output())

    final_link_outputs += [o.as_output() for o in extra_outputs.artifacts.values()]

    ctx.actions.dynamic_output(
        dynamic = final_link_inputs,
        inputs = [],
        outputs = final_link_outputs,
        f = thin_lto_final_link,
    )

    final_output = output
    unstripped_output = output
    if opts.strip:
        strip_args = opts.strip_args_factory(ctx) if opts.strip_args_factory else cmd_args()
        final_output = strip_object(ctx, cxx_toolchain, final_output, strip_args, category_suffix)

    # dsym-util will create a dSYM from an executable by loading native object files pointed to
    # by paths embedded in the executable. We need to collect the artifacts representing these native
    # object files such that they can be provided as hidden dependencies to the dysm creation action.
    native_object_files_required_for_dsym_creation = []
    for artifact in sorted_index_link_data:
        # Without reading dynamic output, we cannot know if a given artifact represents bitcode to be included in LTO, or a native object file already (some inputs to a dthin-lto link are still native object files). We just include both the initial object (that may or may not be bitcode) and the produced bitcode file. dsym-util will only ever need one or the other, we just need to matieralize both.
        if artifact.data_type == _DataType("eager_bitcode") or artifact.data_type == _DataType("lazy_bitcode"):
            native_object_files_required_for_dsym_creation.append(artifact.link_data.initial_object)
            native_object_files_required_for_dsym_creation.append(artifact.link_data.opt_object)
        elif artifact.data_type == _DataType("archive"):
            native_object_files_required_for_dsym_creation.append(artifact.link_data.objects_dir)
            native_object_files_required_for_dsym_creation.append(artifact.link_data.opt_objects_dir)

    external_debug_info = make_artifact_tset(
        actions = ctx.actions,
        artifacts = native_object_files_required_for_dsym_creation,
        label = ctx.label,
        children = [
            unpack_external_debug_info(ctx.actions, link_args)
            for link_args in links
        ],
    )

    return LinkedObject(
        output = final_output,
        unstripped_output = unstripped_output,
        prebolt_output = output,
        dwp = None,
        external_debug_info = external_debug_info,
        linker_argsfile = linker_argsfile_out,
        linker_command = None,  # There is no notion of a single linker command for DistLTO
        index_argsfile = index_argsfile_out,
        dist_thin_lto_codegen_argsfile = opt_argsfile,
        dist_thin_lto_index_argsfile = index_flags_for_debugging_argsfile,
    ), extra_outputs.providers
