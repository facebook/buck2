# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",
)
load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load(
    "@prelude//cxx:cxx_link_utility.bzl",
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
    "FrameworksLinkable",  # @unused Used as a type
    "LinkInfo",
    "LinkedObject",
    "ObjectsLinkable",
    "SharedLibLinkable",  # @unused Used as a type
    "SwiftRuntimeLinkable",  # @unused Used as a type
    "SwiftmoduleLinkable",  # @unused Used as a type
    "append_linkable_args",
    "map_to_link_infos",
)
load("@prelude//linking:strip.bzl", "strip_object")
load("@prelude//utils:argfile.bzl", "at_argfile")
load("@prelude//utils:lazy.bzl", "lazy")

_BitcodeLinkData = record(
    name = str,
    initial_object = Artifact,
    bc_file = Artifact,
    plan = Artifact,
    opt_object = Artifact,
)

_ArchiveLinkData = record(
    name = str,
    manifest = Artifact,
    # A file containing paths to artifacts that are known to reside in opt_objects_dir.
    opt_manifest = Artifact,
    objects_dir = Artifact,
    opt_objects_dir = Artifact,
    indexes_dir = Artifact,
    plan = Artifact,
    link_whole = bool,
)

_DynamicLibraryLinkData = record(
    linkable = SharedLibLinkable,
)

_DataType = enum(
    "bitcode",
    "archive",
    "dynamic_library",
)

_IndexLinkData = record(
    data_type = _DataType,
    link_data = field([_BitcodeLinkData, _ArchiveLinkData, _DynamicLibraryLinkData]),
)

def cxx_darwin_dist_link(
        ctx: AnalysisContext,
        # The destination for the link output.
        output: Artifact,
        opts: LinkOptions,
        linker_map: Artifact | None = None) -> LinkedObject:
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
    lto_prepare = cxx_toolchain.internal_tools.dist_lto.prepare
    lto_copy = cxx_toolchain.internal_tools.dist_lto.copy

    unsorted_index_link_data = []
    linker_flags = []
    common_link_flags = cmd_args(get_target_sdk_version_flags(ctx), get_extra_darwin_linker_flags())
    extra_codegen_flags = get_target_sdk_version_flags(ctx)

    # Information used to construct the dynamic plan:
    plan_inputs = []
    plan_outputs = []

    # Information used to construct the opt dynamic outputs:
    archive_opt_manifests = []

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

                    data = _IndexLinkData(
                        data_type = _DataType("bitcode"),
                        link_data = _BitcodeLinkData(
                            name = name,
                            initial_object = obj,
                            bc_file = bc_output,
                            plan = plan_output,
                            opt_object = opt_output,
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
                archive_opt_manifest = ctx.actions.declare_output("%s/%s/opt_objects.manifest" % (prepare_cat, name))
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
                ])
                ctx.actions.run(prepare_args, category = make_cat("thin_lto_prepare"), identifier = name)

                data = _IndexLinkData(
                    data_type = _DataType("archive"),
                    link_data = _ArchiveLinkData(
                        name = name,
                        manifest = archive_manifest,
                        opt_manifest = archive_opt_manifest,
                        objects_dir = archive_objects,
                        opt_objects_dir = archive_opt_objects,
                        indexes_dir = archive_indexes,
                        plan = archive_plan,
                        link_whole = linkable.link_whole,
                    ),
                )
                unsorted_index_link_data.append(data)
                archive_opt_manifests.append(archive_opt_manifest)
                plan_inputs.extend([archive_manifest, archive_objects])
                plan_outputs.extend([archive_indexes.as_output(), archive_plan.as_output()])
            elif isinstance(linkable, SharedLibLinkable):
                data = _IndexLinkData(
                    data_type = _DataType("dynamic_library"),
                    link_data = _DynamicLibraryLinkData(linkable = linkable),
                )
                unsorted_index_link_data.append(data)
            elif isinstance(linkable, FrameworksLinkable) or isinstance(linkable, SwiftRuntimeLinkable) or isinstance(linkable, SwiftmoduleLinkable):
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
            if link_data.data_type == _DataType("bitcode"):
                object_files.append(link_data)
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

    def prepare_index_flags(include_inputs: bool, index_args_out: cmd_args, index_meta_args_out: cmd_args, ctx: AnalysisContext, artifacts, outputs):
        for flag in linker_flags:
            index_args_out.add(flag)

        if include_inputs:
            # buildifier: disable=uninitialized
            for idx, artifact in enumerate(sorted_index_link_data):
                link_data = artifact.link_data

                if artifact.data_type == _DataType("bitcode"):
                    index_args_out.add(link_data.initial_object)
                    index_meta_args_out.add(link_data.initial_object, outputs[link_data.bc_file].as_output(), outputs[link_data.plan].as_output(), str(idx), "", "", "")

                elif artifact.data_type == _DataType("archive"):
                    manifest = artifacts[link_data.manifest].read_json()

                    if not manifest["objects"]:
                        # Despite not having any objects (and thus not needing a plan), we still need to bind the plan output.
                        ctx.actions.write(outputs[link_data.plan].as_output(), "{}")
                        cmd = cmd_args(["/bin/sh", "-c", "mkdir", "-p", outputs[link_data.indexes_dir].as_output()])
                        ctx.actions.run(cmd, category = make_cat("thin_lto_mkdir"), identifier = link_data.name)
                        continue

                    index_args_out.add(cmd_args(hidden = link_data.objects_dir))

                    if not link_data.link_whole:
                        index_args_out.add("-Wl,--start-lib")

                    for obj in manifest["objects"]:
                        index_meta_args_out.add(obj, "", "", str(idx), link_data.name, outputs[link_data.plan].as_output(), outputs[link_data.indexes_dir].as_output())
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
    index_cmd_parts = cxx_link_cmd_parts(cxx_toolchain)
    index_flags_for_debugging.add(index_cmd_parts.linker_flags)
    index_flags_for_debugging.add(common_link_flags)
    index_flags_for_debugging.add(index_cmd_parts.post_linker_flags)
    prepare_index_flags(include_inputs = False, index_args_out = index_flags_for_debugging, index_meta_args_out = cmd_args(), ctx = ctx, artifacts = None, outputs = None)
    index_flags_for_debugging_argsfile, _ = ctx.actions.write(output.basename + ".thinlto_index_debugging_argsfile", index_flags_for_debugging, allow_args = True)

    def dynamic_plan(link_plan: Artifact, index_argsfile_out: Artifact, final_link_index: Artifact):
        def plan(ctx: AnalysisContext, artifacts, outputs):
            # index link command args
            index_args = cmd_args()

            # See comments in dist_lto_planner.py for semantics on the values that are pushed into index_meta.
            index_meta = cmd_args()

            prepare_index_flags(include_inputs = True, index_args_out = index_args, index_meta_args_out = index_meta, ctx = ctx, artifacts = artifacts, outputs = outputs)

            index_argfile, _ = ctx.actions.write(
                outputs[index_argsfile_out].as_output(),
                index_args,
                allow_args = True,
            )

            index_cat = make_cat("thin_lto_index")
            index_file_out = ctx.actions.declare_output(make_id(index_cat) + "/index")
            index_out_dir = cmd_args(index_file_out.as_output(), parent = 1)

            index_cmd_parts = cxx_link_cmd_parts(cxx_toolchain)

            index_cmd = index_cmd_parts.link_cmd
            index_cmd.add(common_link_flags)
            index_cmd.add(cmd_args(index_argfile, format = "@{}"))

            index_cmd.add(cmd_args(index_file_out.as_output(), format = "-Wl,--thinlto-index-only={}"))
            index_cmd.add("-Wl,--thinlto-emit-imports-files")
            index_cmd.add("-Wl,--thinlto-full-index")
            index_cmd.add(cmd_args(index_out_dir, format = "-Wl,--thinlto-prefix-replace=;{}/"))
            index_cmd.add(index_cmd_parts.post_linker_flags)

            # Terminate the index file with a newline.
            index_meta.add("")
            index_meta_file = ctx.actions.write(
                output.basename + ".thinlto.meta",
                index_meta,
            )

            plan_cmd = cmd_args([lto_planner, "--meta", index_meta_file, "--index", index_out_dir, "--link-plan", outputs[link_plan].as_output(), "--final-link-index", outputs[final_link_index].as_output(), "--"])
            plan_cmd.add(index_cmd)

            plan_cmd.add(cmd_args(hidden = [
                index_meta,
                index_args,
            ]))

            ctx.actions.run(plan_cmd, category = index_cat, identifier = identifier, local_only = True)

        # TODO(T117513091) - dynamic_output does not allow for an empty list of dynamic inputs. If we have no archives
        # to process, we will have no dynamic inputs, and the plan action can be non-dynamic.
        #
        # However, buck2 disallows `dynamic_output` with a empty input list. We also can't call our `plan` function
        # directly, since it uses `ctx.outputs` to bind its outputs. Instead of doing Starlark hacks to work around
        # the lack of `ctx.outputs`, we declare an empty file as a dynamic input.
        plan_inputs.append(ctx.actions.write(output.basename + ".plan_hack.txt", ""))
        plan_outputs.extend([link_plan.as_output(), index_argsfile_out.as_output(), final_link_index.as_output()])
        ctx.actions.dynamic_output(dynamic = plan_inputs, inputs = [], outputs = plan_outputs, f = plan)

    link_plan_out = ctx.actions.declare_output(output.basename + ".link-plan.json")
    dynamic_plan(link_plan = link_plan_out, index_argsfile_out = index_argsfile_out, final_link_index = final_link_index)

    def prepare_opt_flags(link_infos: list[LinkInfo]) -> cmd_args:
        opt_flags = cmd_args(cxx_toolchain.linker_info.dist_thin_lto_codegen_flags)
        opt_flags.add(extra_codegen_flags)
        for link in link_infos:
            opt_flags.add(link.dist_thin_lto_codegen_flags)
        return opt_flags

    common_opt_cmd = cmd_args(cxx_toolchain.linker_info.linker)
    common_opt_cmd.add(prepare_opt_flags(link_infos))

    # Create an argsfile and dump all the flags to be processed later by lto_opt.
    # These flags are common to all opt actions, we don't need an argfile for each action, one
    # for the entire link unit will do.
    opt_argsfile = ctx.actions.declare_output(output.basename + ".lto_opt_argsfile")
    ctx.actions.write(opt_argsfile.as_output(), common_opt_cmd, allow_args = True)

    # We don't want the linker itself in the argsfile for debugging / testing codegen flags
    opt_flags_for_debugging = prepare_opt_flags(link_infos)
    opt_flags_for_debugging_argsfile = ctx.actions.declare_output(output.basename + ".thin_lto_codegen_debugging_argsfile")
    ctx.actions.write(opt_flags_for_debugging_argsfile.as_output(), opt_flags_for_debugging, allow_args = True)

    # We declare a separate dynamic_output for every object file. It would
    # maybe be simpler to have a single dynamic_output that produced all the
    # opt actions, but an action needs to re-run whenever the analysis that
    # produced it re-runs. And so, with a single dynamic_output, we'd need to
    # re-run all actions when any of the plans changed.
    def dynamic_optimize(name: str, initial_object: Artifact, bc_file: Artifact, plan: Artifact, opt_object: Artifact):
        def optimize_object(ctx: AnalysisContext, artifacts, outputs):
            plan_json = artifacts[plan].read_json()

            # If the object was not compiled with thinlto flags, then there
            # won't be valid outputs for it from the indexing, but we still
            # need to bind the artifact. Similarily, if a bitcode file is not
            # loaded by the indexing phase, there is no point optimizing it.
            if "not_loaded_by_linker" in plan_json or not plan_json["is_bc"]:
                ctx.actions.write(outputs[opt_object], "")
                return

            opt_cmd = cmd_args(lto_opt)
            opt_cmd.add("--out", outputs[opt_object].as_output())
            opt_cmd.add("--input", initial_object)
            opt_cmd.add("--index", bc_file)

            opt_cmd.add(cmd_args(hidden = common_opt_cmd))
            opt_cmd.add("--args", opt_argsfile)

            opt_cmd.add("--")
            opt_cmd.add(cxx_toolchain.cxx_compiler_info.compiler)

            imports = [sorted_index_link_data[idx].link_data.initial_object for idx in plan_json["imports"]]
            archives = [sorted_index_link_data[idx].link_data.objects_dir for idx in plan_json["archive_imports"]]
            opt_cmd.add(cmd_args(hidden = imports + archives))
            ctx.actions.run(opt_cmd, category = make_cat("thin_lto_opt_object"), identifier = name)

        ctx.actions.dynamic_output(dynamic = [plan], inputs = [], outputs = [opt_object.as_output()], f = optimize_object)

    def dynamic_optimize_archive(archive: _ArchiveLinkData):
        def optimize_archive(ctx: AnalysisContext, artifacts, outputs):
            plan_json = artifacts[archive.plan].read_json()
            if "objects" not in plan_json or not plan_json["objects"] or lazy.is_all(lambda e: not e["is_bc"], plan_json["objects"]):
                # Nothing in this directory was lto-able; let's just copy the archive.
                ctx.actions.copy_file(outputs[archive.opt_objects_dir], archive.objects_dir)
                ctx.actions.write(outputs[archive.opt_manifest], "")
                return

            output_dir = {}
            output_manifest = cmd_args()
            for entry in plan_json["objects"]:
                if "not_loaded_by_linker" in entry:
                    continue

                base_dir = plan_json["base_dir"]
                source_path = paths.relativize(entry["path"], base_dir)
                if not entry["is_bc"]:
                    opt_object = ctx.actions.declare_output("%s/%s" % (make_cat("thin_lto_opt_copy"), source_path))
                    output_manifest.add(opt_object)
                    copy_cmd = cmd_args([
                        lto_copy,
                        "--to",
                        opt_object.as_output(),
                        "--from",
                        entry["path"],
                    ], hidden = archive.objects_dir)
                    ctx.actions.run(copy_cmd, category = make_cat("thin_lto_opt_copy"), identifier = source_path)
                    output_dir[source_path] = opt_object
                    continue

                opt_object = ctx.actions.declare_output("%s/%s" % (make_cat("thin_lto_opt_archive"), source_path))
                output_manifest.add(opt_object)
                output_dir[source_path] = opt_object
                opt_cmd = cmd_args(lto_opt)
                opt_cmd.add("--out", opt_object.as_output())
                opt_cmd.add("--input", entry["path"])
                opt_cmd.add("--index", entry["bitcode_file"])

                opt_cmd.add(cmd_args(hidden = common_opt_cmd))
                opt_cmd.add("--args", opt_argsfile)

                opt_cmd.add("--")
                opt_cmd.add(cxx_toolchain.cxx_compiler_info.compiler)

                imports = [sorted_index_link_data[idx].link_data.initial_object for idx in entry["imports"]]
                archives = [sorted_index_link_data[idx].link_data.objects_dir for idx in entry["archive_imports"]]
                opt_cmd.add(cmd_args(
                    hidden = imports + archives + [archive.indexes_dir, archive.objects_dir],
                ))
                ctx.actions.run(opt_cmd, category = make_cat("thin_lto_opt_archive"), identifier = source_path)

            ctx.actions.symlinked_dir(outputs[archive.opt_objects_dir], output_dir)
            ctx.actions.write(outputs[archive.opt_manifest], output_manifest, allow_args = True)

        archive_opt_inputs = [archive.plan]
        archive_opt_outputs = [archive.opt_objects_dir.as_output(), archive.opt_manifest.as_output()]
        ctx.actions.dynamic_output(dynamic = archive_opt_inputs, inputs = [], outputs = archive_opt_outputs, f = optimize_archive)

    for artifact in sorted_index_link_data:
        link_data = artifact.link_data
        if artifact.data_type == _DataType("bitcode"):
            dynamic_optimize(
                name = link_data.name,
                initial_object = link_data.initial_object,
                bc_file = link_data.bc_file,
                plan = link_data.plan,
                opt_object = link_data.opt_object,
            )
        elif artifact.data_type == _DataType("archive"):
            dynamic_optimize_archive(link_data)

    linker_argsfile_out = ctx.actions.declare_output(output.basename + ".thinlto_link_argsfile")

    def thin_lto_final_link(ctx: AnalysisContext, artifacts, outputs):
        plan = artifacts[link_plan_out].read_json()
        link_args = cmd_args()
        plan_index = {int(k): v for k, v in plan["index"].items()}

        # non_lto_objects are the ones that weren't compiled with thinlto
        # flags. In that case, we need to link against the original object.
        non_lto_objects = {int(k): 1 for k in plan["non_lto_objects"]}
        opt_objects = []
        for flag in linker_flags:
            link_args.add(flag)

        for idx, artifact in enumerate(sorted_index_link_data):
            if artifact.data_type == _DataType("dynamic_library"):
                append_linkable_args(link_args, artifact.link_data.linkable)
            elif artifact.data_type == _DataType("bitcode"):
                if idx in plan_index:
                    opt_objects.append(artifact.link_data.opt_object)
                elif idx in non_lto_objects:
                    opt_objects.append(artifact.link_data.initial_object)

        link_cmd_parts = cxx_link_cmd_parts(cxx_toolchain)
        link_cmd = link_cmd_parts.link_cmd
        link_cmd.add(common_link_flags)
        link_cmd_hidden = []

        # buildifier: disable=uninitialized
        for artifact in sorted_index_link_data:
            if artifact.data_type == _DataType("archive"):
                link_cmd_hidden.append(artifact.link_data.opt_objects_dir)
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

        ctx.actions.run(link_cmd, category = make_cat("thin_lto_link"), identifier = identifier, local_only = True)

    final_link_inputs = [link_plan_out, final_link_index] + archive_opt_manifests
    ctx.actions.dynamic_output(
        dynamic = final_link_inputs,
        inputs = [],
        outputs = [output.as_output()] + ([linker_map.as_output()] if linker_map else []) + [linker_argsfile_out.as_output()],
        f = thin_lto_final_link,
    )

    final_output = output
    unstripped_output = output
    if opts.strip:
        strip_args = opts.strip_args_factory(ctx) if opts.strip_args_factory else cmd_args()
        final_output = strip_object(ctx, cxx_toolchain, final_output, strip_args, category_suffix)

    return LinkedObject(
        output = final_output,
        unstripped_output = unstripped_output,
        prebolt_output = output,
        dwp = None,
        external_debug_info = ArtifactTSet(),
        linker_argsfile = linker_argsfile_out,
        linker_filelist = None,  # DistLTO doesn't use filelists
        linker_command = None,  # There is no notion of a single linker command for DistLTO
        index_argsfile = index_argsfile_out,
        dist_thin_lto_codegen_argsfile = opt_flags_for_debugging_argsfile,
        dist_thin_lto_index_argsfile = index_flags_for_debugging_argsfile,
    )
