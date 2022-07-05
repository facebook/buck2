load("@fbcode//buck2/prelude:paths.bzl", "paths")
load(
    "@fbcode//buck2/prelude/cxx:cxx_bolt.bzl",
    "bolt",
    "cxx_use_bolt",
)
load(
    "@fbcode//buck2/prelude/cxx:cxx_link_utility.bzl",
    "cxx_link_cmd",
)
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    "@fbcode//buck2/prelude/cxx:dwp.bzl",
    "run_dwp_action",
)
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkArgs",
    "LinkableType",
    "LinkedObject",
    "ObjectsLinkable",
    "append_linkable_args",
    "map_to_link_infos",
)

def cxx_dist_link(
        ctx: "context",
        links: ["LinkArgs"],
        # The destination for the link output.
        output: "artifact",
        linker_map: ["artifact", None] = None,
        # A category suffix that will be added to the category of the link action that is generated.
        category_suffix: [str.type, None] = None,
        # An identifier that will uniquely name this link action in the context of a category. Useful for
        # differentiating multiple link actions in the same rule.
        identifier: [str.type, None] = None,
        # This action will only happen if split_dwarf is enabled via the toolchain.
        generate_dwp: bool.type = True,
        executable_link: bool.type = True) -> LinkedObject.type:
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

    def make_cat(c: str.type) -> str.type:
        """ Used to make sure categories for our actions include the provided suffix """
        if category_suffix != None:
            return c + "_" + category_suffix
        return c

    def make_id(i: str.type) -> str.type:
        """ Used to make sure identifiers for our actions include the provided identifier """
        if identifier != None:
            return identifier + "_" + i
        return i

    recorded_outputs = {}

    def name_for_obj(link_name: str.type, object_artifact: "artifact") -> str.type:
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

    def name_for_link(info: "LinkInfo") -> str.type:
        """ Creates a unique name for a LinkInfo that we are consuming """
        name = info.name or "unknown"
        if name not in names:
            names[name] = 1
        else:
            names[name] += 1
            name += "-{}".format(names[name])
        return make_id(name)

    links = [
        LinkArgs(
            tset = link.tset,
            flags = link.flags,
            infos = link.infos,
        )
        for link in links
    ]

    link_infos = map_to_link_infos(links)

    cxx_toolchain = ctx.attr._cxx_toolchain[CxxToolchainInfo]
    lto_planner = cxx_toolchain.dist_lto_tools_info.planner
    lto_opt = cxx_toolchain.dist_lto_tools_info.opt
    lto_prepare = cxx_toolchain.dist_lto_tools_info.prepare
    lto_copy = cxx_toolchain.dist_lto_tools_info.copy

    dwp_generation_enabled = (generate_dwp and cxx_toolchain.split_dwarf_enabled)
    LinkData = record(
        name = str.type,
        initial_object = "artifact",
        bc_file = "artifact",
        plan = "artifact",
        opt_object = "artifact",
    )

    ArchiveLinkData = record(
        name = str.type,
        manifest = "artifact",
        # A file containing paths to artifacts that are known to reside in opt_objects_dir.
        opt_manifest = "artifact",
        objects_dir = "artifact",
        opt_objects_dir = "artifact",
        indexes_dir = "artifact",
        plan = "artifact",
    )

    # The index into this array is important as it's how things will appear in
    # the plans produced by indexing. That allows us to map those indexes back to
    # the actual artifacts.
    objects = []
    archive_manifests = []
    object_link_args = []
    prepare_cat = make_cat("thin_lto_prepare")
    for link in link_infos:
        link_name = name_for_link(link)
        for linkable in link.linkables:
            if linkable._type == LinkableType("objects"):
                object_link_arg = cmd_args()

                # TODO(T113841827): @christylee enable link_groups for distributed_thinlto
                append_linkable_args(object_link_arg, linkable, use_link_groups = False)
                object_link_args.append(object_link_arg)
                for obj in linkable.objects:
                    name = name_for_obj(link_name, obj)
                    bc_output = ctx.actions.declare_output(name + ".thinlto.bc")
                    plan_output = ctx.actions.declare_output(name + ".opt.plan")
                    opt_output = ctx.actions.declare_output(name + ".opt.o")

                    data = LinkData(
                        name = name,
                        initial_object = obj,
                        bc_file = bc_output,
                        plan = plan_output,
                        opt_object = opt_output,
                    )
                    objects.append(data)
            elif linkable._type == LinkableType("archive"):
                # This linkable is known to not contain any lto-able artifacts. There's no need to inspect it any further.
                if linkable.do_not_inspect_for_thinlto:
                    continue

                # Our implementation of Distributed ThinLTO operates on individual objects, not archives. Since these
                # archives might still contain LTO-able bitcode, we first extract the objects within the archive into
                # another directory and write a "manifest" containing the list of objects that the archive contained.
                #
                # Later actions in the LTO compilation pipeline will read this manifest and dynamically dispatch
                # actions on the objects that the manifest reports.

                name = name_for_obj(link_name, linkable.archive.artifact)
                archive_manifest = ctx.actions.declare_output("%s/%s/manifest.json" % (prepare_cat, name))
                archive_objects = ctx.actions.declare_output("%s/%s/objects" % (prepare_cat, name))
                archive_opt_objects = ctx.actions.declare_output("%s/%s/opt_objects" % (prepare_cat, name))
                archive_indexes = ctx.actions.declare_output("%s/%s/indexes" % (prepare_cat, name))
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
                archive_manifests.append(ArchiveLinkData(
                    name = name,
                    manifest = archive_manifest,
                    opt_manifest = archive_opt_manifest,
                    objects_dir = archive_objects,
                    opt_objects_dir = archive_opt_objects,
                    indexes_dir = archive_indexes,
                    plan = archive_plan,
                ))

    index_argsfile_out = ctx.actions.declare_output(output.basename + ".thinlto.index.argsfile")

    def dynamic_plan(link_plan: "artifact", index_argsfile_out: "artifact"):
        def plan(ctx):
            index_args = cmd_args()

            # See comments in dist_lto_planner.py for semantics on the values that are pushed into index_meta.
            index_meta = cmd_args()
            for obj in objects:
                index_meta.add(obj.initial_object, ctx.outputs[obj.bc_file].as_output(), ctx.outputs[obj.plan].as_output(), "", "", "", "")

            for obj_link_args in object_link_args:
                index_args.add(obj_link_args)

            for idx, archive in enumerate(archive_manifests):
                manifest = ctx.artifacts[archive.manifest].read_json()
                if not manifest["objects"]:
                    # Despite not having any objects (and thus not needing a plan), we still need to bind the plan output.
                    ctx.actions.write(ctx.outputs[archive.plan].as_output(), "{}")
                    cmd = cmd_args(["/bin/sh", "-c", "mkdir", "-p", ctx.outputs[archive.indexes_dir].as_output()])
                    ctx.actions.run(cmd, category = make_cat("thin_lto_mkdir"), identifier = archive.name)
                    continue

                index_args.hidden(archive.objects_dir)
                index_args.add("-Wl,--start-lib")
                for obj in manifest["objects"]:
                    index_meta.add(obj, "", "", str(idx), archive.name, ctx.outputs[archive.plan].as_output(), ctx.outputs[archive.indexes_dir].as_output())
                    index_args.add(obj)
                index_args.add("-Wl,--end-lib")
                index_args.hidden(archive.objects_dir)

            index_argfile, index_argfile_inputs = ctx.actions.write(
                ctx.outputs[index_argsfile_out].as_output(),
                index_args,
                allow_args = True,
            )

            index_cat = make_cat("thin_lto_index")
            index_file_out = ctx.actions.declare_output(make_id(index_cat) + "/index")
            index_out = cmd_args(index_file_out.as_output()).parent()

            index_cmd = cxx_link_cmd(ctx)
            index_cmd.add(cmd_args(index_argfile, format = "@{}"))

            output_as_string = cmd_args(output)
            output_as_string.ignore_artifacts()
            index_cmd.add("-o", output_as_string)
            index_cmd.add(cmd_args(index_file_out.as_output(), format = "-Wl,--thinlto-index-only={}"))
            index_cmd.add("-Wl,--thinlto-emit-imports-files")
            index_cmd.add(cmd_args(index_out, format = "-Wl,--thinlto-prefix-replace=;{}/"))

            # Terminate the index file with a newline.
            index_meta.add("")
            index_meta_file = ctx.actions.write(
                output.basename + ".thinlto.meta",
                index_meta,
            )

            plan_cmd = cmd_args([lto_planner, "--meta", index_meta_file, "--index", index_out, "--link-plan", ctx.outputs[link_plan].as_output(), "--"])
            plan_cmd.add(index_cmd)

            plan_extra_inputs = cmd_args()
            plan_extra_inputs.add(index_meta)
            plan_extra_inputs.add(index_args)
            plan_extra_inputs.add(index_argfile_inputs)
            plan_cmd.hidden(plan_extra_inputs)

            ctx.actions.run(plan_cmd, category = index_cat, identifier = identifier, local_only = True)

        todo_inputs = []
        plan_inputs = []
        plan_outputs = [link_plan]
        for artifact in archive_manifests:
            plan_inputs.append(artifact.manifest)
            plan_inputs.append(artifact.objects_dir)
            plan_outputs.append(artifact.plan)
            plan_outputs.append(artifact.indexes_dir)
        for obj in objects:
            plan_outputs.append(obj.bc_file)
            plan_outputs.append(obj.plan)

        # TODO(T117513091) - dynamic_output does not allow for an empty list of dynamic inputs. If we have no archives
        # to process, we will have no dynamic inputs, and the plan action can be non-dynamic.
        #
        # However, buck2 disallows `dynamic_output` with a empty input list. We also can't call our `plan` function
        # directly, since it uses `ctx.outputs` to bind its outputs. Instead of doing Starlark hacks to work around
        # the lack of `ctx.outputs`, we declare an empty file as a dynamic input.
        plan_inputs.append(ctx.actions.write("plan_hack.txt", ""))
        ctx.actions.dynamic_output(plan_inputs, todo_inputs, plan_outputs + [index_argsfile_out], plan)

    link_plan_out = ctx.actions.declare_output(output.basename + ".link-plan.json")
    dynamic_plan(link_plan = link_plan_out, index_argsfile_out = index_argsfile_out)

    # We declare a separate dynamic_output for every object file. It would
    # maybe be simpler to have a single dynamic_output that produced all the
    # opt actions, but an action needs to re-run whenever the analysis that
    # produced it re-runs. And so, with a single dynamic_output, we'd need to
    # re-run all actions when any of the plans changed.
    def dynamic_optimize(name: str.type, initial_object: "artifact", bc_file: "artifact", plan: "artifact", opt_object: "artifact"):
        def optimize_object(ctx):
            plan_json = ctx.artifacts[plan].read_json()

            # If the object was not compiled with thinlto flags, then there
            # won't be valid outputs for it from the indexing, but we still
            # need to bind the artifact.
            if not plan_json["is_lto"]:
                ctx.actions.write(ctx.outputs[opt_object], "")
                return

            opt_cmd = cmd_args(lto_opt)
            opt_cmd.add("--out", ctx.outputs[opt_object].as_output())
            opt_cmd.add("--")
            opt_cmd.add(cxx_toolchain.cxx_compiler_info.compiler)

            # TODO(cjhopman): we need to move the thinlto opt flags into the
            # toolchain somewhere. For now, it's just hardcoded '-O3'.
            opt_cmd.add("-O3")
            opt_cmd.add("-o", ctx.outputs[opt_object].as_output())
            opt_cmd.add("-x", "ir", initial_object)
            opt_cmd.add("-c")
            opt_cmd.add(cmd_args(bc_file, format = "-fthinlto-index={}"))

            # When invoking opt and llc via clang, clang will not respect IR metadata to generate
            # dwo files unless -gsplit-dwarf is explicitly passed in. In other words, even if
            # 'splitDebugFilename' set in IR 'DICompileUnit', we still need to force clang to tell
            # llc to generate dwo sections.
            # Note that -gsplit-dwarf will not do anything if the IR is not compiled with
            # -gsplit-dwarf. I.e. no dwo sections will be generated if IR metadata contains
            # 'splitDebugInlining: false' in 'DICompileUnit'.
            if dwp_generation_enabled:
                opt_cmd.add("-gsplit-dwarf=single")

            imports = [objects[idx].initial_object for idx in plan_json["imports"]]
            archives = [archive_manifests[idx].objects_dir for idx in plan_json["archive_imports"]]
            opt_cmd.hidden(imports)
            opt_cmd.hidden(archives)
            ctx.actions.run(opt_cmd, category = make_cat("thin_lto_opt"), identifier = name)

        todo_inputs = []
        ctx.actions.dynamic_output([plan], todo_inputs, [opt_object], optimize_object)

    def dynamic_optimize_archive(archive: ArchiveLinkData.type):
        def optimize_archive(ctx):
            plan_json = ctx.artifacts[archive.plan].read_json()
            if "objects" not in plan_json or not plan_json["objects"]:
                # Nothing in this directory was lto-able; let's just copy the archive.
                ctx.actions.copy_file(ctx.outputs[archive.opt_objects_dir], archive.objects_dir)
                ctx.actions.write(ctx.outputs[archive.opt_manifest], "")
                return

            output_dir = {}
            output_manifest = cmd_args()
            for entry in plan_json["objects"]:
                base_dir = plan_json["base_dir"]
                source_path = paths.relativize(entry["path"], base_dir)
                if not entry["is_lto"]:
                    opt_object = ctx.actions.declare_output("%s/%s" % (make_cat("thin_lto_opt_copy"), source_path))
                    output_manifest.add(opt_object)
                    copy_cmd = cmd_args([
                        lto_copy,
                        "--to",
                        opt_object.as_output(),
                        "--from",
                        entry["path"],
                    ])

                    copy_cmd.hidden(archive.objects_dir)
                    ctx.actions.run(copy_cmd, category = make_cat("thin_lto_opt_copy"), identifier = source_path)
                    output_dir[source_path] = opt_object
                    continue

                opt_object = ctx.actions.declare_output("%s/%s" % (make_cat("thin_lto_opt"), source_path))
                output_manifest.add(opt_object)
                output_dir[source_path] = opt_object
                opt_cmd = cmd_args(lto_opt)
                opt_cmd.add("--out", opt_object.as_output())
                opt_cmd.add("--")
                opt_cmd.add(cxx_toolchain.cxx_compiler_info.compiler)
                opt_cmd.add("-O3")
                opt_cmd.add("-o", opt_object.as_output())
                opt_cmd.add("-x", "ir", entry["path"])
                opt_cmd.add("-c")
                opt_cmd.add(cmd_args(entry["bitcode_file"], format = "-fthinlto-index={}"))

                imports = [objects[idx].initial_object for idx in entry["imports"]]
                archives = [archive_manifests[idx].objects_dir for idx in entry["archive_imports"]]
                opt_cmd.hidden(imports)
                opt_cmd.hidden(archives)
                opt_cmd.hidden(archive.indexes_dir)
                opt_cmd.hidden(archive.objects_dir)
                ctx.actions.run(opt_cmd, category = make_cat("thin_lto_opt"), identifier = source_path)

            ctx.actions.symlinked_dir(ctx.outputs[archive.opt_objects_dir], output_dir)
            ctx.actions.write(ctx.outputs[archive.opt_manifest], output_manifest, allow_args = True)

        todo_inputs = []
        archive_opt_inputs = [archive.plan]
        archive_opt_outputs = [archive.opt_objects_dir, archive.opt_manifest]
        ctx.actions.dynamic_output(archive_opt_inputs, todo_inputs, archive_opt_outputs, optimize_archive)

    for item in objects:
        # TODO(cjhopman): This copy works around a buck bug where we don't
        # get early cutoff if a non-consumed output of an action dependency
        # changes.
        copied_bc_file = ctx.actions.declare_output(item.bc_file.short_path + ".copied")
        copied_plan = ctx.actions.declare_output(item.plan.short_path + ".copied")
        ctx.actions.copy_file(copied_bc_file, item.bc_file)
        ctx.actions.copy_file(copied_plan, item.plan)

        dynamic_optimize(
            name = item.name,
            initial_object = item.initial_object,
            bc_file = copied_bc_file,
            plan = copied_plan,
            opt_object = item.opt_object,
        )

    for archive in archive_manifests:
        dynamic_optimize_archive(archive)

    linker_argsfile_out = ctx.actions.declare_output(output.basename + ".thinlto.link.argsfile")

    def thin_lto_final_link(ctx):
        plan = ctx.artifacts[link_plan_out].read_json()
        link_args = cmd_args()
        plan_index = {int(k): v for k, v in plan["index"].items()}

        # non_lto_objects are the ones that weren't compiled with thinlto
        # flags. In that case, we need to link against the original object.
        non_lto_objects = {int(k): 1 for k in plan["non_lto_objects"]}
        current_index = 0
        current_archive_index = 0
        opt_objects = []
        archives = []
        for link in link_infos:
            link_args.add(link.pre_flags)
            for linkable in link.linkables:
                if linkable._type == LinkableType("objects"):
                    new_objs = []
                    for obj in linkable.objects:
                        if current_index in plan_index:
                            new_objs.append(objects[current_index].opt_object)
                            opt_objects.append(objects[current_index].opt_object)
                        elif current_index in non_lto_objects:
                            new_objs.append(obj)
                            opt_objects.append(obj)
                        current_index += 1
                    linkable = ObjectsLinkable(
                        objects = new_objs,
                        linker_type = linkable.linker_type,
                        link_whole = linkable.link_whole,
                    )

                    # TODO(T113841827): @christylee enable link_groups for distributed_thinlto
                    append_linkable_args(link_args, linkable, use_link_groups = False)
                elif linkable._type == LinkableType("archive"):
                    if linkable.do_not_inspect_for_thinlto:
                        append_linkable_args(link_args, linkable, use_link_groups = False)
                        archives.append(linkable.archive.artifact)
                        continue

                    manifest = archive_manifests[current_archive_index]
                    current_archive_index += 1
                    opt_manifest = ctx.artifacts[manifest.opt_manifest].read_string().splitlines()
                    link_args.add("-Wl,--start-lib")
                    for line in opt_manifest:
                        link_args.add(line)
                    link_args.add("-Wl,--end-lib")
                else:
                    # TODO(T113841827): @christylee enable link_groups for distributed_thinlto
                    append_linkable_args(link_args, linkable, use_link_groups = False)
            link_args.add(link.post_flags)

        link_cmd = cxx_link_cmd(ctx)
        final_link_argfile, final_link_inputs = ctx.actions.write(
            ctx.outputs[linker_argsfile_out].as_output(),
            link_args,
            allow_args = True,
        )

        for archive in archive_manifests:
            link_cmd.hidden(archive.opt_objects_dir)
        link_cmd.add(cmd_args(final_link_argfile, format = "@{}"))
        link_cmd.add("-o", ctx.outputs[output].as_output())
        link_cmd_extra_inputs = cmd_args()
        link_cmd_extra_inputs.add(final_link_inputs)
        link_cmd.hidden(link_cmd_extra_inputs)
        link_cmd.hidden(link_args)
        link_cmd.hidden(opt_objects)
        link_cmd.hidden(archives)

        ctx.actions.run(link_cmd, category = make_cat("thin_lto_link"), identifier = identifier, local_only = True)

    todo_inputs = []
    final_link_inputs = [link_plan_out] + [archive.opt_manifest for archive in archive_manifests]
    ctx.actions.dynamic_output(
        final_link_inputs,
        todo_inputs,
        [output] + ([linker_map] if linker_map else []) + [linker_argsfile_out],
        thin_lto_final_link,
    )

    final_output = output if not (executable_link and cxx_use_bolt(ctx)) else bolt(ctx, output, identifier)
    dwp_output = ctx.actions.declare_output(output.short_path.removesuffix("-wrapper") + ".dwp") if dwp_generation_enabled else None

    if dwp_generation_enabled:
        run_dwp_action(
            ctx = ctx,
            obj = final_output,
            identifier = identifier,
            category_suffix = category_suffix,
            referenced_objects = final_link_inputs,
            dwp_output = dwp_output,
        )

    return LinkedObject(
        output = final_output,
        prebolt_output = output,
        dwp = dwp_output,
        linker_argsfile = linker_argsfile_out,
        index_argsfile = index_argsfile_out,
    )
