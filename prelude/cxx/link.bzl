load(
    "@prelude//cxx:cxx_bolt.bzl",
    "bolt",
    "cxx_use_bolt",
)
load(
    "@prelude//cxx/dist_lto:dist_lto.bzl",
    "cxx_dist_link",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkedObject",
    "unpack_link_args",
)
load("@prelude//linking:link_postprocessor.bzl", "postprocess")
load("@prelude//linking:strip.bzl", "strip_shared_library")
load("@prelude//utils:utils.bzl", "value_or")
load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(
    ":cxx_link_utility.bzl",
    "cxx_link_cmd",
    "linker_map_args",
    "make_link_args",
)
load(":dwp.bzl", "dwp", "dwp_available")
load(
    ":linker.bzl",
    "SharedLibraryFlagOverrides",  # @unused Used as a type
    "get_shared_library_flags",
    "get_shared_library_name_linker_flags",
)

# Actually perform a link into the supplied output.
def cxx_link(
        ctx: "context",
        links: [LinkArgs.type],
        # The destination for the link output.
        output: "artifact",
        linker_map: ["artifact", None] = None,
        prefer_local: bool.type = False,
        local_only: bool.type = False,
        link_weight: int.type = 1,
        enable_distributed_thinlto: bool.type = False,
        # A category suffix that will be added to the category of the link action that is generated.
        category_suffix: [str.type, None] = None,
        # An identifier that will uniquely name this link action in the context of a category. Useful for
        # differentiating multiple link actions in the same rule.
        identifier: [str.type, None] = None,
        is_shared: bool.type = False,
        strip: bool.type = False,
        # A function/lambda which will generate the strip args using the ctx.
        strip_args_factory = None,
        generate_dwp: bool.type = True,
        executable_link = False,
        link_postprocessor: ["cmd_args", None] = None) -> LinkedObject.type:
    cxx_toolchain_info = get_cxx_toolchain_info(ctx)
    linker_info = cxx_toolchain_info.linker_info

    should_generate_dwp = generate_dwp and dwp_available(ctx) and cxx_toolchain_info.split_dwarf_enabled
    if linker_info.supports_distributed_thinlto and enable_distributed_thinlto:
        if not linker_info.requires_objects:
            fail("Cannot use distributed thinlto if the cxx toolchain doesn't require_objects")
        return cxx_dist_link(
            ctx,
            links,
            output,
            linker_map,
            category_suffix,
            identifier,
            should_generate_dwp,
            executable_link,
        )
    if linker_map != None:
        links += [linker_map_args(ctx, linker_map.as_output())]
    (link_args, hidden, dwo_dir) = make_link_args(ctx, links, suffix = identifier, dwo_dir_name = output.short_path + ".dwo.d", is_shared = is_shared)

    external_debug_paths = []
    if dwo_dir != None:
        external_debug_paths.append(dwo_dir)

    shell_quoted_args = cmd_args(link_args, quote = "shell")
    argfile, macro_files = ctx.actions.write(
        output.short_path + ".linker.argsfile",
        shell_quoted_args,
        allow_args = True,
    )
    command = cxx_link_cmd(ctx)
    command.add("-o", output.as_output())
    command.add(cmd_args(argfile, format = "@{}"))
    command.hidden([hidden, macro_files])
    category = "cxx_link"
    if category_suffix != None:
        category += "_" + category_suffix

    # If the linked object files don't contain debug info, clang may not
    # generate a DWO directory, so make sure we at least `mkdir` and empty
    # one to make v2/RE happy.
    if dwo_dir != None:
        cmd = cmd_args(["/bin/sh", "-c"])
        cmd.add(cmd_args(dwo_dir.as_output(), format = 'mkdir -p {}; "$@"'))
        cmd.add('""').add(command)
        cmd.hidden(command)
        command = cmd
    ctx.actions.run(command, prefer_local = prefer_local, local_only = local_only, weight = link_weight, category = category, identifier = identifier)
    if strip:
        strip_args = strip_args_factory(ctx) if strip_args_factory else cmd_args()
        output = strip_shared_library(ctx, cxx_toolchain_info, output, strip_args)

    if link_postprocessor:
        output = postprocess(ctx, output, link_postprocessor)

    final_output = output if not (executable_link and cxx_use_bolt(ctx)) else bolt(ctx, output, identifier)
    dwp_artifact = None
    if should_generate_dwp:
        # TODO(T110378144): Once we track split dwarf from compiles, we should
        # just pass in `binary.external_debug_paths` here instead of all link
        # args.
        dwp_inputs = cmd_args()
        for link in links:
            dwp_inputs.add(unpack_link_args(link))
        dwp_inputs.add(external_debug_paths)

        dwp_artifact = dwp(
            ctx,
            final_output,
            identifier = identifier,
            category_suffix = category_suffix,
            # TODO(T110378142): Ideally, referenced objects are a list of
            # artifacts, but currently we don't track them properly.  So, we
            # just pass in the full link line and extract all inputs from that,
            # which is a bit of an overspecification.
            referenced_objects = [dwp_inputs],
            allow_huge_dwp = ctx.attrs.allow_huge_dwp if hasattr(ctx.attrs, "allow_huge_dwp") else False,
        )

    return LinkedObject(
        output = final_output,
        prebolt_output = output,
        dwp = dwp_artifact,
        external_debug_paths = external_debug_paths,
        linker_argsfile = argfile,
    )

def cxx_link_shared_library(
        ctx: "context",
        # The destination for the link output.
        output: "artifact",
        # Optional soname to link into shared library.
        name: [str.type, None] = None,
        links: [LinkArgs.type] = [],
        prefer_local: [bool.type, None] = None,
        local_only: [bool.type, None] = None,
        link_weight: int.type = 1,
        # A category suffix that will be added to the category of the link action that is generated.
        category_suffix: [str.type, None] = None,
        # An identifier that will uniquely name this link action in the context of a category. Useful for
        # differentiating multiple link actions in the same rule.
        identifier: [str.type, None] = None,
        # Overrides the default flags used to specify building shared libraries
        shared_library_flags: [SharedLibraryFlagOverrides.type, None] = None,
        strip: bool.type = False,
        strip_args_factory = None,
        link_postprocessor: ["cmd_args", None] = None) -> LinkedObject.type:
    """
    Link a shared library into the supplied output.
    """
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    linker_type = linker_info.type
    extra_args = []

    extra_args.extend(get_shared_library_flags(linker_type, shared_library_flags))  # e.g. "-shared"
    if name != None:
        extra_args.extend(get_shared_library_name_linker_flags(linker_type, name, shared_library_flags))

    return cxx_link(
        ctx,
        [LinkArgs(flags = extra_args)] + links,
        output,
        prefer_local = value_or(prefer_local, value_or(linker_info.link_libraries_locally, False)),
        local_only = value_or(local_only, False),
        link_weight = link_weight,
        category_suffix = category_suffix,
        identifier = identifier,
        is_shared = True,
        strip = strip,
        strip_args_factory = strip_args_factory,
        link_postprocessor = link_postprocessor,
    )

def cxx_link_into_shared_library(
        ctx: "context",
        name: str.type,
        links: [LinkArgs.type] = [],
        # Wether to embed the library name as the SONAME.
        soname: bool.type = True,
        prefer_local: [bool.type, None] = None,
        local_only: [bool.type, None] = None,
        link_weight: int.type = 1,
        # A category suffix that will be added to the category of the link action that is generated.
        category_suffix: [str.type, None] = None,
        # An identifier that will uniquely name this link action in the context of a category. Useful for
        # differentiating multiple link actions in the same rule.
        identifier: [str.type, None] = None,
        # Overrides the default flags used to specify building shared libraries
        shared_library_flags: [SharedLibraryFlagOverrides.type, None] = None,
        strip: bool.type = False,
        strip_args_factory = None,
        link_postprocessor: ["cmd_args", None] = None) -> LinkedObject.type:
    output = ctx.actions.declare_output(name)
    return cxx_link_shared_library(
        ctx,
        output,
        name = name if soname else None,
        links = links,
        prefer_local = prefer_local,
        local_only = local_only,
        link_weight = link_weight,
        category_suffix = category_suffix,
        identifier = identifier,
        shared_library_flags = shared_library_flags,
        strip = strip,
        strip_args_factory = strip_args_factory,
        link_postprocessor = link_postprocessor,
    )
