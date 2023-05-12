# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:cxx_bolt.bzl",
    "bolt",
    "cxx_use_bolt",
)
load("@prelude//cxx:debug.bzl", "SplitDebugMode", "maybe_external_debug_info")
load(
    "@prelude//cxx/dist_lto:dist_lto.bzl",
    "cxx_dist_link",
)
load("@prelude//linking:execution_preference.bzl", "LinkExecutionPreference", "LinkExecutionPreferenceInfo", "get_action_execution_attributes")
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkOrdering",
    "LinkedObject",
    "unpack_external_debug_info",
    "unpack_link_args",
)
load("@prelude//linking:lto.bzl", "darwin_lto_linker_flags")
load("@prelude//linking:strip.bzl", "strip_shared_library")
load("@prelude//utils:utils.bzl", "map_val", "value_or")
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
    "get_import_library",
    "get_output_flags",
    "get_shared_library_flags",
    "get_shared_library_name_linker_flags",
)

CxxLinkResultType = enum(
    "executable",
    "shared_library",
)

CxxLinkerMapData = record(
    map = field("artifact"),
    binary = field("artifact"),
)

# Actually perform a link into the supplied output.
def cxx_link(
        ctx: "context",
        links: [LinkArgs.type],
        # The destination for the link output.
        output: "artifact",
        result_type: CxxLinkResultType.type,
        link_execution_preference: LinkExecutionPreference.type,
        link_weight: int.type = 1,
        link_ordering: [LinkOrdering.type, None] = None,
        enable_distributed_thinlto: bool.type = False,
        # A category suffix that will be added to the category of the link action that is generated.
        category_suffix: [str.type, None] = None,
        # An identifier that will uniquely name this link action in the context of a category. Useful for
        # differentiating multiple link actions in the same rule.
        identifier: [str.type, None] = None,
        strip: bool.type = False,
        # A function/lambda which will generate the strip args using the ctx.
        strip_args_factory = None,
        allow_bolt_optimization_and_dwp_generation: bool.type = True,
        import_library: ["artifact", None] = None) -> (LinkedObject.type, [CxxLinkerMapData.type, None], LinkExecutionPreferenceInfo.type):
    cxx_toolchain_info = get_cxx_toolchain_info(ctx)
    linker_info = cxx_toolchain_info.linker_info

    should_generate_dwp = allow_bolt_optimization_and_dwp_generation and dwp_available(ctx) and cxx_toolchain_info.split_debug_mode != SplitDebugMode("none")
    is_result_executable = result_type.value == "executable"

    if linker_info.generate_linker_maps:
        linker_map = ctx.actions.declare_output(output.short_path + "-LinkMap.txt")
        linker_map_data = CxxLinkerMapData(
            map = linker_map,
            binary = output,
        )
    else:
        linker_map = None
        linker_map_data = None

    if linker_info.supports_distributed_thinlto and enable_distributed_thinlto:
        if not linker_info.requires_objects:
            fail("Cannot use distributed thinlto if the cxx toolchain doesn't require_objects")
        exe = cxx_dist_link(
            ctx,
            links,
            output,
            linker_map,
            category_suffix,
            identifier,
            should_generate_dwp,
            is_result_executable,
        )
        return (exe, linker_map_data, LinkExecutionPreferenceInfo(preference = link_execution_preference))

    if linker_info.generate_linker_maps:
        links += [linker_map_args(ctx, linker_map.as_output())]

    (link_args, hidden, dwo_dir, pdb_artifact) = make_link_args(
        ctx,
        links,
        suffix = identifier,
        output_short_path = output.short_path,
        is_shared = result_type.value == "shared_library",
        link_ordering = value_or(
            link_ordering,
            # Fallback to toolchain default.
            map_val(LinkOrdering, linker_info.link_ordering),
        ),
    )

    # Darwin LTO requires extra link outputs to preserve debug info
    darwin_lto_flags, darwin_lto_artifacts = darwin_lto_linker_flags(ctx)
    link_args.add(darwin_lto_flags)

    external_debug_artifacts = darwin_lto_artifacts
    external_debug_infos = []

    # When using LTO+split-dwarf, the link step will generate externally
    # referenced debug info.
    if dwo_dir != None:
        external_debug_artifacts.append(dwo_dir)

    if pdb_artifact != None:
        external_debug_artifacts.append(pdb_artifact)

    # If we're not stripping the output linked object, than add-in an externally
    # referenced debug info that the linked object may reference (and which may
    # need to be available for debugging).
    if not (strip or getattr(ctx.attrs, "prefer_stripped_objects", False)):
        for link in links:
            external_debug_infos.append(unpack_external_debug_info(ctx.actions, link))

    external_debug_info = maybe_external_debug_info(
        actions = ctx.actions,
        label = ctx.label,
        artifacts = external_debug_artifacts,
        children = external_debug_infos,
    )

    if linker_info.type == "windows":
        shell_quoted_args = cmd_args(link_args)
    else:
        shell_quoted_args = cmd_args(link_args, quote = "shell")
    argfile, _ = ctx.actions.write(
        output.short_path + ".linker.argsfile",
        shell_quoted_args,
        allow_args = True,
    )
    command = cxx_link_cmd(ctx)
    command.add(get_output_flags(linker_info.type, output))
    command.add(cmd_args(argfile, format = "@{}"))
    command.hidden(hidden)
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

    link_execution_preference_info = LinkExecutionPreferenceInfo(preference = link_execution_preference)
    action_execution_properties = get_action_execution_attributes(link_execution_preference)

    ctx.actions.run(
        command,
        prefer_local = action_execution_properties.prefer_local,
        prefer_remote = action_execution_properties.prefer_remote,
        local_only = action_execution_properties.local_only,
        weight = link_weight,
        category = category,
        identifier = identifier,
        force_full_hybrid_if_capable = action_execution_properties.full_hybrid,
    )
    if strip:
        strip_args = strip_args_factory(ctx) if strip_args_factory else cmd_args()
        output = strip_shared_library(ctx, cxx_toolchain_info, output, strip_args)

    final_output = output if not (allow_bolt_optimization_and_dwp_generation and is_result_executable and cxx_use_bolt(ctx)) else bolt(ctx, output, identifier)
    dwp_artifact = None
    if should_generate_dwp:
        # TODO(T110378144): Once we track split dwarf from compiles, we should
        # just pass in `binary.external_debug_info` here instead of all link
        # args.
        dwp_inputs = cmd_args()
        for link in links:
            dwp_inputs.add(unpack_link_args(link))
        if external_debug_info != None:
            dwp_inputs.add(external_debug_info.project_as_args("external_debug_info"))

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
        )

    linked_object = LinkedObject(
        output = final_output,
        prebolt_output = output,
        dwp = dwp_artifact,
        external_debug_info = external_debug_info,
        linker_argsfile = argfile,
        import_library = import_library,
        pdb = pdb_artifact,
    )
    return (linked_object, linker_map_data, link_execution_preference_info)

def cxx_link_shared_library(
        ctx: "context",
        # The destination for the link output.
        output: "artifact",
        # Optional soname to link into shared library.
        name: [str.type, None] = None,
        links: [LinkArgs.type] = [],
        link_execution_preference: LinkExecutionPreference.type = LinkExecutionPreference("any"),
        link_weight: int.type = 1,
        link_ordering: [LinkOrdering.type, None] = None,
        enable_distributed_thinlto: bool.type = False,
        # A category suffix that will be added to the category of the link action that is generated.
        category_suffix: [str.type, None] = None,
        # An identifier that will uniquely name this link action in the context of a category. Useful for
        # differentiating multiple link actions in the same rule.
        identifier: [str.type, None] = None,
        # Overrides the default flags used to specify building shared libraries
        shared_library_flags: [SharedLibraryFlagOverrides.type, None] = None,
        strip: bool.type = False,
        strip_args_factory = None) -> (LinkedObject.type, [CxxLinkerMapData.type, None], LinkExecutionPreferenceInfo.type):
    """
    Link a shared library into the supplied output.
    """
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    linker_type = linker_info.type
    extra_args = []

    extra_args.extend(get_shared_library_flags(linker_type, shared_library_flags))  # e.g. "-shared"
    if name != None:
        extra_args.extend(get_shared_library_name_linker_flags(linker_type, name, shared_library_flags))

    if linker_info.link_libraries_locally:
        link_execution_preference = LinkExecutionPreference("local")

    (import_library, import_library_args) = get_import_library(
        ctx,
        linker_type,
        output.short_path,
    )

    links_with_extra_args = [LinkArgs(flags = extra_args)] + links + [LinkArgs(flags = import_library_args)]

    exe, linker_map_data, preference_info = cxx_link(
        ctx,
        links_with_extra_args,
        output,
        CxxLinkResultType("shared_library"),
        enable_distributed_thinlto = enable_distributed_thinlto,
        category_suffix = category_suffix,
        identifier = identifier,
        strip = strip,
        strip_args_factory = strip_args_factory,
        import_library = import_library,
        link_ordering = link_ordering,
        link_weight = link_weight,
        link_execution_preference = link_execution_preference,
    )

    return (exe, linker_map_data, preference_info)

def cxx_link_into_shared_library(
        ctx: "context",
        name: str.type,
        links: [LinkArgs.type] = [],
        # Whether to embed the library name as the SONAME.
        soname: bool.type = True,
        link_execution_preference: LinkExecutionPreference.type = LinkExecutionPreference("any"),
        link_ordering: [LinkOrdering.type, None] = None,
        link_weight: int.type = 1,
        enable_distributed_thinlto: bool.type = False,
        # A category suffix that will be added to the category of the link action that is generated.
        category_suffix: [str.type, None] = None,
        # An identifier that will uniquely name this link action in the context of a category. Useful for
        # differentiating multiple link actions in the same rule.
        identifier: [str.type, None] = None,
        # Overrides the default flags used to specify building shared libraries
        shared_library_flags: [SharedLibraryFlagOverrides.type, None] = None,
        strip: bool.type = False,
        strip_args_factory = None) -> (LinkedObject.type, [CxxLinkerMapData.type, None], LinkExecutionPreferenceInfo.type):
    output = ctx.actions.declare_output(name)
    return cxx_link_shared_library(
        ctx,
        output,
        name = name if soname else None,
        links = links,
        link_execution_preference = link_execution_preference,
        link_weight = link_weight,
        enable_distributed_thinlto = enable_distributed_thinlto,
        category_suffix = category_suffix,
        identifier = identifier,
        link_ordering = link_ordering,
        shared_library_flags = shared_library_flags,
        strip = strip,
        strip_args_factory = strip_args_factory,
    )
