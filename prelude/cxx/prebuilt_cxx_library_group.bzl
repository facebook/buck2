# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "LinkerType",
    "PicBehavior",
)
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(
    "@prelude//linking:link_groups.bzl",
    "merge_link_group_lib_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "Archive",
    "ArchiveLinkable",
    "LibOutputStyle",
    "LinkInfo",
    "LinkInfos",
    "LinkStrategy",
    "LinkedObject",
    "SharedLibLinkable",
    "create_merged_link_info",
    "get_lib_output_style",
    "get_output_styles_for_linkage",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
    "create_linkable_graph_node",
    "create_linkable_node",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "create_shared_libraries",
    "merge_shared_libraries",
)
load("@prelude//linking:strip.bzl", "strip_debug_info")
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//unix:providers.bzl", "UnixEnv", "create_unix_env_info")
load("@prelude//utils:expect.bzl", "expect")
load("@prelude//utils:utils.bzl", "flatten_dict")
load(":cxx_context.bzl", "get_cxx_toolchain_info")
load(
    ":cxx_library_utility.bzl",
    "cxx_attr_dep_metadata",
    "cxx_inherited_link_info",
    "cxx_use_shlib_intfs",
)
load(
    ":shared_library_interface.bzl",
    "shared_library_interface",
)

def _linkage(ctx: AnalysisContext) -> Linkage:
    """
    Construct the preferred linkage to use for the given prebuilt library.
    """

    # If we have both shared and static libs, we support any linkage.
    if (ctx.attrs.shared_link and
        (ctx.attrs.static_link or ctx.attrs.static_pic_link)):
        return Linkage("any")

    # Otherwise, if we have a shared library, we only support shared linkage.
    if ctx.attrs.shared_link:
        return Linkage("shared")

    # Otherwise, if we have a static library, we only support static linkage.
    if ctx.attrs.static_link or ctx.attrs.static_pic_link:
        return Linkage("static")

    # Otherwise, header only libs use any linkage.
    return Linkage("any")

def _parse_macro(arg: str) -> [(str, str), None]:
    """
    Parse a lib reference macro (e.g. `$(lib 0)`, `$(rel-lib libfoo.so)`) into
    the format string used to format the arg, the name of the macro parsed, and
    the argument passed to the macro.
    """

    # TODO(T110378124): This is obviously not ideal and longer-term we should
    # probably come up with a better UI for this rule or properly support these
    # macros.

    # If there's not macro, then there's nothing to do.
    if "$(" not in arg:
        return None

    # Extract the macro name and it's arg out of the string.  Also, create a
    # format string with the remaining parts which can be used to format to an
    # actual arg.  This is pretty ugly, but we don't have too complex a case to
    # support (e.g. a single macro with a single arg).
    start, rest = arg.split("$(")
    expect(start == "")
    pos = rest.find(" ")
    macro = rest[:pos]
    rest = rest[pos + 1:]
    pos = rest.find(")")
    param = rest[:pos]
    end = rest[pos + 1:]
    expect(end == "")

    return macro, param

def _get_static_link_infos(
        ctx: AnalysisContext,
        linker_type: LinkerType,
        libs: list[Artifact],
        args: list[str]) -> LinkInfos:
    """
    Format a pair of static link string args and static libs into args to be
    passed to the link, by resolving macro references to libraries.
    """

    def archive_linkable(artifact):
        return ArchiveLinkable(
            archive = Archive(artifact = artifact),
            linker_type = linker_type,
            # We assume prebuilt C/C++ libs don't contain LTO code and
            # avoid potentially expensive processing of the to support
            # dist LTO.  In additional, some prebuilt library groups
            # use `--start-group`/`--end-group` which breaks with our
            # dist LTO impl wrapping w/ `--start-lib`.
            supports_lto = False,
        )

    pre_flags = []
    post_flags = []
    linkables = []
    linkables_stripped = []

    for arg in args:
        res = _parse_macro(arg)
        if res != None:
            # We require that link lines are written such that link flags wrap
            # linkables.  So verify that we haven't already seen post linker
            # flags.
            expect(not post_flags)

            # Macros in the static link line are indexes to the list of static
            # archives.
            macro, param = res
            expect(macro == "lib")
            lib = libs[int(param)]
            linkables.append(archive_linkable(lib))
            linkables_stripped.append(archive_linkable(strip_debug_info(ctx, lib.short_path, lib, anonymous = True)))
        elif linkables:
            # If we've already seen linkables, put remaining flags/args into
            # post-linker flags.
            post_flags.append(arg)
        else:
            pre_flags.append(arg)

    return LinkInfos(
        default = LinkInfo(
            pre_flags = pre_flags,
            post_flags = post_flags,
            linkables = linkables,
            metadata = cxx_attr_dep_metadata(ctx),
        ),
        stripped = LinkInfo(
            pre_flags = pre_flags,
            post_flags = post_flags,
            linkables = linkables_stripped,
            metadata = cxx_attr_dep_metadata(ctx),
        ),
    )

def _get_shared_link_infos(
        ctx: AnalysisContext,
        shared_libs: dict[str, Artifact],
        args: list[str],
        shlib_intfs: bool = True) -> LinkInfos:
    """
    Format a pair of shared link string args and shared libs into args to be
    passed to the link, by resolving macro references to libraries.
    """

    pre_flags = []
    post_flags = []
    linkables = []

    for arg in args:
        res = _parse_macro(arg)
        if res != None:
            # We require that link lines are written such that link flags wrap
            # linkables.  So verify that we haven't already seen post linker
            # flags.
            expect(not post_flags)

            # Macros in the shared link line are named references to the map
            # of all shared libs.
            macro, lib_name = res
            expect(macro in ("lib", "rel-lib"))
            shared_lib = shared_libs[lib_name]
            if shlib_intfs:
                shared_lib = shared_library_interface(
                    ctx = ctx,
                    shared_lib = shared_lib,
                    anonymous = True,
                )
            if macro == "lib":
                linkables.append(SharedLibLinkable(lib = shared_lib))
            elif macro == "rel-lib":
                # rel-lib means link-without-soname.
                linkables.append(SharedLibLinkable(lib = shared_lib, link_without_soname = True))
        elif linkables:
            # If we've already seen linkables, put remaining flags/args into
            # post-linker flags.
            post_flags.append(arg)
        else:
            pre_flags.append(arg)

    return LinkInfos(
        default = LinkInfo(
            pre_flags = pre_flags,
            post_flags = post_flags,
            linkables = linkables,
            metadata = cxx_attr_dep_metadata(ctx),
        ),
    )

# The `prebuilt_cxx_library_group` rule is meant to provide fine user control for
# how a group libraries of libraries are added to the link line and was added for
# `fbcode//third-party-buck/platform009/build/IntelComposerXE:mkl_lp64_iomp`, which
# includes libraries with dep cycles, and so must be linked together with flags
# like `--start-group`/`--end-group`.
#
# The link arguments for the various link styles are specified by pair of string
# arguments with macros referencing a collection of libraries:
#
# - For static link styles, the string link args (e.g. specific in `static_link`)
#   contain macros of the form `$(lib <number>)`, where the number is an index
#   into the corresponding list of static libraries artifacts (e.g. specified in
#   `static_libs`).  For example:
#
#     static_link = ["-Wl,--start-group", "$(lib 0)", "$(lib 1)", "-Wl,--end-group"],
#     static_libs = ["libfoo1.a", "libfoo2.a"],
#
# - For shared linking, the string link args contain macros of the form
#   `$(lib <name>)` or `$(rel-lib <name>)`, where the name is key for shared
#   libraries specified in `shared_libs` or `provided_shared_libs`.  The
#   `lib` macro examples to the full path of the shared library, whereas the
#   `rel-lib` macro expands to `-L<dirname> -l<name>` of the library and is
#   meant to be used in situations where shared library does not contain an
#   embedded soname.  For example:
#
#     shared_link = ["$(lib libfoo1.so)", "$(rel-lib libfoo2.so)"],
#     shared_libs = {
#         "libfoo1.so": "lib/libfoo1.so",
#         "libfoo2.so": "lib/libfoo2.so",
#     },
#
def prebuilt_cxx_library_group_impl(ctx: AnalysisContext) -> list[Provider]:
    providers = []

    deps = ctx.attrs.deps
    exported_deps = ctx.attrs.exported_deps

    # Figure out preprocessor stuff
    args = []
    args.extend(ctx.attrs.exported_preprocessor_flags)
    for inc_dir in ctx.attrs.include_dirs:
        args += ["-isystem", inc_dir]
    preprocessor = CPreprocessor(args = CPreprocessorArgs(args = args))
    inherited_pp_info = cxx_inherited_preprocessor_infos(exported_deps)
    providers.append(cxx_merge_cpreprocessors(ctx, [preprocessor], inherited_pp_info))

    # Figure out all the link styles we'll be building archives/shlibs for.
    preferred_linkage = _linkage(ctx)

    inherited_non_exported_link = cxx_inherited_link_info(deps)
    inherited_exported_link = cxx_inherited_link_info(exported_deps)

    linker_type = get_cxx_toolchain_info(ctx).linker_info.type

    # Gather link infos, outputs, and shared libs for effective link style.
    outputs = {}
    libraries = {}
    solibs = {}
    for output_style in get_output_styles_for_linkage(preferred_linkage):
        outs = []
        if output_style == LibOutputStyle("archive"):
            outs.extend(ctx.attrs.static_libs)
            infos = _get_static_link_infos(
                ctx,
                linker_type,
                ctx.attrs.static_libs,
                ctx.attrs.static_link,
            )
        elif output_style == LibOutputStyle("pic_archive"):
            outs.extend(ctx.attrs.static_pic_libs)
            infos = _get_static_link_infos(
                ctx,
                linker_type,
                ctx.attrs.static_pic_libs,
                ctx.attrs.static_pic_link,
            )
        else:  # shared
            outs.extend(ctx.attrs.shared_libs.values())
            infos = _get_shared_link_infos(
                ctx = ctx,
                shared_libs = flatten_dict([ctx.attrs.shared_libs, ctx.attrs.provided_shared_libs]),
                args = ctx.attrs.shared_link,
                shlib_intfs = ctx.attrs.supports_shared_library_interface and cxx_use_shlib_intfs(ctx),
            )
            solibs.update({n: LinkedObject(output = lib, unstripped_output = lib) for n, lib in ctx.attrs.shared_libs.items()})
        outputs[output_style] = outs

        libraries[output_style] = infos

    # This code is already compiled, so, the argument (probably) has little/no value.
    pic_behavior = PicBehavior("supported")

    # prebuilt_cxx_library_group default output is always the output used for the "static" link strategy.
    static_output_style = get_lib_output_style(LinkStrategy("static"), preferred_linkage, pic_behavior)
    providers.append(DefaultInfo(default_outputs = outputs[static_output_style]))

    # Provider for native link.
    providers.append(create_merged_link_info(
        ctx,
        pic_behavior,
        libraries,
        preferred_linkage = preferred_linkage,
        # Export link info from our (non-exported) deps (e.g. when we're linking
        # statically).
        deps = inherited_non_exported_link,
        # Export link info from our (exported) deps.
        exported_deps = inherited_exported_link,
    ))

    # Propagate shared libraries up the tree.
    shared_libs = create_shared_libraries(ctx, solibs)
    providers.append(merge_shared_libraries(
        ctx.actions,
        shared_libs,
        filter(None, [x.get(SharedLibraryInfo) for x in deps + exported_deps]),
    ))

    # Create, augment and provide the linkable graph.
    linkable_graph = create_linkable_graph(
        ctx,
        node = create_linkable_graph_node(
            ctx,
            linkable_node = create_linkable_node(
                ctx = ctx,
                deps = deps,
                exported_deps = exported_deps,
                preferred_linkage = preferred_linkage,
                link_infos = libraries,
                shared_libs = shared_libs,
                can_be_asset = getattr(ctx.attrs, "can_be_asset", False) or False,
                # TODO(cjhopman): this should be set to non-None
                default_soname = None,
            ),
        ),
        deps = deps + exported_deps,
    )
    providers.append(linkable_graph)

    providers.append(merge_link_group_lib_info(deps = deps + exported_deps))

    providers.append(
        create_unix_env_info(
            actions = ctx.actions,
            env = UnixEnv(
                label = ctx.label,
                native_libs = [shared_libs],
            ),
            deps = deps + exported_deps,
        ),
    )

    return providers
