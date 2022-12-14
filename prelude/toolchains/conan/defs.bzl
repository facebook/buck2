# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Conan C/C++ Package Manager Toolchain.

Provides a toolchain and rules to use the [Conan package manager][conan] to
manage and install third-party C/C++ dependencies.
"""

# TODO[AH] Not sure if this self-reference within the prelude is acceptable.
#   If not, consider a custom rule implementation using the C++ API.
load("@prelude//:prelude.bzl", "native")

ConanInitInfo = provider(fields = ["user_home"])
ConanLockInfo = provider(fields = ["lockfile"])
ConanPackageInfo = provider(fields = ["reference", "cache_out"])
ConanToolchainInfo = provider(fields = ["conan"])

def conan_component(
        name: "string",
        defines: ["string"],
        cflags: ["string"],
        cppflags: ["string"],
        include_paths: ["string"],
        libs: ["string"],
        static_libs: {"string": ["string"]},
        shared_libs: {"string": ["string"]},
        system_libs: ["string"],
        deps: ["string"],
        package: "string"):

    extract_name = name + "_extract"
    extract_tpl = ":" + extract_name + "[{}]"
    extract_include_paths = [extract_tpl.format(p) for p in include_paths]
    extract_shared_libs = { n: extract_tpl.format(l) for n, l in shared_libs.items() }
    extract_static_libs = { n: extract_tpl.format(l) for n, l in static_libs.items() }

    # TODO[AH] Extract the required files and directories

    if len(libs) == 1:
        native.prebuilt_cxx_library(
            name = name,
            deps = deps,  # TODO[AH] Do we need exported_deps?
            exported_headers = extract_include_paths,  # TODO[AH] Should we use header_dirs? Should we list individual files here?
            exported_preprocessor_flags = ["-D" + d for d in defines],
            exported_lang_preprocessor_flags = {
                "c": cflags,
                "cxx": cppflags,
            },
            shared_lib = extract_shared_libs.get(libs[0]),
            static_lib = extract_static_libs.get(libs[0]),
            # TODO[AH] Can we set static_pic_lib, some libs seem to end on _pic?
            # TODO[AH] Do we need supports_merged_linking?
            # TODO[AH] Do we need supports_shared_library_interface?
        )
    else:
        fail("Implement prebuilt_cxx_library_group")
        #"contacts": attrs.list(attrs.string(), default = []),
        #"default_host_platform": attrs.option(attrs.configuration_label(), default = None),
        #"deps": attrs.list(attrs.dep(), default = []),
        #"exported_deps": attrs.list(attrs.dep(), default = []),
        #"exported_platform_deps": attrs.list(attrs.tuple(attrs.regex(), attrs.set(attrs.dep(), sorted = True)), default = []),
        #"exported_preprocessor_flags": attrs.list(attrs.string(), default = []),
        #"import_libs": attrs.dict(key = attrs.string(), value = attrs.source(), sorted = False, default = {}),
        #"include_dirs": attrs.list(attrs.source(), default = []),
        #"include_in_android_merge_map_output": attrs.bool(),
        #"labels": attrs.list(attrs.string(), default = []),
        #"licenses": attrs.list(attrs.source(), default = []),
        #"provided_shared_libs": attrs.dict(key = attrs.string(), value = attrs.source(), sorted = False, default = {}),
        #"shared_libs": attrs.dict(key = attrs.string(), value = attrs.source(), sorted = False, default = {}),
        #"shared_link": attrs.list(attrs.string(), default = []),
        #"static_libs": attrs.list(attrs.source(), default = []),
        #"static_link": attrs.list(attrs.string(), default = []),
        #"static_pic_libs": attrs.list(attrs.source(), default = []),
        #"static_pic_link": attrs.list(attrs.string(), default = []),
        #"supported_platforms_regex": attrs.option(attrs.regex(), default = None),
        #"within_view": attrs.option(attrs.list(attrs.string())),

def _conan_dep_impl(ctx: "context") -> ["provider"]:
    # TODO[AH] Exponse components as sub-targets.
    # TODO[AH] Exponse a top-level target that bundles and exports all components

    # cxx_merge_cpreprocessors(
    #     ctx,
    #     [generic_exported_pre, specific_exportd_pre],
    #     inherited_pp_infos,
    # )

    # create_merged_link_info(
    #     ctx,
    #     # Add link info for each link style,
    #     libraries,
    #     preferred_linkage = preferred_linkage,
    #     # Export link info from non-exported deps (when necessary).
    #     deps = [inherited_link],
    #     # Export link info from out (exported) deps.
    #     exported_deps = [inherited_exported_link],
    # )

    # merge_shared_libraries(
    #     ctx.actions,
    #     create_shared_libraries(ctx, solibs),
    #     filter(None, [x.get(SharedLibraryInfo) for x in exported_first_order_deps]),
    # )

    # If static-pic
    #
    # create_linkable_root(
    #     ctx,
    #     name = soname,
    #     link_infos = LinkInfos(default = LinkInfo(
    #         name = soname,
    #         pre_flags = cxx_attr_exported_linker_flags(ctx),
    #         linkables = [ArchiveLinkable(
    #             archive = Archive(
    #                 artifact = static_pic_lib or static_lib,
    #             ),
    #             linker_type = linker_type,
    #             link_whole = True,
    #         )],
    #         post_flags = cxx_attr_exported_post_linker_flags(ctx),
    #     )),
    #     deps = exported_first_order_deps,
    #     graph = deps_linkable_graph,
    #     create_shared_root = known_omnibus_root,
    # )

    # create_linkable_graph(
    #     ctx,
    #     node = create_linkable_graph_node(
    #         ctx,
    #         linkable_node = create_linkable_node(
    #             ctx = ctx,
    #             preferred_linkage = preferred_linkage,
    #             exported_deps = exported_first_order_deps,
    #             # If we don't have link input for this link style, we pass in `None` so
    #             # that omnibus knows to avoid it.
    #             link_infos = libraries,
    #             shared_libs = solibs,
    #         ),
    #         roots = roots,
    #         excluded = {ctx.label: None} if not value_or(ctx.attrs.supports_merged_linking, True) else {},
    #     ),
    #     children = [deps_linkable_graph],
    # )

    return [
        DefaultInfo(),
    ]

conan_dep = rule(
    impl = _conan_dep_impl,
    attrs = {
        "components": attrs.dict(key = attrs.string(), value = attrs.dep(providers = [])),
    },
)

def _conan_generate_impl(ctx: "context") -> ["provider"]:
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]
    conan_init = ctx.attrs._conan_init[ConanInitInfo]
    conan_generate = ctx.attrs._conan_generate[RunInfo]

    install_folder = ctx.actions.declare_output("install-folder")
    output_folder = ctx.actions.declare_output("output-folder")
    user_home = ctx.actions.declare_output("user-home")
    manifests = ctx.actions.declare_output("manifests")
    install_info = ctx.actions.declare_output("install-info.json")
    trace_log = ctx.actions.declare_output("trace.log")
    targets_out = ctx.actions.declare_output(ctx.label.name + ".bzl")

    cmd = cmd_args([conan_generate])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--conan-init", conan_init.user_home])
    cmd.add(["--buckler", ctx.attrs._buckler])
    cmd.add(["--install-folder", install_folder.as_output()])
    cmd.add(["--output-folder", output_folder.as_output()])
    cmd.add(["--user-home", user_home.as_output()])
    cmd.add(["--manifests", manifests.as_output()])
    cmd.add(["--install-info", install_info.as_output()])
    cmd.add(["--trace-file", trace_log.as_output()])
    cmd.add(["--conanfile", ctx.attrs.conanfile])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--targets-out", targets_out.as_output()])
    ctx.actions.run(cmd, category = "conan_build")

    return [
        # TODO[AH] ConanGenerateInfo with the generated targets file
        DefaultInfo(
            default_outputs = [targets_out],
            other_outputs = [
                install_folder,
                output_folder,
                user_home,
                manifests,
                install_info,
                trace_log,
            ],
        ),
    ]

conan_generate = rule(
    impl = _conan_generate_impl,
    attrs = {
        "conanfile": attrs.source(doc = "The conanfile defining the project dependencies."),
        "lockfile": attrs.source(doc = "The Conan lockfile pinning the package versions."),
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_init": attrs.dep(providers = [ConanInitInfo], default = "toolchains//:conan-init"),
        "_buckler": attrs.source(default = "prelude//toolchains/conan:buckler"),
        "_conan_generate": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_generate"),
    },
)

def _conan_init_impl(ctx: "context") -> ["provider"]:
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]
    conan_init = ctx.attrs._conan_init[RunInfo]

    user_home = ctx.actions.declare_output("user-home")
    trace_log = ctx.actions.declare_output("trace.log")

    cmd = cmd_args([conan_init])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--user-home", user_home.as_output()])
    cmd.add(["--trace-file", trace_log.as_output()])
    ctx.actions.run(cmd, category = "conan_init")

    return [
        ConanInitInfo(
            user_home = user_home,
        ),
        DefaultInfo(default_outputs = [
            user_home,
            trace_log,
        ]),
    ]

conan_init = rule(
    impl = _conan_init_impl,
    attrs = {
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_init": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_init"),
    },
)

def _conan_lock_impl(ctx: "context") -> ["provider"]:
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]
    conan_init = ctx.attrs._conan_init[ConanInitInfo]
    conan_lock = ctx.attrs._conan_lock[RunInfo]

    lockfile_out = ctx.actions.declare_output("conan.lock")
    user_home = ctx.actions.declare_output("user-home")
    trace_log = ctx.actions.declare_output("trace.log")

    cmd = cmd_args([conan_lock])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--conan-init", conan_init.user_home])
    cmd.add(["--user-home", user_home.as_output()])
    cmd.add(["--trace-file", trace_log.as_output()])
    cmd.add(["--conanfile", ctx.attrs.conanfile])
    cmd.add(["--lockfile-out", lockfile_out.as_output()])
    if ctx.attrs.lockfile:
        cmd.add(["--lockfile", ctx.attrs.lockfile])
    ctx.actions.run(cmd, category = "conan_lock")

    return [
        ConanLockInfo(
            lockfile = lockfile_out,
        ),
        DefaultInfo(
            default_outputs = [lockfile_out],
            other_outputs = [user_home, trace_log],
        ),
    ]

conan_lock = rule(
    impl = _conan_lock_impl,
    attrs = {
        "conanfile": attrs.source(doc = "The conanfile defining the project dependencies."),
        "lockfile": attrs.option(attrs.source(doc = "A pre-existing lockfile to base the dependency resolution on."), default = None),
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_init": attrs.dep(providers = [ConanInitInfo], default = "toolchains//:conan-init"),
        "_conan_lock": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_lock"),
    },
)

def _conan_package_impl(ctx: "context") -> ["provider"]:
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]
    conan_init = ctx.attrs._conan_init[ConanInitInfo]
    conan_package = ctx.attrs._conan_package[RunInfo]

    install_folder = ctx.actions.declare_output("install-folder")
    output_folder = ctx.actions.declare_output("output-folder")
    user_home = ctx.actions.declare_output("user-home")
    manifests = ctx.actions.declare_output("manifests")
    install_info = ctx.actions.declare_output("install-info.json")
    trace_log = ctx.actions.declare_output("trace.log")
    cache_out = ctx.actions.declare_output("cache-out")

    cmd = cmd_args([conan_package])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--conan-init", conan_init.user_home])
    cmd.add(["--buckler", ctx.attrs._buckler])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--reference", ctx.attrs.reference])
    cmd.add(["--install-folder", install_folder.as_output()])
    cmd.add(["--output-folder", output_folder.as_output()])
    cmd.add(["--user-home", user_home.as_output()])
    cmd.add(["--manifests", manifests.as_output()])
    cmd.add(["--install-info", install_info.as_output()])
    cmd.add(["--trace-file", trace_log.as_output()])
    cmd.add(["--cache-out", cache_out.as_output()])
    # TODO[AH] Track transitive dependencies.
    for dep in ctx.attrs.deps:
        info = dep[ConanPackageInfo]
        cmd.add(["--dep-reference", info.reference, "--dep-cache-out", info.cache_out])
    ctx.actions.run(cmd, category = "conan_build")

    return [
        ConanPackageInfo(
            reference = ctx.attrs.reference,
            cache_out = cache_out,
        ),
        DefaultInfo(default_outputs = [
            install_folder,
            output_folder,
            user_home,
            manifests,
            install_info,
            trace_log,
            cache_out,
        ]),
    ]

conan_package = rule(
    impl = _conan_package_impl,
    attrs = {
        "lockfile": attrs.source(doc = "The Conan lockfile defining the package and its dependencies."),
        "reference": attrs.string(doc = "The Conan package reference <name>/<version>#<revision>."),
        "deps": attrs.list(attrs.dep(providers = [ConanPackageInfo], doc = "Conan Package dependencies.")),
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_init": attrs.dep(providers = [ConanInitInfo], default = "toolchains//:conan-init"),
        "_conan_package": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_package"),
        "_buckler": attrs.source(default = "prelude//toolchains/conan:buckler"),
    },
)

def _conan_update_impl(ctx: "context") -> ["provider"]:
    conan_update = ctx.attrs._conan_update[RunInfo]

    cmd = cmd_args([conan_update])
    cmd.add(["--update-label", str(ctx.label.raw_target())])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--lock-targets", ctx.attrs.lock_generate])
    cmd.add(["--conan-targets", ctx.attrs.conan_generate])
    cmd.add(["--conanfile", ctx.attrs.conanfile])
    cmd.add(["--lockfile-out", ctx.attrs.lockfile_name])
    cmd.add(["--targets-out", ctx.attrs.targets_name])

    return [
        DefaultInfo(default_outputs = []),
        RunInfo(args = [cmd]),
    ]

conan_update = rule(
    impl = _conan_update_impl,
    attrs = {
        "lockfile": attrs.source(doc = "The generated Conan lockfile."),
        "lock_generate": attrs.source(doc = "The targets generated from the Conan lockfile."),
        "conan_generate": attrs.source(doc = "The targets generated by Buckler."),
        "conanfile": attrs.source(doc = "The Conanfile."),
        "lockfile_name": attrs.string(doc = "Generate a lockfile with this name next to the Conanfile."),
        "targets_name": attrs.string(doc = "Generate a TARGETS file with this name next to the Conanfile."),
        "_conan_update": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_update"),
    },
    doc = "Defines a runnable target that will update the Conan lockfile and import targets.",
)

def _relative_label(lbl: "label", relto: "label") -> "string":
    """Return a string representation of `lbl` relative to `relto`.

    E.g. `root//some/package:here` is `:here` relative to `root//some/package:there`.
    """
    if lbl.cell != relto.cell:
        tpl = "{cell}//{package}:{name}"
    elif lbl.package != relto.package:
        tpl = "//{package}:{name}"
    else:
        tpl = ":{name}"
    if lbl.sub_target:
        tpl += "[{sub_target}]"
    return tpl.format(
        cell = lbl.cell,
        package = lbl.package,
        name = lbl.name,
        sub_target = lbl.sub_target,
    )

def _lock_generate_impl(ctx: "context") -> ["provider"]:
    lock_generate = ctx.attrs._lock_generate[RunInfo]

    targets_out = ctx.actions.declare_output(ctx.label.name + ".bzl")

    cmd = cmd_args([lock_generate])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--lockfile-label", _relative_label(ctx.attrs.lockfile.owner, ctx.label)])
    cmd.add(["--targets-out", targets_out.as_output()])
    ctx.actions.run(cmd, category = "conan_generate")

    return [
        DefaultInfo(
            default_outputs = [targets_out],
        ),
    ]

lock_generate = rule(
    impl = _lock_generate_impl,
    attrs = {
        "lockfile": attrs.source(doc = "The Conan lockfile defining the package and its dependencies."),
        "_lock_generate": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:lock_generate"),
    },
)

def _system_conan_toolchain_impl(ctx: "context") -> ["provider"]:
    return [
        DefaultInfo(),
        ConanToolchainInfo(
            conan = RunInfo(args = [ctx.attrs.conan_path]),
        ),
    ]

system_conan_toolchain = rule(
    impl = _system_conan_toolchain_impl,
    attrs = {
        "conan_path": attrs.string(doc = "Path to the Conan executable."),
    },
    is_toolchain_rule = True,
    doc = "Uses a globally installed Conan executable.",
)
