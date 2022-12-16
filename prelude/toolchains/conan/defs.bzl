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
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//utils:utils.bzl", "flatten")

ConanInitInfo = provider(fields = ["profile", "user_home"])
ConanLockInfo = provider(fields = ["lockfile"])
ConanPackageInfo = provider(fields = ["reference", "package_id", "cache_out", "package_out"])
ConanToolchainInfo = provider(fields = ["conan"])

def _conan_package_extract_impl(ctx: "context") -> ["provider"]:
    conan_package_extract = ctx.attrs._conan_package_extract[RunInfo]

    cmd = cmd_args([conan_package_extract])
    sub_targets = {}

    for filename in ctx.attrs.files:
        output = ctx.actions.declare_output(filename)
        cmd.add(["--file-from", filename, "--file-to", output.as_output()])
        if filename in sub_targets:
            fail("File-name collision: " + filename)
        sub_targets[filename] = [DefaultInfo(default_outputs = [output])]

    i = 0
    for dirname in ctx.attrs.directories:
        # Some packages provide overlapping include directories, e.g.
        # `include`, and `include/jemalloc`. Such overlapping directories
        # cannot both be passed to `prebuilt_cxx_library`'s `include_dirs`.
        # This adds a counter prefix to avoid the overlap.
        prefix = str(i) + "/"
        i += 1
        output = ctx.actions.declare_output(prefix + dirname)
        cmd.add(["--directory-from", dirname, "--directory-to", output.as_output()])
        if dirname in sub_targets:
            fail("Directory-name collision: " + dirname)
        sub_targets[dirname] = [DefaultInfo(default_outputs = [output])]

    cmd.add(["--package", ctx.attrs.package[ConanPackageInfo].package_out])
    ctx.actions.run(cmd, category = "conan_extract")

    return [DefaultInfo(default_outputs = [], sub_targets = sub_targets)]

_conan_package_extract = rule(
    impl = _conan_package_extract_impl,
    attrs = {
        "package": attrs.dep(providers = [ConanPackageInfo]),
        "files": attrs.list(attrs.string()),
        "directories": attrs.list(attrs.string()),
        "_conan_package_extract": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_package_extract"),
    },
)

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
    extract_shared_libs = { name: [extract_tpl.format(lib) for lib in libs] for name, libs in shared_libs.items() }
    extract_static_libs = { name: [extract_tpl.format(lib) for lib in libs] for name, libs in static_libs.items() }

    _conan_package_extract(
        name = extract_name,
        package = package,
        files = flatten(static_libs.values() + shared_libs.values()),
        directories = include_paths,
    )

    if len(libs) == 1:
        lib = libs[0]
        if lib in shared_libs:
            shared_lib = extract_shared_libs[lib][0]
        else:
            shared_lib = None
        if lib in static_libs:
            static_lib = extract_static_libs[lib][0]
        else:
            static_lib = None
        native.prebuilt_cxx_library(
            name = name,
            deps = deps,  # TODO[AH] Do we need exported_deps?
            header_dirs = extract_include_paths,
            exported_preprocessor_flags = ["-D" + d for d in defines],
            exported_lang_preprocessor_flags = {
                "c": cflags,
                "cxx": cppflags,
            },
            exported_post_linker_flags = ["-l" + lib for lib in system_libs],
            shared_lib = shared_lib,
            static_lib = static_lib,
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

def _conan_cxx_libraries_impl(ctx: "context") -> list.type:
    default_info = DefaultInfo(
        default_outputs = ctx.attrs.main[DefaultInfo].default_outputs + flatten([c[DefaultInfo].default_outputs for c in ctx.attrs.components.values()]),
        sub_targets = { n: c.providers for n, c in ctx.attrs.components.items() },
    )
    providers = [p for p in ctx.attrs.main.providers if type(p) != "DefaultInfo"]
    providers.append(default_info)
    # TODO[AH] This doesn't satisfy the return type annotation ["provider"], why?
    return providers

_conan_cxx_libraries = rule(
    impl = _conan_cxx_libraries_impl,
    attrs = {
        "main": attrs.dep(),
        "components": attrs.dict(key = attrs.string(), value = attrs.dep()),
    },
)

def conan_dep(name: "string", components: {"string": "string"}, **kwargs):
    native.cxx_library(
        name = "_bundle_" + name,
        exported_deps = components.values(),
    )
    _conan_cxx_libraries(
        name = name,
        main = ":_bundle_" + name,
        components = components,
        **kwargs
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
    cmd.add(["--profile", conan_init.profile])
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
            profile = ctx.attrs.profile,
        ),
        DefaultInfo(default_outputs = [
            user_home,
            trace_log,
        ]),
    ]

conan_init = rule(
    impl = _conan_init_impl,
    attrs = {
        "profile": attrs.source(),
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
    cmd.add(["--profile", conan_init.profile])
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
    package_out = ctx.actions.declare_output("package")

    cmd = cmd_args([conan_package])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--conan-init", conan_init.user_home])
    cmd.add(["--profile", conan_init.profile])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--reference", ctx.attrs.reference])
    cmd.add(["--package-id", ctx.attrs.package_id])
    cmd.add(["--install-folder", install_folder.as_output()])
    cmd.add(["--output-folder", output_folder.as_output()])
    cmd.add(["--user-home", user_home.as_output()])
    cmd.add(["--manifests", manifests.as_output()])
    cmd.add(["--install-info", install_info.as_output()])
    cmd.add(["--trace-file", trace_log.as_output()])
    cmd.add(["--cache-out", cache_out.as_output()])
    cmd.add(["--package-out", package_out.as_output()])
    # TODO[AH] Track transitive dependencies.
    for dep in ctx.attrs.deps:
        info = dep[ConanPackageInfo]
        cmd.add(["--dep-reference", info.reference, "--dep-cache-out", info.cache_out])
    ctx.actions.run(cmd, category = "conan_build")

    return [
        ConanPackageInfo(
            reference = ctx.attrs.reference,
            package_id = ctx.attrs.package_id,
            cache_out = cache_out,
            package_out = package_out,
        ),
        DefaultInfo(
            default_outputs = [package_out],
            other_outputs = [
                install_folder,
                output_folder,
                user_home,
                manifests,
                install_info,
                trace_log,
                cache_out,
            ],
        ),
    ]

conan_package = rule(
    impl = _conan_package_impl,
    attrs = {
        "lockfile": attrs.source(doc = "The Conan lockfile defining the package and its dependencies."),
        "reference": attrs.string(doc = "The Conan package reference <name>/<version>#<revision>."),
        "package_id": attrs.string(doc = "The Conan package-id."),
        "deps": attrs.list(attrs.dep(providers = [ConanPackageInfo], doc = "Conan Package dependencies.")),
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_init": attrs.dep(providers = [ConanInitInfo], default = "toolchains//:conan-init"),
        "_conan_package": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_package"),
    },
)

def _profile_env_tool(name, tool):
    return cmd_args([name, cmd_args(tool, delimiter = " ")], delimiter = "=", quote = "shell")

def _conan_profile_impl(ctx: "context") -> ["provider"]:
    cxx = ctx.attrs._cxx_toolchain[CxxToolchainInfo]

    content = cmd_args()

    content.add("[settings]")
    content.add(cmd_args(ctx.attrs.arch, format = "arch={}"))
    content.add(cmd_args(ctx.attrs.os, format = "os={}"))
    content.add(cmd_args(ctx.attrs.build_type, format = "build_type={}"))
    # TODO[AH] Define translation of CxxToolProviderType to compiler setting.
    content.add(cmd_args(ctx.attrs.compiler, format = "compiler={}"))
    content.add(cmd_args(ctx.attrs.compiler_version, format = "compiler.version={}"))
    content.add(cmd_args(ctx.attrs.compiler_libcxx, format = "compiler.libcxx={}"))

    content.add("")
    content.add("[env]")
    # TODO[AH] Define CMAKE_FIND_ROOT_PATH
    # TODO[AH] Define CMAKE_SYSROOT
    # TODO[AH] Do we need to overwrite PATH?
    # TODO[AH] Define target CHOST for cross-compilation
    content.add(_profile_env_tool("AR", cxx.linker_info.archiver))
    if cxx.as_compiler_info:
        content.add(_profile_env_tool("AS", cxx.as_compiler_info.compiler))
        # TODO[AH] Use asm_compiler_info for Windows
    if cxx.binary_utilities_info:
        if cxx.binary_utilities_info.nm:
            content.add(_profile_env_tool("NM", cxx.binary_utilities_info.nm))
        if cxx.binary_utilities_info.ranlib:
            content.add(_profile_env_tool("RANLIB", cxx.binary_utilities_info.ranlib))
        if cxx.binary_utilities_info.strip:
            content.add(_profile_env_tool("STRIP", cxx.binary_utilities_info.strip))
    if cxx.c_compiler_info:
        content.add(_profile_env_tool("CC", cxx.c_compiler_info.compiler))
        content.add(_profile_env_tool("CFLAGS", cxx.c_compiler_info.compiler_flags))
    if cxx.cxx_compiler_info:
        content.add(_profile_env_tool("CXX", cxx.cxx_compiler_info.compiler))
        content.add(_profile_env_tool("CXXFLAGS", cxx.cxx_compiler_info.compiler_flags))

    output = ctx.actions.declare_output(ctx.label.name)
    ctx.actions.write(output, content)

    return [DefaultInfo(default_outputs = [output])]

conan_profile = rule(
    impl = _conan_profile_impl,
    attrs = {
        "arch": attrs.string(doc = "The target architecture"),
        "os": attrs.string(doc = "The target operating system"),
        "build_type": attrs.string(doc = "The Conan build-type, e.g. Release or Debug"),
        "compiler": attrs.string(doc = "The name of the C/C++ compiler, e.g. gcc, clang, or Visual Studio."),
        "compiler_version": attrs.string(doc = "The version of the C/C++ compiler, e.g. 12.2 for gcc, 15 for clang, or 17 for Visual Studio."),
        "compiler_libcxx": attrs.string(doc = "The C++ standard library, e.g. libstdc++, or libc++"),
        "_cxx_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:cxx", providers = [CxxToolchainInfo])),
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

def _lock_generate_impl(ctx: "context") -> ["provider"]:
    lock_generate = ctx.attrs._lock_generate[RunInfo]

    targets_out = ctx.actions.declare_output(ctx.label.name + ".bzl")

    cmd = cmd_args([lock_generate])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--lockfile-label", str(ctx.attrs.lockfile.owner.raw_target())])
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
