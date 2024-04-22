# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:preprocessor.bzl",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(
    "@prelude//haskell:library_info.bzl",
    "HaskellLibraryInfoTSet",
)
load(
    "@prelude//haskell:toolchain.bzl",
    "HaskellToolchainInfo",
)
load(
    "@prelude//haskell:util.bzl",
    "attr_deps_haskell_lib_infos",
    "attr_deps_haskell_link_infos",
    "get_artifact_suffix",
    "is_haskell_src",
    "output_extensions",
    "srcs_to_pairs",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)

# The type of the return value of the `_compile()` function.
CompileResultInfo = record(
    objects = field(Artifact),
    hi = field(Artifact),
    stubs = field(Artifact),
    producing_indices = field(bool),
)

CompileArgsInfo = record(
    result = field(CompileResultInfo),
    srcs = field(cmd_args),
    args_for_cmd = field(cmd_args),
    args_for_file = field(cmd_args),
)

PackagesInfo = record(
    exposed_package_args = cmd_args,
    packagedb_args = cmd_args,
    transitive_deps = field(HaskellLibraryInfoTSet),
)

def _package_flag(toolchain: HaskellToolchainInfo) -> str:
    if toolchain.support_expose_package:
        return "-expose-package"
    else:
        return "-package"

def get_packages_info(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        specify_pkg_version: bool,
        enable_profiling: bool) -> PackagesInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    # Collect library dependencies. Note that these don't need to be in a
    # particular order.
    direct_deps_link_info = attr_deps_haskell_link_infos(ctx)
    libs = ctx.actions.tset(
        HaskellLibraryInfoTSet,
        children = [
            lib.prof_info[link_style] if enable_profiling else lib.info[link_style]
            for lib in direct_deps_link_info
        ],
    )

    # base is special and gets exposed by default
    package_flag = _package_flag(haskell_toolchain)
    exposed_package_args = cmd_args([package_flag, "base"])

    packagedb_args = cmd_args()
    packagedb_set = {}

    for lib in libs.traverse():
        packagedb_set[lib.db] = None
        exposed_package_args.hidden(lib.import_dirs.values())
        exposed_package_args.hidden(lib.stub_dirs)

        # libs of dependencies might be needed at compile time if
        # we're using Template Haskell:
        exposed_package_args.hidden(lib.libs)

        packagedb_args.hidden(lib.import_dirs.values())
        packagedb_args.hidden(lib.stub_dirs)
        packagedb_args.hidden(lib.libs)

    # These we need to add for all the packages/dependencies, i.e.
    # direct and transitive (e.g. `fbcode-common-hs-util-hs-array`)
    packagedb_args.add([cmd_args("-package-db", x) for x in packagedb_set])

    haskell_direct_deps_lib_infos = attr_deps_haskell_lib_infos(
        ctx,
        link_style,
        enable_profiling,
    )

    # Expose only the packages we depend on directly
    for lib in haskell_direct_deps_lib_infos:
        pkg_name = lib.name
        if (specify_pkg_version):
            pkg_name += "-{}".format(lib.version)

        exposed_package_args.add(package_flag, pkg_name)

    return PackagesInfo(
        exposed_package_args = exposed_package_args,
        packagedb_args = packagedb_args,
        transitive_deps = libs,
    )

def compile_args(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool,
        pkgname = None,
        suffix: str = "") -> CompileArgsInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]

    compile_cmd = cmd_args()
    compile_cmd.add(haskell_toolchain.compiler_flags)

    # Some rules pass in RTS (e.g. `+RTS ... -RTS`) options for GHC, which can't
    # be parsed when inside an argsfile.
    compile_cmd.add(ctx.attrs.compiler_flags)

    compile_args = cmd_args()
    compile_args.add("-no-link", "-i")

    if enable_profiling:
        compile_args.add("-prof")

    if link_style == LinkStyle("shared"):
        compile_args.add("-dynamic", "-fPIC")
    elif link_style == LinkStyle("static_pic"):
        compile_args.add("-fPIC", "-fexternal-dynamic-refs")

    osuf, hisuf = output_extensions(link_style, enable_profiling)
    compile_args.add("-osuf", osuf, "-hisuf", hisuf)

    if getattr(ctx.attrs, "main", None) != None:
        compile_args.add(["-main-is", ctx.attrs.main])

    artifact_suffix = get_artifact_suffix(link_style, enable_profiling, suffix)

    objects = ctx.actions.declare_output(
        "objects-" + artifact_suffix,
        dir = True,
    )
    hi = ctx.actions.declare_output("hi-" + artifact_suffix, dir = True)
    stubs = ctx.actions.declare_output("stubs-" + artifact_suffix, dir = True)

    compile_args.add(
        "-odir",
        objects.as_output(),
        "-hidir",
        hi.as_output(),
        "-hiedir",
        hi.as_output(),
        "-stubdir",
        stubs.as_output(),
    )

    # Add -package-db and -package/-expose-package flags for each Haskell
    # library dependency.
    packages_info = get_packages_info(
        ctx,
        link_style,
        specify_pkg_version = False,
        enable_profiling = enable_profiling,
    )

    compile_args.add(packages_info.exposed_package_args)
    compile_args.add(packages_info.packagedb_args)

    # Add args from preprocess-able inputs.
    inherited_pre = cxx_inherited_preprocessor_infos(ctx.attrs.deps)
    pre = cxx_merge_cpreprocessors(ctx, [], inherited_pre)
    pre_args = pre.set.project_as_args("args")
    compile_args.add(cmd_args(pre_args, format = "-optP={}"))

    if pkgname:
        compile_args.add(["-this-unit-id", pkgname])

    srcs = cmd_args()
    for (path, src) in srcs_to_pairs(ctx.attrs.srcs):
        # hs-boot files aren't expected to be an argument to compiler but does need
        # to be included in the directory of the associated src file
        if is_haskell_src(path):
            srcs.add(src)
        else:
            srcs.hidden(src)

    producing_indices = "-fwrite-ide-info" in ctx.attrs.compiler_flags

    return CompileArgsInfo(
        result = CompileResultInfo(
            objects = objects,
            hi = hi,
            stubs = stubs,
            producing_indices = producing_indices,
        ),
        srcs = srcs,
        args_for_cmd = compile_cmd,
        args_for_file = compile_args,
    )

# Compile all the context's sources.
def compile(
        ctx: AnalysisContext,
        link_style: LinkStyle,
        enable_profiling: bool,
        pkgname: str | None = None) -> CompileResultInfo:
    haskell_toolchain = ctx.attrs._haskell_toolchain[HaskellToolchainInfo]
    compile_cmd = cmd_args(haskell_toolchain.compiler)

    args = compile_args(ctx, link_style, enable_profiling, pkgname)

    compile_cmd.add(args.args_for_cmd)

    artifact_suffix = get_artifact_suffix(link_style, enable_profiling)

    if args.args_for_file:
        if haskell_toolchain.use_argsfile:
            argsfile = ctx.actions.declare_output(
                "haskell_compile_" + artifact_suffix + ".argsfile",
            )
            for_file = cmd_args(args.args_for_file).add(args.srcs)
            ctx.actions.write(argsfile.as_output(), for_file, allow_args = True)
            compile_cmd.add(cmd_args(argsfile, format = "@{}"))
            compile_cmd.hidden(for_file)
        else:
            compile_cmd.add(args.args_for_file)
            compile_cmd.add(args.srcs)

    artifact_suffix = get_artifact_suffix(link_style, enable_profiling)
    ctx.actions.run(
        compile_cmd,
        category = "haskell_compile_" + artifact_suffix.replace("-", "_"),
        no_outputs_cleanup = True,
    )

    return args.result
