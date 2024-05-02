# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_library.bzl", "cxx_compile_srcs")
load(
    "@prelude//cxx:cxx_sources.bzl",
    "CxxSrcWithFlags",
)
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    "@prelude//cxx:cxx_types.bzl",
    "CxxRuleConstructorParams",  # @unused Used as a type
)
load("@prelude//cxx:headers.bzl", "cxx_attr_header_namespace", "cxx_get_regular_cxx_headers_layout", "prepare_headers")
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
    "CPreprocessorInfo",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//utils:cmd_script.bzl", "ScriptOs", "cmd_script")
load("@prelude//utils:expect.bzl", "expect")
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_env_vars")

# A map of expected linkages for provided link style
_LINKAGE_FOR_LINK_STYLE = {
    LinkStyle("static"): Linkage("static"),
    LinkStyle("static_pic"): Linkage("static"),
    LinkStyle("shared"): Linkage("shared"),
}

def _cgo(
        ctx: AnalysisContext,
        srcs: list[Artifact],
        own_pre: list[CPreprocessor],
        inherited_pre: list[CPreprocessorInfo],
        c_flags: list[str],
        cpp_flags: list[str]) -> (list[Artifact], list[Artifact], list[Artifact], Artifact):
    """
    Run `cgo` on `.go` sources to generate Go, C, and C-Header sources.
    """
    gen_dir = ctx.actions.declare_output("cgo_gen_tmp", dir = True)

    go_srcs = []
    c_headers = []
    c_srcs = []
    go_srcs.append(gen_dir.project("_cgo_gotypes.go"))
    c_srcs.append(gen_dir.project("_cgo_export.c"))
    c_headers.append(gen_dir.project("_cgo_export.h"))
    for src in srcs:
        go_srcs.append(gen_dir.project(paths.replace_extension(src.basename, ".cgo1.go")))
        c_srcs.append(gen_dir.project(paths.replace_extension(src.basename, ".cgo2.c")))

    # Return a `cmd_args` to use as the generated sources.
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]

    cmd = cmd_args(
        go_toolchain.cgo_wrapper,
        cmd_args(go_toolchain.cgo, format = "--cgo={}"),
        cmd_args(gen_dir.as_output(), format = "--output={}"),
        "--",
        c_flags + cpp_flags,
        ctx.attrs.compiler_flags,
        srcs,
    )

    env = get_toolchain_env_vars(go_toolchain)
    env["CC"] = _cxx_wrapper(ctx, own_pre, inherited_pre)

    ctx.actions.run(cmd, env = env, category = "cgo")

    return go_srcs, c_headers, c_srcs, gen_dir

def _cxx_wrapper(ctx: AnalysisContext, own_pre: list[CPreprocessor], inherited_pre: list[CPreprocessorInfo]) -> cmd_args:
    pre = cxx_merge_cpreprocessors(ctx, own_pre, inherited_pre)
    pre_args = pre.set.project_as_args("args")
    pre_include_dirs = pre.set.project_as_args("include_dirs")

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    expect(CxxToolchainInfo in ctx.attrs._cxx_toolchain)
    cxx_toolchain = ctx.attrs._cxx_toolchain[CxxToolchainInfo]

    c_compiler = cxx_toolchain.c_compiler_info

    # Construct the full C/C++ command needed to preprocess/compile sources.
    cxx_cmd = cmd_args(
        c_compiler.compiler,
        c_compiler.preprocessor_flags,
        c_compiler.compiler_flags,
        pre_args,
        pre_include_dirs,
        go_toolchain.c_compiler_flags,
    )

    # Wrap the C/C++ command in a wrapper script to avoid arg length limits.
    return cmd_script(
        ctx = ctx,
        name = "cxx_wrapper",
        cmd = cxx_cmd,
        os = ScriptOs("windows" if ctx.attrs._exec_os_type[OsLookup].platform == "windows" else "unix"),
    )

# build CPreprocessor similar as cxx_private_preprocessor_info does, but with our filtered headers
def _own_pre(ctx: AnalysisContext, h_files: list[Artifact]) -> CPreprocessor:
    namespace = cxx_attr_header_namespace(ctx)
    header_map = {paths.join(namespace, h.short_path): h for h in h_files}
    header_root = prepare_headers(ctx, header_map, "h_files-private-headers")

    return CPreprocessor(
        args = CPreprocessorArgs(args = ["-I", header_root.include_path] if header_root != None else []),
    )

def build_cgo(ctx: AnalysisContext, cgo_files: list[Artifact], h_files: list[Artifact], c_files: list[Artifact], c_flags: list[str], cpp_flags: list[str]) -> (list[Artifact], list[Artifact], Artifact):
    if len(cgo_files) == 0:
        return [], [], ctx.actions.copied_dir("cgo_gen_tmp", {})

    # Gather preprocessor inputs.
    own_pre = _own_pre(ctx, h_files)
    inherited_pre = cxx_inherited_preprocessor_infos(ctx.attrs.deps)

    # Separate sources into C++ and GO sources.
    go_gen_srcs, c_gen_headers, c_gen_srcs, gen_dir = _cgo(ctx, cgo_files, [own_pre], inherited_pre, c_flags, cpp_flags)

    # Wrap the generated CGO C headers in a CPreprocessor object for compiling.
    cgo_headers_pre = CPreprocessor(args = CPreprocessorArgs(args = [
        "-I",
        prepare_headers(
            ctx,
            {h.basename: h for h in c_gen_headers},
            "cgo-private-headers",
        ).include_path,
    ]))

    link_style = ctx.attrs.link_style
    if link_style == None:
        link_style = "static"
    linkage = _LINKAGE_FOR_LINK_STYLE[LinkStyle(link_style)]

    # Copmile C++ sources into object files.
    c_compile_cmds = cxx_compile_srcs(
        ctx,
        CxxRuleConstructorParams(
            rule_type = "cgo_library",
            headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
            srcs = [CxxSrcWithFlags(file = src) for src in c_files + c_gen_srcs],
            compiler_flags = c_flags + ctx.attrs.compiler_flags,
            lang_compiler_flags = ctx.attrs.lang_compiler_flags,
            platform_compiler_flags = ctx.attrs.platform_compiler_flags,
            lang_platform_compiler_flags = ctx.attrs.lang_platform_compiler_flags,
            preprocessor_flags = cpp_flags + ctx.attrs.preprocessor_flags,
            lang_preprocessor_flags = ctx.attrs.lang_preprocessor_flags,
            platform_preprocessor_flags = ctx.attrs.platform_preprocessor_flags,
            lang_platform_preprocessor_flags = ctx.attrs.lang_platform_preprocessor_flags,
        ),
        # Create private header tree and propagate via args.
        [own_pre, cgo_headers_pre],
        inherited_pre,
        [],
        linkage,
    )

    compiled_objects = c_compile_cmds.pic.objects

    return go_gen_srcs, compiled_objects, gen_dir
