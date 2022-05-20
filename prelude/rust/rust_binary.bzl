load("@fbcode//buck2/prelude/cxx:cxx_link_utility.bzl", "executable_shared_lib_arguments")
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkStyle",
    "Linkage",
)
load(
    "@fbcode//buck2/prelude/linking:shared_libraries.bzl",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load(
    ":build.bzl",
    "compile_context",
    "generate_rustdoc",
    "rust_compile",
    "rust_compile_multi",
)
load(
    ":build_params.bzl",
    "Emit",
    "LinkageLang",
    "RuleType",
    "build_params",
    "output_filename",
)
load(
    ":link_info.bzl",
    "attr_crate",
    "inherited_non_rust_shared_libs",
)
load(":rust_toolchain.bzl", "ctx_toolchain_info")

def _rust_binary_common(
        ctx: "context",
        default_roots: [str.type],
        extra_flags: [str.type]) -> ([[DefaultInfo.type, RunInfo.type]], "cmd_args"):
    toolchain_info = ctx_toolchain_info(ctx)

    crate = attr_crate(ctx)

    styles = {}
    style_param = {}  # style -> param

    specified_link_style = LinkStyle(ctx.attr.link_style or "static_pic")
    compile_ctx = compile_context(ctx)

    for link_style in LinkStyle:
        params = build_params(
            RuleType("binary"),
            False,
            link_style,
            Linkage("any"),
            LinkageLang("rust"),
        )
        style_param[link_style] = params
        name = link_style.value + "/" + output_filename(crate, Emit("link"), params)
        output = ctx.actions.declare_output(name)

        # Gather and setup symlink tree of transitive shared library deps.
        shared_libs = {}

        # As per v1, we only setup a shared library symlink tree for the shared
        # link style.
        # XXX need link tree for dylib crates
        if link_style == LinkStyle("shared"):
            shlib_info = merge_shared_libraries(
                ctx.actions,
                deps = inherited_non_rust_shared_libs(ctx),
            )
            for soname, shared_lib in traverse_shared_library_info(shlib_info).items():
                shared_libs[soname] = shared_lib.lib
        extra_link_args, runtime_files = executable_shared_lib_arguments(ctx, output, shared_libs)

        extra_flags = toolchain_info.rustc_binary_flags + (extra_flags or [])

        # Compile rust binary.
        link, meta = rust_compile_multi(
            ctx,
            compile_ctx,
            [Emit("link"), Emit("metadata")],
            crate,
            params,
            link_style,
            default_roots,
            extra_link_args,
            predeclared_outputs = {Emit("link"): output},
            extra_flags = extra_flags,
            is_binary = True,
        )

        args = cmd_args(link.outputs[Emit("link")]).hidden(runtime_files)
        extra_targets = [("check", meta.outputs[Emit("metadata")])] + meta.diag.items()
        if Emit("save-analysis") in meta.outputs:
            extra_targets += [("save-analysis", meta.outputs[Emit("save-analysis")])]

        styles[link_style] = (link.outputs[Emit("link")], args, extra_targets, runtime_files)

    expand = rust_compile(
        ctx,
        compile_ctx,
        Emit("expand"),
        crate,
        style_param[LinkStyle("static_pic")],
        LinkStyle("static_pic"),
        default_roots,
        extra_flags = extra_flags,
        predeclared_outputs = {Emit("expand"): ctx.actions.declare_output("expand/{}.rs".format(crate))},
    )

    extra_targets += [
        ("doc", generate_rustdoc(
            ctx = ctx,
            compile_ctx = compile_ctx,
            crate = crate,
            params = style_param[LinkStyle("static_pic")],
            default_roots = default_roots,
            document_private_items = True,
        )),
        ("expand", expand.outputs[Emit("expand")]),
    ]
    sub_targets = {k: [DefaultInfo(default_outputs = [v])] for k, v in extra_targets}
    for (k, (sub_link, sub_args, _sub_extra, sub_runtime_files)) in styles.items():
        sub_targets[k.value] = [
            DefaultInfo(
                default_outputs = [sub_link],
                other_outputs = sub_runtime_files,
                # Check/save-analysis for each link style?
                # sub_targets = { k: [DefaultInfo(default_outputs = [v])] for k, v in sub_extra }
            ),
            RunInfo(args = sub_args),
        ]

    (link, args, extra_targets, runtime_files) = styles[specified_link_style]

    providers = [
        DefaultInfo(
            default_outputs = [link],
            other_outputs = runtime_files,
            sub_targets = sub_targets,
        ),
        RunInfo(args = args),
    ]

    return (providers, args)

def rust_binary_impl(ctx: "context") -> [[DefaultInfo.type, RunInfo.type]]:
    providers, _ = _rust_binary_common(ctx, ["main.rs"], [])

    return providers

def rust_test_impl(ctx: "context") -> [[DefaultInfo.type, RunInfo.type, ExternalRunnerTestInfo.type]]:
    toolchain_info = ctx_toolchain_info(ctx)

    extra_flags = toolchain_info.rustc_test_flags or []
    if ctx.attr.framework:
        extra_flags += ["--test"]

    providers, args = _rust_binary_common(ctx, ["main.rs", "lib.rs"], extra_flags)

    return providers + [
        ExternalRunnerTestInfo(
            type = "rust",
            command = [args],
            env = ctx.attr.env,
            labels = ctx.attr.labels,
            contacts = ctx.attr.contacts,
            use_templated_api = False,
        ),
    ]
