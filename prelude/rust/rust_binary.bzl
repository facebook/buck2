load("@fbcode//buck2/prelude/cxx:cxx_link_utility.bzl", "executable_shared_lib_arguments")
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
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
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "flatten_dict", "from_named_set")
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
load(
    ":resources.bzl",
    "create_resource_db",
    "gather_rust_resources",
)
load(":rust_toolchain.bzl", "ctx_toolchain_info")

def rust_attr_resources(ctx: "context") -> {str.type: ("artifact", ["_arglike"])}:
    """
    Return the resources provided by this rule, as a map of resource name to
    a tuple of the resource artifact and any "other" outputs exposed by it.
    """
    resources = {}

    # `getattr` because: `rust_binary` is used from `rust_test`, and currently `rust_test`
    # has not been extended to support the `resources` attribute. As such, it doesn't inject
    # a default value, and so this would explode. If / when `rust_test` is extended with
    # `resources` support, this *should* be safe to remove (barring any other rules that also
    # build on `rust_binary`).
    for name, resource in from_named_set(getattr(ctx.attrs, "resources", {})).items():
        if type(resource) == "artifact":
            other = [resource]
        else:
            info = resource[DefaultInfo]
            expect(
                len(info.default_outputs) == 1,
                "expected exactly one default output from {} ({})"
                    .format(resource, info.default_outputs),
            )
            [resource] = info.default_outputs
            other = info.other_outputs

        resources[name] = (resource, other)

    return resources

def _rust_binary_common(
        ctx: "context",
        default_roots: [str.type],
        extra_flags: [str.type]) -> ([[DefaultInfo.type, RunInfo.type]], "cmd_args"):
    toolchain_info = ctx_toolchain_info(ctx)

    crate = attr_crate(ctx)

    styles = {}
    style_param = {}  # style -> param

    specified_link_style = LinkStyle(ctx.attrs.link_style or "static_pic")
    compile_ctx = compile_context(ctx)

    linker_type = ctx.attrs._cxx_toolchain[CxxToolchainInfo].linker_info.type

    for link_style in LinkStyle:
        params = build_params(
            rule = RuleType("binary"),
            proc_macro = False,
            link_style = link_style,
            preferred_linkage = Linkage("any"),
            lang = LinkageLang("rust"),
            linker_type = linker_type,
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
            ctx = ctx,
            compile_ctx = compile_ctx,
            emits = [Emit("link"), Emit("metadata")],
            crate = crate,
            params = params,
            link_style = link_style,
            default_roots = default_roots,
            extra_link_args = extra_link_args,
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
        ctx = ctx,
        compile_ctx = compile_ctx,
        emit = Emit("expand"),
        crate = crate,
        params = style_param[LinkStyle("static_pic")],
        link_style = LinkStyle("static_pic"),
        default_roots = default_roots,
        predeclared_outputs = {Emit("expand"): ctx.actions.declare_output("expand/{}.rs".format(crate))},
        extra_flags = extra_flags,
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

    # If we have some resources, write it to the resources JSON file and add
    # it and all resources to "runtime_files" so that we make to materialize
    # them with the final binary.
    resources_hidden = []
    resources = flatten_dict(gather_rust_resources(
        label = ctx.label,
        resources = rust_attr_resources(ctx),
    ).values())
    if resources:
        resources_hidden.append(create_resource_db(ctx, link, resources))
        for resource, other in resources.values():
            resources_hidden.append(resource)
            resources_hidden.extend(other)
    runtime_files.extend(resources_hidden)
    args = args.hidden(resources_hidden)

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
    if ctx.attrs.framework:
        extra_flags += ["--test"]

    providers, args = _rust_binary_common(ctx, ["main.rs", "lib.rs"], extra_flags)

    return providers + [
        ExternalRunnerTestInfo(
            type = "rust",
            command = [args],
            env = ctx.attrs.env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
        ),
    ]
