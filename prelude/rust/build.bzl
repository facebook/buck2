load("@fbcode//buck2/prelude/cxx:cxx_link_utility.bzl", "make_link_args")
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    "@fbcode//buck2/prelude/cxx:linker.bzl",
    "get_default_shared_library_name",
    "get_shared_library_name_linker_flags",
)
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkArgs",
    "LinkStyle",  #@unused Used as a type
    "get_link_args",
)
load("@fbcode//buck2/prelude/utils:set.bzl", "set")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")
load(
    ":build_params.bzl",
    "BuildParams",  # @unused Used as a type
    "CrateType",
    "Emit",
    "crate_type_codegen",
    "crate_type_linked",
    "emit_needs_codegen",
    "output_filename",
)
load(
    ":failure_filter.bzl",
    "RustFailureFilter",
    "failure_filter",
)
load(
    ":link_info.bzl",
    "RustLinkInfo",
    "inherited_non_rust_link_info",
    "normalize_crate",
    "resolve_deps",
    "style_info",
)
load(":rust_toolchain.bzl", "ctx_toolchain_info")

# Struct for sharing common args between rustc and rustdoc
# (rustdoc just relays bunch of the same args to rustc when trying to gen docs)
CommonArgsInfo = record(
    args = field("cmd_args"),
    subdir = field(str.type),
    tempfile = field(str.type),
    short_cmd = field(str.type),
    is_check = field(bool.type),
    crate_map = field({str.type: "label"}),
)

# Compile info which is reusable between multiple compilation command performed
# by the same rule.
CompileContext = record(
    # Symlink root containing all sources.
    symlinked_srcs = field("artifact"),
    # Linker args to pass the linker wrapper to rustc.
    linker_args = field("cmd_args"),
    # Clippy wrapper (wrapping clippy-driver so it has the same CLI as rustc)
    clippy_wrapper = field("cmd_args"),
    # Memoized common args for reuse
    common_args = field({(CrateType.type, Emit.type, LinkStyle.type): CommonArgsInfo.type}),
)

RustcOutput = record(
    outputs = field({Emit.type: "artifact"}),
    diag = field({str.type: "artifact"}),
)

def compile_context(ctx: "context") -> CompileContext.type:
    # Setup source symlink tree.
    srcs = ctx.attr.srcs
    mapped_srcs = ctx.attr.mapped_srcs
    symlinks = {src.short_path: src for src in srcs}
    symlinks.update({k: v for v, k in mapped_srcs.items()})
    symlinked_srcs = ctx.actions.symlinked_dir("__srcs", symlinks)

    linker = _linker_args(ctx)
    clippy_wrapper = _clippy_wrapper(ctx)

    return CompileContext(
        symlinked_srcs = symlinked_srcs,
        linker_args = linker,
        clippy_wrapper = clippy_wrapper,
        common_args = {},
    )

def generate_rustdoc(
        ctx: "context",
        compile_ctx: CompileContext.type,
        crate: str.type,
        # link style doesn't matter, but caller should pass in build params
        # with static-pic (to get best cache hits for deps)
        params: BuildParams.type,
        default_roots: [str.type]) -> "artifact":
    common_args = _compute_common_args(
        ctx,
        compile_ctx,
        # to make sure we get the rmeta's generated for the crate dependencies,
        # rather than full .rlibs
        Emit("metadata"),
        crate,
        params,
        params.dep_link_style,
        default_roots,
    )

    subdir = common_args.subdir + "_rustdoc"
    output = ctx.actions.declare_output(subdir)
    rustdoc_cmd = cmd_args([
        ctx_toolchain_info(ctx).rustdoc,
        # TODO it seems we need to generate a long list of
        # crate=https://www.internalfb.com/intern/rustdoc/...
        # and pass those to --extern-html-root-url to get
        # cross-crate hyperlinks. below is just a hardcoded demo
        "--extern-html-root-url",
        "fbinit=https://www.internalfb.com/intern/rustdoc/common/rust/shed/fbinit:fbinit/",
        "-o",
        output.as_output(),
        common_args.args,
    ])
    ctx.actions.run(rustdoc_cmd, category = "rustdoc")
    return output

# Generate multiple compile artifacts so that distinct sets of artifacts can be
# generated concurrently.
def rust_compile_multi(
        ctx: "context",
        compile_ctx: CompileContext.type,
        emits: [Emit.type],
        crate: str.type,
        params: BuildParams.type,
        link_style: LinkStyle.type,
        default_roots: [str.type],
        extra_link_args: [""] = [],
        predeclared_outputs: {Emit.type: "artifact"} = {},
        extra_flags: [str.type] = [],
        is_binary: bool.type = False) -> [RustcOutput.type]:
    outputs = []

    for emit in emits:
        outs = rust_compile(
            ctx,
            compile_ctx,
            emit,
            crate,
            params,
            link_style,
            default_roots,
            extra_link_args,
            predeclared_outputs,
            extra_flags,
            is_binary,
        )
        outputs.append(outs)

    return outputs

# Generate a compilation action. A single instance of rustc can emit
# numerous output artifacts, so return an artifact object for each of
# them.
def rust_compile(
        ctx: "context",
        compile_ctx: CompileContext.type,
        emit: Emit.type,
        crate: str.type,
        params: BuildParams.type,
        link_style: LinkStyle.type,
        default_roots: [str.type],
        extra_link_args: [""] = [],
        predeclared_outputs: {Emit.type: "artifact"} = {},
        extra_flags: [str.type] = [],
        is_binary: bool.type = False) -> RustcOutput.type:
    toolchain_info = ctx_toolchain_info(ctx)

    lints, clippy_lints = _lint_flags(ctx)

    common_args = _compute_common_args(ctx, compile_ctx, emit, crate, params, link_style, default_roots)
    rustc_cmd = cmd_args(
        [common_args.args] +
        [
            "--remap-path-prefix",
            cmd_args(compile_ctx.symlinked_srcs, format = "{}/=" + ctx.label.package),
            compile_ctx.linker_args,
        ] +
        # Normalize working directory
        ["-Zremap-cwd-prefix="] +
        # Report unused --extern crates in the notification stream
        (["--json=unused-externs", "-Wunused-crate-dependencies"] if toolchain_info.report_unused_deps else []) +
        extra_flags +
        lints,
    )

    if crate_type_linked(params.crate_type) and not common_args.is_check:
        subdir = common_args.subdir
        tempfile = common_args.tempfile

        # If this crate type has an associated native dep link style, include deps
        # of that style.
        (link_args, hidden, _dwo_dir_unused_in_rust) = make_link_args(
            ctx,
            [
                LinkArgs(flags = extra_link_args),
                get_link_args(
                    inherited_non_rust_link_info(ctx),
                    link_style,
                ),
            ],
            "{}-{}".format(subdir, tempfile),
        )
        linker_argsfile, macro_files = ctx.actions.write(
            "{}/__{}_linker_args.txt".format(subdir, tempfile),
            link_args,
            allow_args = True,
        )
        rustc_cmd.add(cmd_args(linker_argsfile, format = "-Clink-arg=@{}"))
        rustc_cmd.hidden(macro_files)
        rustc_cmd.hidden(hidden)

    # If we're using failure filtering then we need to make sure the final
    # artifact location is the predeclared one since its specific path may have
    # already been encoded into the other compile args (eg rpath). So we still
    # let rustc_emits generate its own output artifacts, and then make sure we
    # use the predeclared one as the output after the failure filter action
    # below. Otherwise we'll use the predeclared outputs directly.
    if toolchain_info.failure_filter:
        outputs, emit_args = _rustc_emits(ctx, emit, {}, common_args.subdir, crate, params)
    else:
        outputs, emit_args = _rustc_emits(ctx, emit, predeclared_outputs, common_args.subdir, crate, params)

    (diag, build_status) = _rustc_invoke(
        ctx,
        compile_ctx,
        "{}/{}".format(common_args.subdir, common_args.tempfile),
        cmd_args([toolchain_info.compiler, rustc_cmd] + emit_args),
        "diag",
        outputs.values(),
        common_args.short_cmd,
        is_binary,
        common_args.crate_map,
    )

    # Add clippy diagnostic targets for check builds
    if common_args.is_check:
        # We don't really need the outputs from this build, just to keep the artifact accounting straight
        clippy_out, clippy_emit_args = _rustc_emits(ctx, emit, {}, common_args.subdir + "-clippy", crate, params)
        (clippy_diag, _) = _rustc_invoke(
            ctx,
            compile_ctx,
            "{}/{}".format(common_args.subdir, common_args.tempfile),
            cmd_args([compile_ctx.clippy_wrapper, rustc_cmd] + clippy_lints + clippy_emit_args),
            "clippy",
            clippy_out.values(),
            common_args.short_cmd,
            False,
            common_args.crate_map,
        )
        diag.update(clippy_diag)

    if toolchain_info.failure_filter:
        # Filter each output through a failure filter
        filtered_outputs = {}
        for (emit, output) in outputs.items():
            # This is only needed when this action's output is being used as an
            # input, so we only need standard diagnostics (clippy is always
            # asked for explicitly).
            stderr = diag["diag.txt"]
            filter_prov = RustFailureFilter(buildstatus = build_status, required = output, stderr = stderr)

            filtered_outputs[emit] = failure_filter(
                ctx,
                "{}/{}".format(common_args.subdir, emit.value),
                predeclared_outputs.get(emit),
                filter_prov,
                common_args.short_cmd,
            )
    else:
        filtered_outputs = outputs

    return RustcOutput(outputs = filtered_outputs, diag = diag)

# --extern <crate>=<path> for direct dependencies
# -Ldependency=<dir> for transitive dependencies
# For native dependencies, we use -Clink-arg=@argsfile
# Second element of result tuple is a list of files/directories that should be present for executable to be run successfully
# Third return is the mapping from crate names back to targets (needed so that a deps linter knows what deps need fixing)
def _dependency_args(
        ctx: "context",
        subdir: str.type,
        crate_type: CrateType.type,
        link_style: LinkStyle.type,
        is_check: bool.type) -> (["_arg"], {str.type: "label"}):
    args = cmd_args()
    transitive_deps = {}
    deps = []
    crate_targets = {}
    for x in resolve_deps(ctx):
        crate = x.name and normalize_crate(x.name)
        dep = x.dep

        deps.append(dep)

        # Rust dependency
        info = dep[RustLinkInfo]
        if info == None:
            continue
        crate = crate or info.crate

        style = style_info(info, link_style)

        # Use rmeta dependencies whenever possible because they
        # should be cheaper to produce.
        if is_check or (ctx_toolchain_info(ctx).pipelined and not crate_type_codegen(crate_type)):
            artifact = style.rmeta
            transitive_artifacts = style.transitive_rmeta_deps
        else:
            artifact = style.rlib
            transitive_artifacts = style.transitive_deps

        args.add("--extern")
        flags = ""
        if x.flags != []:
            flags = ",".join(x.flags) + ":"
        args.add(cmd_args([flags + crate, "=", artifact], delimiter = ""))
        crate_targets[crate] = dep.label

        # Unwanted transitive_deps have already been excluded
        transitive_deps.update(transitive_artifacts)

    # Add as many -Ldependency dirs as we need to avoid name conflicts
    deps_dirs = [{}]
    for dep in transitive_deps.keys():
        name = dep.basename
        if name in deps_dirs[-1]:
            deps_dirs.append({})
        deps_dirs[-1][name] = dep

    for idx, srcs in enumerate(deps_dirs):
        deps_dir = "{}-deps{}-{}".format(subdir, ("-check" if is_check else ""), idx)
        dep_link_dir = ctx.actions.symlinked_dir(deps_dir, srcs = srcs)
        args.add(cmd_args(dep_link_dir), format = "-Ldependency={}")

    return ([args], crate_targets)

def _lintify(flag: str.type, clippy: bool.type, lints: ["resolved_macro"]) -> ["cmd_args"]:
    return [cmd_args(lint, format = "-{}{{}}".format(flag)) for lint in lints if str(lint).startswith("\"clippy::") == clippy]

def _lint_flags(ctx: "context") -> (["cmd_args"], ["cmd_args"]):
    toolchain_info = ctx_toolchain_info(ctx)

    plain = (
        _lintify("A", False, toolchain_info.allow_lints) +
        _lintify("D", False, toolchain_info.deny_lints) +
        _lintify("W", False, toolchain_info.warn_lints)
    )

    clippy = (
        _lintify("A", True, toolchain_info.allow_lints) +
        _lintify("D", True, toolchain_info.deny_lints) +
        _lintify("W", True, toolchain_info.warn_lints)
    )

    return (plain, clippy)

# Compute which are common to both rustc and rustdoc
def _compute_common_args(
        ctx: "context",
        compile_ctx: CompileContext.type,
        emit: Emit.type,
        crate: str.type,
        params: BuildParams.type,
        link_style: LinkStyle.type,
        default_roots: [str.type]) -> CommonArgsInfo.type:
    crate_type = params.crate_type

    args_key = (crate_type, emit, link_style)
    if args_key in compile_ctx.common_args:
        return compile_ctx.common_args[args_key]

    # Keep filenames distinct in per-flavour subdirs
    subdir = "{}-{}-{}".format(crate_type.value, params.reloc_model.value, link_style.value)

    # Included in tempfiles
    tempfile = "{}-{}".format(crate, emit.value)

    srcs = ctx.attr.srcs
    mapped_srcs = ctx.attr.mapped_srcs
    all_srcs = map(lambda s: s.short_path, srcs) + mapped_srcs.values()
    crate_root = ctx.attr.crate_root or _crate_root(all_srcs, crate, default_roots)

    is_check = not emit_needs_codegen(emit)

    dependency_args, crate_map = _dependency_args(
        ctx,
        subdir,
        crate_type,
        link_style,
        is_check,
    )

    if crate_type == CrateType("proc-macro"):
        dependency_args.extend(["--extern", "proc_macro"])

    if crate_type == CrateType("cdylib") and not is_check:
        linker_type = ctx.attr._cxx_toolchain[CxxToolchainInfo].linker_info.type
        shlib_name = get_default_shared_library_name(linker_type, ctx.label)
        dependency_args.extend([
            "-Clink-arg={}".format(a)
            for a in get_shared_library_name_linker_flags(linker_type, shlib_name)
        ])

    toolchain_info = ctx_toolchain_info(ctx)
    args = cmd_args(
        [
            cmd_args([compile_ctx.symlinked_srcs, "/", crate_root], delimiter = ""),
            "--crate-name={}".format(crate),
            "--crate-type={}".format(crate_type.value),
            "-Crelocation-model={}".format(params.reloc_model.value),
            "--edition={}".format(ctx.attr.edition or toolchain_info.default_edition),
            "-Cmetadata={}".format(_metadata(ctx.label)[0]),
            # Make diagnostics json with the option to extract rendered text
            "--error-format=json",
            "--json=diagnostic-rendered-ansi",
        ] +
        (["-Cprefer-dynamic=yes"] if crate_type == CrateType("dylib") else []) +
        toolchain_info.rustc_flags +
        (toolchain_info.rustc_check_flags if is_check else []) +
        ctx.attr.rustc_flags +
        _feature_args(ctx) +
        dependency_args,
    )

    common_args = CommonArgsInfo(
        args = args,
        subdir = subdir,
        tempfile = tempfile,
        short_cmd = "{},{},{}".format(crate_type.value, params.reloc_model.value, emit.value),
        is_check = is_check,
        crate_map = crate_map,
    )

    compile_ctx.common_args[args_key] = common_args
    return common_args

# Return wrapper script for clippy-driver to make sure sysroot is set right
# We need to make sure clippy is using the same sysroot - compiler, std libraries -
# as rustc itself, so explicitly invoke rustc to get the path. This is a
# (small - ~15ms per invocation) perf hit but only applies when generating
# specifically requested clippy diagnostics.
def _clippy_wrapper(ctx: "context") -> "cmd_args":
    clippy_driver = cmd_args(ctx_toolchain_info(ctx).clippy_driver)
    rustc = cmd_args(ctx_toolchain_info(ctx).compiler)

    wrapper_file, macro_files = ctx.actions.write(
        ctx.actions.declare_output("__clippy_driver_wrapper.sh"),
        [
            "#!/bin/bash",
            # Force clippy to be clippy: https://github.com/rust-lang/rust-clippy/blob/e405c68b3c1265daa9a091ed9b4b5c5a38c0c0ba/src/driver.rs#L334
            "export __CLIPPY_INTERNAL_TESTS=true",
            cmd_args(rustc, format = "export SYSROOT=$({} --print=sysroot)"),
            cmd_args(clippy_driver, format = "{} \"$@\"\n"),
        ],
        is_executable = True,
        allow_args = True,
    )

    return cmd_args(wrapper_file).hidden(macro_files + [clippy_driver, rustc])

# This is a hack because we need to pass the linker to rustc
# using -Clinker=path and there is currently no way of doing this
# without an artifact. We create a wrapper (which is an artifact),
# and add -Clinker=
def _linker_args(ctx: "context") -> "cmd_args":
    linker_info = ctx.attr._cxx_toolchain[CxxToolchainInfo].linker_info
    linker_flags = linker_info.linker_flags or []
    linker = cmd_args(linker_info.linker)
    linker.add(linker_flags)
    linker.add(ctx.attr.linker_flags)

    # Now we create a wrapper to actually run the linker. Use $(cat <<heredoc) to
    # combine the multiline command into a single logical command.
    wrapper, macro_files = ctx.actions.write(
        ctx.actions.declare_output("__linker_wrapper.sh"),
        [
            "#!/bin/bash",
            cmd_args(_shell_quote(linker), delimiter = " \\\n", format = "{} \"$@\"\n"),
        ],
        is_executable = True,
        allow_args = True,
    )

    return cmd_args(wrapper, format = "-Clinker={}").hidden([linker] + macro_files)

def _shell_quote(args: "cmd_args") -> "cmd_args":
    return cmd_args(args, quote = "shell")

def _feature_args(ctx: "context") -> [str.type]:
    args = []
    for feature in ctx.attr.features:
        args.append("--cfg")
        args.append('feature="{}"'.format(feature))
    return args

# Returns the full label and its hash. The full label is used for `-Cmetadata`
# which provided the primary disambiguator for two otherwise identically named
# crates. The hash is added to the filename to give them a lower likelihood of
# duplicate names, but it doesn't matter if they collide.
def _metadata(label: "label") -> (str.type, str.type):
    label = str(label.raw_target())
    h = hash(label)
    if h < 0:
        h = -h
    h = "%x" % h
    return (label, "0" * (8 - len(h)) + h)

def _crate_root(
        srcs: [str.type],
        crate: str.type,
        default_roots: [str.type]) -> str.type:
    candidates = set()
    crate_with_suffix = crate + ".rs"
    for src in srcs:
        filename = src.split("/")[-1]
        if filename in default_roots or filename == crate_with_suffix:
            candidates.insert(src)

    candidates = candidates.list()
    if len(candidates) == 1:
        return candidates[0]

    fail("Could not infer crate_root. candidates=%s\nAdd 'crate_root = \"src/example.rs\"' to your attributes to disambiguate." % candidates)

# Take a desired output and work out how to convince rustc to generate it
def _rustc_emits(
        ctx: "context",
        emit: Emit.type,
        predeclared_outputs: {Emit.type: "artifact"},
        subdir: str.type,
        crate: str.type,
        params: BuildParams.type) -> ({Emit.type: "artifact"}, [["cmd_args", str.type]]):
    toolchain_info = ctx_toolchain_info(ctx)
    crate_type = params.crate_type

    expect(emit != Emit("save-analysis"), "Don't specify 'save-analysis' in emits directly")

    # Metadata for pipelining needs has enough info to be used as an input
    # for dependents. To do this reliably, we actually emit "link" but
    # suppress actual codegen with -Zno-codegen.
    #
    # We don't bother to do this with "codegen" crates - ie, ones which are
    # linked into an artifact like binaries and dylib, since they're not
    # used as a pipelined dependency input.
    pipeline_meta = emit == Emit("metadata") and \
                    toolchain_info.pipelined and \
                    not crate_type_codegen(crate_type)

    emit_args = []
    extra_hash = ""
    if emit in predeclared_outputs:
        output = predeclared_outputs[emit]
    else:
        extra_hash = "-" + _metadata(ctx.label)[1]
        emit_args += ["-Cextra-filename=" + extra_hash]
        if pipeline_meta:
            # Make sure hollow rlibs are distinct from real ones
            filename = subdir + "/hollow/" + output_filename(crate, Emit("link"), params, extra_hash)
        else:
            filename = subdir + "/" + output_filename(crate, emit, params, extra_hash)

        output = ctx.actions.declare_output(filename)

    outputs = {emit: output}

    if pipeline_meta:
        # If we're doing a pipelined build, instead of emitting an actual rmeta
        # we emit a "hollow" .rlib - ie, it only contains lib.rmeta and no object
        # code. It should contain full information needed by any dependent
        # crate which is generating code (MIR, etc).
        # Requires https://github.com/rust-lang/rust/pull/86045
        emit_args += [
            "--emit",
            cmd_args(output.as_output(), format = "link={}"),
            "-Zno-codegen",
        ]
    else:
        # Assume https://github.com/rust-lang/rust/issues/85356 is fixed (ie
        # https://github.com/rust-lang/rust/pull/85362 is applied)
        emit_args += ["--emit", cmd_args(output.as_output(), format = emit.value + "={}")]

    if emit == Emit("metadata") and toolchain_info.save_analysis:
        # Emit save-analysis as a bonus output - it doesn't cost much to generate
        # along with metadata. (If it does turn out to be expensive, either make it
        # a per-build option, or split it out into a separate action).
        filename = "{}/save-analysis/{}{}{}.json".format(subdir, params.prefix, crate, extra_hash)
        output = ctx.actions.declare_output(filename)
        outputs[Emit("save-analysis")] = output
        emit_args += [
            "-Zsave-analysis",
            # No way to explicitly set the output location except with the output dir
            "--out-dir",
            cmd_args(output.as_output()).parent(2),
        ]
    else:
        extra_dir = subdir + "/extras/" + output_filename(crate, emit, params)
        extra_dir = ctx.actions.declare_output(extra_dir)
        emit_args += [
            "--out-dir",
            cmd_args(extra_dir.as_output()),
        ]

    return (outputs, emit_args)

# Invoke rustc and capture outputs
def _rustc_invoke(
        ctx: "context",
        compile_ctx: CompileContext.type,
        prefix: str.type,
        rustc_cmd: "cmd_args",
        diag: str.type,
        outputs: ["artifact"],
        short_cmd: str.type,
        is_binary: bool.type,
        crate_map: {str.type: "label"}) -> ({str.type: "artifact"}, ["artifact", None]):
    toolchain_info = ctx_toolchain_info(ctx)

    plain_env, path_env = _process_env(ctx)

    # Save diagnostic outputs
    json_diag = ctx.actions.declare_output("{}-{}.json".format(prefix, diag))
    txt_diag = ctx.actions.declare_output("{}-{}.txt".format(prefix, diag))

    rustc_action = toolchain_info.rustc_action[RunInfo]

    args = [
        rustc_action,
        "--diag-json",
        json_diag.as_output(),
        "--diag-txt",
        txt_diag.as_output(),
        "--buck-target",
        str(ctx.label.raw_target()),
    ]
    for k, v in crate_map.items():
        args.extend(["--crate-map", k, str(v.raw_target())])
    for k, v in plain_env.items():
        args.extend(["--env", k, v])
    for k, v in path_env.items():
        args.extend(["--path-env", k, v])

    build_status = None
    if toolchain_info.failure_filter:
        # Build status for fail filter
        build_status = ctx.actions.declare_output("{}_build_status-{}.json".format(prefix, diag))
        args.extend(["--failure-filter", build_status.as_output()])
        for out in outputs:
            args.extend(["--required-output", out.short_path, out.as_output()])

    args.append(rustc_cmd)

    compile_cmd = cmd_args(args)

    compile_cmd.hidden([toolchain_info.compiler, compile_ctx.symlinked_srcs])

    local_only = is_binary and ctx.attr._cxx_toolchain[CxxToolchainInfo].linker_info.link_binaries_locally
    identifier = "{} {} [{}]".format(prefix, short_cmd, diag)
    ctx.actions.run(compile_cmd, env = plain_env, local_only = local_only, category = "rustc", identifier = identifier)

    return ({diag + ".json": json_diag, diag + ".txt": txt_diag}, build_status)

# Separate env settings into "plain" and "with path". Path env vars are often
# used in Rust `include!()` and similar directives, which always interpret the
# path relative to the source file containing the directive. Since paths in env
# vars are often expanded from macros such as `$(location)`, they will be
# cell-relative which will not work properly. To solve this, we canonicalize
# paths to absolute paths so they'll work in any context. Hence the need to
# distinguish path from non-path. (This will not work if the value contains both
# path and non-path content, but we'll burn that bridge when we get to it.)
def _process_env(
        ctx: "context") -> ({str.type: "cmd_args"}, {str.type: "cmd_args"}):
    # Values with inputs (ie artifact references).
    path_env = {}

    # Plain strings.
    plain_env = {}

    for k, v in ctx.attr.env.items():
        v = cmd_args(v)
        if len(v.inputs) > 0:
            path_env[k] = v
        else:
            plain_env[k] = v

    return (plain_env, path_env)
