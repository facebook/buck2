# Implementation of the `genrule` build rule.

# Some rules have to be run locally for various reasons listed next to the label.
_LOCAL_LABELS = [
    # Used for buck2 tests that want to run locally
    "buck2_test_local_exec",

    # Split dwarf merge rules currently don't properly list their inputs.
    "dwp",

    # Bolt and hottext post-processing rules operate on a large statically
    # linked binary which contains non-deterministic build info, meaning its
    # a) currently too large for RE to handle and b) caching it would only
    # waste cache space.
    "postprocess_bolt",
    "postprocess_hottext",

    # The iOS build needs to run a genrule locally to gather non-deterministic
    # build info from `hg`.
    "non_deterministic_build_info",

    # Some call "buck run" & "buck root" recursively.
    "uses_buck_run",

    # Some antlir genrules use clowder for downloading from everstore
    "uses_clowder",

    # Some antlir genrules use cpio for unpacking rpms
    "uses_cpio",

    # Creates secondary Eden repos outside of `buck-out/`
    "uses_eden_mounts",

    # The Antlir core compiler uses sudo
    "uses_sudo",

    # Some rules utilize hg for some reason as part of code generation
    "uses_hg",

    # Dotslash is not yet supported on RE.
    "uses_dotslash",

    # Directly uses the smcc binary which is not on RE.
    "uses_smcc",

    # Uses shasum which is not on RE.
    "uses_shasum",

    # Uses xz which is not on RE.
    "uses_xz",

    # Uses tw tool which is not on RE.
    "uses_tw",

    # Yarn installs use a large in-repo yarn repo that's ~6.1GB at the time of
    # writing, and so v1 uses workarounds (D17359502) to avoid the overhead this
    # would causes.  So, run these rules locally to maintain compatiblity and
    # until we have a better yarn solution.
    "yarn_install",

    # Non-deterministic builds that depend on data from configerator.
    "reads_configerator",

    # Third party java artifacts are stored in manifold and therefore can't be accessed from RE worker.
    "third_party_java",

    # The antlir packge-at-build-time rules current rely on tools like hg/git,
    # which don't work on RE (e.g. fbcode//ti/platform/edgeos/base_image/rootfs:OPSFILES_CHEF_SOLO_BUNDLE__centos8__fna_persistent).
    "antlir_macros",

    # UPM codegen does lots of network I/O (e.g. scuba, JK, configerator), which
    # makes it fail on RE.
    "upm_binary_gen",

    # PHP isn't available in RE or in our repos so, for now, we run them locally
    # (https://fb.workplace.com/groups/1042353022615812/posts/1849505965233843/).
    "uses_php",

    # mksquashfs isn't available in RE, so run these locally
    # (https://fb.workplace.com/groups/buck2users/permalink/3023630007893360/)
    "uses_mksquashfs",

    # PXL rules can't yet run on RE.
    "pxl",

    # Accesses dewey
    "uses_dewey",

    # Accesses justknobs configuration
    "justknobs",

    # Side effecting writes directly into buck-out on the local
    # filesystem
    "writes_to_buck_out",

    # Calculates and writes absolute paths in the local filesystem
    "uses_local_filesystem_abspaths",

    # Uses fbpkg outside of the repo
    "uses_fbpkg",

    # Makes recursive calls to buck
    "uses_buck",

    # Uses files in the repo that it doesn't declare as dependencies
    "uses_undeclared_inputs",

    # Connects to service router which won't work on RE
    "uses_service_router",

    # Downloads direct from manifold
    "uses_manifold",

    # When run on RE produces "Cache is out of space" (excessive disk/memory)
    "re_cache_out_of_space",

    # HHVM Post-link rules need to be local since the binary is huge.
    "hhvm_postlink",

    # Uses network access (unspecified what as of yet)
    "network_access",

    # Uses clang format which is not in RE
    "uses_clang_format",
]

# Currently, some rules require running from the project root, so provide an
# opt-in list for those here.  Longer-term, these should be ported to actual
# rule implementations in v2, rather then using `genrule`s.
_BUILD_ROOT_LABELS = [
    # The buck2 test suite
    "buck2_test_build_root",
    "antlir_macros",
    "rust_bindgen",
    "haskell_hsc",
    "cql_cxx_genrule",
    "clang-module",
    "cuda_build_root",
    "bundle_pch_genrule",  # Compiles C++, and so need to run from build root
    "lpm_package",
]

# In Buck1 the SRCS environment variable is only set if the substring SRCS is on the command line.
# That's a horrible heuristic, and doesn't account for users accessing $SRCS from a shell script.
# But in some cases, $SRCS is so large it breaks the process limit, so have a label to opt in to
# that behavior.
_NO_SRCS_ENVIRONMENT_LABEL = "no_srcs_environment"

def _requires_build_root(ctx: "context") -> bool.type:
    for label in ctx.attr.labels:
        if label in _BUILD_ROOT_LABELS:
            return True
    return False

def _requires_local(ctx: "context") -> bool.type:
    for label in ctx.attr.labels:
        if label in _LOCAL_LABELS:
            return True
    return False

def _ignore_artifacts(ctx: "context") -> bool.type:
    return "buck2_ignore_artifacts" in ctx.attr.labels

def _requires_no_srcs_environment(ctx: "context") -> bool.type:
    return _NO_SRCS_ENVIRONMENT_LABEL in ctx.attr.labels

# There is a special use case of `default_outs` which is pretty frequent:
# ```
# default_outs = ["."],
# ```
# which makes the whole $OUT directory a default output.
# To handle it in a v1 compatible way create an auxiliary symlinked directory with all output artifacts
# and return it as a single output.
def _should_handle_special_case_whole_out_dir_is_output(ctx: "context", outs_attr: dict.type):
    for (_, item_outputs) in outs_attr.items():
        # Situation when `"."` is both in `outs` and `default_outs` is handled by default
        if "." in item_outputs:
            return False
    default_outs = ctx.attr.default_outs
    if default_outs and default_outs[0] == ".":
        if len(default_outs) != 1:
            fail("When present, `.` should be a single element in `default_outs`.")
        return True
    return False

def genrule_impl(ctx: "context") -> ["provider"]:
    # Directories:
    #   sh - sh file
    #   src - sources files
    #   out - where outputs go
    # `src` is the current directory
    # Buck1 uses `.` as output, but that won't work since
    # Buck2 clears the output directory before execution, and thus src/sh too.
    return process_genrule(ctx, ctx.attr.out, ctx.attr.outs)

def _declare_output(ctx: "context", path: str.type) -> "artifact":
    if path == ".":
        return ctx.actions.declare_output("out")
    else:
        return ctx.actions.declare_output("out", path)

def process_genrule(
        ctx: "context",
        out_attr: [str.type, None],
        outs_attr: [dict.type, None],
        extra_env_vars: dict.type = {},
        identifier: [str.type, None] = None) -> ["provider"]:
    if (out_attr != None) and (outs_attr != None):
        fail("Only one of `out` and `outs` should be set. Got out=`%s`, outs=`%s`" % (repr(out_attr), repr(outs_attr)))

    handle_whole_out_dir_is_output = False
    default_out_map = {}

    # TODO(cjhopman): verify output paths are ".", "./", or forward-relative.
    if out_attr != None:
        out_env = out_attr
        out_artifact = _declare_output(ctx, out_attr)
        default_outputs = [out_artifact]
        all_outputs = default_outputs
        named_outputs = {}
    elif outs_attr != None:
        out_env = ""

        default_outputs = []
        all_outputs = []
        named_outputs = {}
        default_out_paths = ctx.attr.default_outs or []

        handle_whole_out_dir_is_output = _should_handle_special_case_whole_out_dir_is_output(ctx, outs_attr)

        for (name, this_outputs) in outs_attr.items():
            output_artifacts = []
            for path in this_outputs:
                artifact = _declare_output(ctx, path)
                if path in default_out_paths:
                    default_outputs.append(artifact)
                output_artifacts.append(artifact)
                default_out_map[path] = artifact
            named_outputs[name] = output_artifacts
            all_outputs.extend(output_artifacts)

        if handle_whole_out_dir_is_output:
            # handle it later when artifacts are bound
            pass
        elif len(default_outputs) != len(default_out_paths):
            # TODO(akozhevnikov) handle arbitrary `default_out`, currently fallback to all outputs to support
            # cases when `default_out` points to directory containing all files from `outs`
            warning("Could not properly handle `default_outs` and `outs` parameters of `{}` rule, default outputs for the rule are defaulted to all artifacts from `outs` parameter.".format(ctx.label))
            default_outputs = all_outputs
        elif len(default_outputs) == 0:
            # We want building to force something to be built, so make sure it contains at least one artifact
            default_outputs = all_outputs
    else:
        fail("One of `out` or `outs` should be set. Got `%s`" % repr(ctx.attr))

    cmd = ctx.attr.bash if ctx.attr.bash != None else ctx.attr.cmd

    if _ignore_artifacts(ctx):
        cmd = cmd_args(cmd).ignore_artifacts()

    if type(ctx.attr.srcs) == type([]):
        # FIXME: We should always use the short_path, but currently that is sometimes blank.
        # See fbcode//buck2/tests/targets/rules/genrule:genrule-dot-input for a test that exposes it.
        symlinks = {src.short_path: src for src in ctx.attr.srcs}

        if len(symlinks) != len(ctx.attr.srcs):
            for src in ctx.attr.srcs:
                name = src.short_path
                if symlinks[name] != src:
                    msg = "genrule srcs include duplicative name: `{}`. ".format(name)
                    msg += "`{}` conflicts with `{}`".format(symlinks[name].owner, src.owner)
                    fail(msg)
    else:
        symlinks = ctx.attr.srcs
    srcs_artifact = ctx.actions.symlinked_dir("srcs" if not identifier else "{}-srcs".format(identifier), symlinks)

    # Setup environment variables.
    srcs = cmd_args()
    for symlink in symlinks:
        srcs.add(cmd_args(srcs_artifact, format = "./{}/" + symlink))
    env_vars = {
        "ASAN_OPTIONS": cmd_args("detect_leaks=0,detect_odr_violation=0"),
        "GEN_DIR": cmd_args("GEN_DIR_DEPRECATED"),  # ctx.relpath(ctx.output_root_dir(), srcs_path)
        "OUT": cmd_args(srcs_artifact, format = "./{}/../out/" + out_env),
        "SRCDIR": cmd_args(srcs_artifact, format = "./{}"),
        "SRCS": srcs,
    }

    if _requires_no_srcs_environment(ctx):
        env_vars.pop("SRCS")

    for key, value in extra_env_vars.items():
        env_vars[key] = value

    # Create required directories.
    script = [
        cmd_args(srcs_artifact, format = "mkdir -p ./{}/../out"),
        cmd_args("export TMP=${TMPDIR:-/tmp}"),
    ]

    # Actually define the operation, relative to where we changed to
    script.append(cmd_args(cmd))

    # Some rules need to run from the build root, but for everything else, `cd`
    # into the sandboxed source dir and relative all paths to that.
    if not _requires_build_root(ctx):
        script = (
            # Change to the directory that genrules expect.
            [cmd_args(srcs_artifact, format = "cd {}")] +
            # Relative all paths in the command to the sandbox dir.
            [cmd.relative_to(srcs_artifact) for cmd in script]
        )

        # Relative all paths in the env to the sandbox dir.
        env_vars = {key: val.relative_to(srcs_artifact) for key, val in env_vars.items()}

    sh_script, macro_files = ctx.actions.write(
        "sh/genrule.sh" if not identifier else "sh/{}-genrule.sh".format(identifier),
        script,
        is_executable = True,
        allow_args = True,
    )

    category = "genrule"
    if ctx.attr.type != None:
        # As of 09/2021, all genrule types were legal snake case if their dashes and periods were replaced with underscores.
        category += "_" + ctx.attr.type.replace("-", "_").replace(".", "_")
    ctx.actions.run(
        cmd_args(["/bin/bash", sh_script]).hidden([cmd, srcs_artifact, macro_files] + [a.as_output() for a in all_outputs]),
        env = env_vars,
        local_only = _requires_local(ctx),
        category = category,
        identifier = identifier,
    )

    if handle_whole_out_dir_is_output:
        default_outputs = [ctx.actions.symlinked_dir("out_dir" if not identifier else "{}-outdir", default_out_map)]

    providers = [DefaultInfo(
        default_outputs = default_outputs,
        sub_targets = {k: [DefaultInfo(default_outputs = v)] for (k, v) in named_outputs.items()},
    )]

    # The cxx_genrule also forwards here, and that doesn't have .executable, so use getattr
    if getattr(ctx.attr, "executable", False):
        if out_attr == None:
            # Match Buck 1 behavior: default output is the executable and remaining
            # outputs are all materialized.
            providers.append(RunInfo(args = cmd_args(default_outputs).hidden(all_outputs)))
        else:
            providers.append(RunInfo(args = cmd_args(all_outputs)))
    return providers
