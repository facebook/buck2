# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load(":erlang_dependencies.bzl", "ErlAppDependencies")
load(
    ":erlang_info.bzl",
    "ErlangAppIncludeInfo",
    "ErlangAppInfo",
    "ErlangTestInfo",
)
load(":erlang_paths.bzl", "strip_extension")
load(
    ":erlang_toolchain.bzl",
    "Toolchain",  # @unused Used as type
)
load(":erlang_utils.bzl", "action_identifier")

_BUILD_DIR = "__build"

# mapping
#   from include base name (e.g. "header.hrl")
#   to artifact
PathArtifactMapping = dict[str, Artifact]

# mapping
#   from application to includes in that application
#   the artifacts are source .hrl files
IncludesMapping = dict[str, PathArtifactMapping]

# mapping
#   from module name to beam file location
ModuleArtifactMapping = dict[str, Artifact]

# mapping
#   from application to beam files it defines
EbinMapping = dict[str, ModuleArtifactMapping]

# mapping
#   from application to merged deps files
DepsMapping = dict[str, Artifact]

BuildEnvironment = record(
    includes = field(IncludesMapping, {}),
    include_dirs = field(PathArtifactMapping, {}),
    private_includes = field(IncludesMapping, {}),
    private_include_dirs = field(PathArtifactMapping, {}),
    # private includes that are not "peeked", used for conflict detection
    hidden_private_includes = field(IncludesMapping, {}),
    hidden_private_include_dirs = field(PathArtifactMapping, {}),
    beams = field(EbinMapping, {}),
    header_deps_files = field(DepsMapping, {}),
)

SmallBuildEnvironment = record(
    includes = field(IncludesMapping, {}),
    include_dirs = field(PathArtifactMapping, {}),
    private_includes = field(IncludesMapping, {}),
    private_include_dirs = field(PathArtifactMapping, {}),
    beams = field(EbinMapping, {}),
)

DepInfo = record(
    dep_file = field(Artifact),
    path = field(str),
)

def _prepare_build_environment(
        ctx: AnalysisContext,
        dependencies: ErlAppDependencies,
        includes_target: [ErlangAppIncludeInfo, None] = None) -> BuildEnvironment:
    """Prepare build environment and collect the context from all dependencies."""
    includes = {}
    include_dirs = {}
    all_includes = {}
    header_deps_files = {}
    hidden_private_includes = {}
    hidden_private_include_dirs = {}
    beams = {}
    all_beams = {}

    if includes_target:
        include_dirs[includes_target.name] = includes_target.include_dir
        includes[includes_target.name] = includes_target.includes
        header_deps_files[includes_target.name] = includes_target.header_deps_file
        for hrl in includes_target.includes:
            all_includes[hrl] = includes_target.name

    for name in dependencies:
        dep = dependencies[name]

        if ErlangAppInfo in dep:
            dep_info = dep[ErlangAppInfo]

            if dep_info.virtual:
                # virtual applications don't directories we need to include
                # we skip this entire step
                continue

            # collect beams
            new_beams = dep_info.beams
            for mod in new_beams:
                if mod in all_beams:
                    _fail_dep_conflict("module", mod, all_beams[mod], name)
                all_beams[mod] = name
            beams[name] = new_beams

            # collect private includes
            hidden_private_include_dirs[name] = dep_info.private_include_dir
            new_includes = dep_info.private_includes
            for hrl in new_includes:
                if hrl in all_includes:
                    _fail_dep_conflict("header", hrl, all_includes[hrl], name)
                all_includes[hrl] = name
            hidden_private_includes[name] = new_includes

        elif ErlangAppIncludeInfo in dep:
            dep_info = dep[ErlangAppIncludeInfo]

            if dep_info.name == ctx.attrs.name:
                continue
        elif ErlangTestInfo in dep:
            # we only care about application deps
            continue
        else:
            fail("invalid dep {}", dep)

        # collect includes
        include_dirs[name] = dep_info.include_dir
        new_includes = dep_info.includes
        for hrl in new_includes:
            if hrl in all_includes:
                _fail_dep_conflict("header", hrl, all_includes[hrl], name)
            all_includes[hrl] = name
        includes[name] = new_includes

        # collect header_deps_files
        if dep_info.header_deps_file:
            header_deps_files[name] = dep_info.header_deps_file

    return BuildEnvironment(
        includes = includes,
        include_dirs = include_dirs,
        private_includes = {},
        private_include_dirs = {},
        hidden_private_includes = hidden_private_includes,
        hidden_private_include_dirs = hidden_private_include_dirs,
        header_deps_files = header_deps_files,
        beams = beams,
    )

def _fail_dep_conflict(kind: str, name: str, app1: str, app2: str) -> None:
    fail("conflicting {} `{}` found, defined in applications '{}' and '{}'".format(kind, name, app1, app2))

def _generated_source_artifacts(ctx: AnalysisContext, toolchain: Toolchain, name: str) -> PathArtifactMapping:
    """Generate source output artifacts and build actions for generated erl files."""

    def build(src, custom_include_opt):
        return _build_xyrl(
            ctx,
            toolchain,
            src,
            custom_include_opt,
            ctx.actions.declare_output(generated_erl_path(name, src)),
        )

    yrl_outputs = {module_name(src): build(src, "yrl_includefile") for src in ctx.attrs.srcs if _is_yrl(src)}
    xrl_outputs = {module_name(src): build(src, "xrl_includefile") for src in ctx.attrs.srcs if _is_xrl(src)}
    return yrl_outputs | xrl_outputs

# mutates build_environment in place
def _generate_include_artifacts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: BuildEnvironment,
        name: str,
        header_artifacts: list[Artifact],
        is_private: bool = False):
    include_files = {hrl.basename: hrl for hrl in header_artifacts}
    dir_name = "{}_private".format(name) if is_private else name
    include_dir = ctx.actions.symlinked_dir(paths.join(_BUILD_DIR, dir_name, "include"), include_files)

    # dep files
    deps_files = _get_deps_files(ctx, toolchain, header_artifacts)

    # detect conflicts
    for file in deps_files:
        for app in build_environment.includes:
            if file in build_environment.includes[app]:
                _fail_dep_conflict("header", file, name, app)
        for app in build_environment.private_includes:
            if file in build_environment.private_includes[app]:
                _fail_dep_conflict("header", file, name, app)
        for app in build_environment.hidden_private_includes:
            if file in build_environment.hidden_private_includes[app]:
                _fail_dep_conflict("header", file, name, app)

    if name in build_environment.header_deps_files:
        build_environment.header_deps_files[name] = _merged_deps_file(ctx, toolchain, name, deps_files, is_private, build_environment.header_deps_files[name])
    else:
        file = _merged_deps_file(ctx, toolchain, name, deps_files, is_private, None)
        if file:
            build_environment.header_deps_files[name] = file

    # construct updates build environment
    if not is_private:
        # fields for public include directory
        build_environment.includes[name] = include_files
        build_environment.include_dirs[name] = include_dir
    else:
        # fields for private include directory
        build_environment.private_includes[name] = include_files
        build_environment.private_include_dirs[name] = include_dir

def _merged_deps_file(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        name: str,
        deps_files: PathArtifactMapping,
        is_private: bool,
        previous_merged_file: [Artifact, None]) -> [Artifact, None]:
    """Merge the deps files of the headers into a single deps file."""

    if not deps_files:
        return previous_merged_file

    name = "{}-private".format(name) if is_private else name

    file_name = _dep_merged_name(name)
    merged_file = ctx.actions.declare_output(file_name)
    deps_files_json = ctx.actions.write_json(file_name + ".json", deps_files, with_inputs = True)

    cmd = cmd_args(toolchain.dependency_merger, merged_file.as_output(), deps_files_json)
    if previous_merged_file:
        cmd.add(previous_merged_file)

    _run_with_env(
        ctx,
        toolchain,
        cmd,
        category = "dependency_merger",
        identifier = action_identifier(toolchain, name),
    )

    return merged_file

# mutates build_environment in place
def _generate_beam_artifacts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: BuildEnvironment,
        name: str,
        src_artifacts: list[Artifact]):
    ebin = paths.join(_BUILD_DIR, "ebin")

    beam_mapping = {}
    for erl in src_artifacts:
        module = module_name(erl)
        beam_mapping[module] = ctx.actions.declare_output(ebin, "{}.beam".format(module))

    # detect conflicts
    for key in beam_mapping:
        for app in build_environment.beams:
            if key in build_environment.beams[app]:
                _fail_dep_conflict("module", key, name, app)

    build_environment.beams[name] = beam_mapping

    # dep files
    deps_files = _get_deps_files(ctx, toolchain, src_artifacts)
    dep_info_file = ctx.actions.write_json(_dep_info_name(), build_environment.header_deps_files, with_inputs = True)

    small_build_environment = SmallBuildEnvironment(
        includes = build_environment.includes,
        include_dirs = build_environment.include_dirs,
        private_includes = build_environment.private_includes,
        private_include_dirs = build_environment.private_include_dirs,
        beams = build_environment.beams,
    )

    for erl in src_artifacts:
        _build_erl(ctx, toolchain, small_build_environment, deps_files, dep_info_file, erl, beam_mapping[module_name(erl)])

def _get_deps_files(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        srcs: list[Artifact]):
    """Mapping from the output path to the deps file artifact for each srcs artifact and dependencies."""

    return {src.basename: _get_deps_file(ctx, toolchain, src) for src in srcs}

def _get_deps_file(ctx: AnalysisContext, toolchain: Toolchain, src: Artifact) -> Artifact:
    dependency_json = ctx.actions.declare_output(_dep_file_name(src))

    _run_with_env(
        ctx,
        toolchain,
        cmd_args(toolchain.dependency_analyzer, src, dependency_json.as_output()),
        category = "dependency_analyzer",
        identifier = action_identifier(toolchain, src.short_path),
    )
    return dependency_json

def _build_xyrl(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        xyrl: Artifact,
        custom_include_opt: str,
        output: Artifact) -> Artifact:
    """Generate an erl file out of an xrl or yrl input file."""
    erlc = toolchain.otp_binaries.erlc
    custom_include = getattr(ctx.attrs, custom_include_opt, None)
    cmd = cmd_args(erlc)
    if custom_include:
        cmd.add("-I", custom_include)
    cmd.add("-o", cmd_args(output.as_output(), parent = 1), xyrl)
    _run_with_env(
        ctx,
        toolchain,
        cmd,
        category = "erlc",
        identifier = action_identifier(toolchain, xyrl.basename),
    )
    return output

def _build_erl(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: SmallBuildEnvironment,
        beam_deps_files: PathArtifactMapping,
        dep_info_file: WriteJsonCliArgs,
        src: Artifact,
        output: Artifact) -> None:
    """Compile erl files into beams."""

    final_dep_file = ctx.actions.declare_output(_dep_final_name(src))
    initial_dep_file = beam_deps_files[src.basename]
    _run_with_env(
        ctx,
        toolchain,
        cmd_args(toolchain.dependency_finalizer, initial_dep_file, dep_info_file, final_dep_file.as_output()),
        category = "dependency_finalizer",
        identifier = action_identifier(toolchain, src.basename),
    )

    def dynamic_lambda(ctx: AnalysisContext, artifacts, outputs):
        trampoline = toolchain.erlc_trampoline
        erlc = toolchain.otp_binaries.erlc
        erl_opts = _get_erl_opts(ctx, toolchain, src)
        deps_args, mapping = _dependencies_to_args(artifacts, final_dep_file, build_environment)
        erlc_cmd = cmd_args(
            trampoline,
            erlc,
            erl_opts,
            deps_args,
            "-o",
            cmd_args(outputs[output].as_output(), parent = 1),
            src,
        )
        mapping_file = ctx.actions.write_json(_dep_mapping_name(src), mapping)
        _run_with_env(
            ctx,
            toolchain,
            erlc_cmd,
            category = "erlc",
            identifier = action_identifier(toolchain, src.basename),
            env = {"BUCK2_FILE_MAPPING": mapping_file},
            always_print_stderr = True,
        )

    ctx.actions.dynamic_output(dynamic = [final_dep_file], inputs = [src], outputs = [output.as_output()], f = dynamic_lambda)
    return None

def _dependencies_to_args(
        artifacts,
        final_dep_file: Artifact,
        build_environment: SmallBuildEnvironment) -> (cmd_args, dict[str, (bool, [str, Artifact])]):
    """Add the transitive closure of all per-file Erlang dependencies as specified in the deps files to the `args` with .hidden.
    """
    includes = set()
    include_libs = set()
    beams = set()
    precise_includes = []

    input_mapping = {}
    deps = artifacts[final_dep_file].read_json()

    # silently ignore not found dependencies and let erlc report the not found stuff
    for dep in deps:
        file = dep["file"]
        if dep["type"] == "include_lib":
            app = dep["app"]
            if app in build_environment.includes:
                # if file doesn't exist in app, let the compiler fail
                if file in build_environment.includes[app]:
                    include_dir = build_environment.include_dirs[app]
                    include_libs.add(include_dir)
                    precise_includes.append(include_dir.project(file))
                    input_mapping[file] = (True, build_environment.includes[app][file])
            else:
                # the file might come from OTP
                input_mapping[file] = (False, paths.join(app, "include", file))

        elif dep["type"] == "include":
            # these includes can either reside in the private includes or the public ones
            found_private = False

            # we've checked for duplicates earlier, we'll find at most one
            for app in build_environment.private_includes:
                if file in build_environment.private_includes[app]:
                    include_dir = build_environment.private_include_dirs[app]
                    includes.add(include_dir)
                    precise_includes.append(include_dir.project(file))
                    input_mapping[file] = (True, build_environment.private_includes[app][file])
                    found_private = True
                    break

            if not found_private:
                # we've checked for duplicates earlier, we'll find at most one
                for app in build_environment.includes:
                    if file in build_environment.includes[app]:
                        include_dir = build_environment.include_dirs[app]
                        includes.add(include_dir)
                        precise_includes.append(include_dir.project(file))
                        input_mapping[file] = (True, build_environment.includes[app][file])
                        break

        elif (dep["type"] == "behaviour" or
              dep["type"] == "parse_transform" or
              dep["type"] == "manual_dependency"):
            module = strip_extension(file)

            # we made sure earlier there are no conflicts, we'll find at most one
            for app in build_environment.beams:
                beam = build_environment.beams[app].get(module, None)
                if beam != None:
                    beams.add(beam)
                    break

        else:
            fail("unrecognized dependency type %s", (dep["type"]))

    args = cmd_args(
        cmd_args(list(includes), format = "-I{}", ignore_artifacts = True),
        cmd_args(list(include_libs), format = "-I{}", parent = 2, ignore_artifacts = True),
        cmd_args(list(beams), format = "-pa{}", parent = 1),
        hidden = precise_includes,
    )

    return args, input_mapping

def _get_erl_opts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        src: Artifact) -> cmd_args:
    always = ["+deterministic"]

    # use erl_opts defined in taret if present
    if getattr(ctx.attrs, "erl_opts", None) == None:
        opts = toolchain.erl_opts
    else:
        opts = ctx.attrs.erl_opts

    # dedupe options
    opts = dedupe(opts + always)

    # build args
    args = cmd_args(opts)

    # add parse_transforms
    parse_transforms = dict(toolchain.core_parse_transforms)
    if getattr(ctx.attrs, "use_global_parse_transforms", True):
        for parse_transform in toolchain.parse_transforms:
            if (
                # add parse_transform if there is no filter set
                not parse_transform in toolchain.parse_transforms_filters or
                # or if module is listed in the filter and add conditionally
                module_name(src) in toolchain.parse_transforms_filters[parse_transform]
            ):
                parse_transforms[parse_transform] = toolchain.parse_transforms[parse_transform]

    for parse_transform in parse_transforms:
        (beam, resource_folder) = parse_transforms[parse_transform]
        args.add(
            "+{parse_transform, %s}" % (parse_transform,),
            cmd_args(beam, format = "-pa{}", parent = 1),
        )
        args.add(cmd_args(hidden = resource_folder))

    source = cmd_args(src, format = "{source, \"{}\"}")
    path_type = "{path_type, relative}"
    preserved_opts = _preserved_opts(opts)

    compile_info = cmd_args(source, path_type, preserved_opts, delimiter = ", ")

    # add relevant compile_info manually
    args.add(cmd_args(compile_info, format = "+{compile_info, [{}]}"))

    return args

def is_preservable_opt(opt: str) -> bool:
    return opt in ["+beam_debug_info", "+debug_info", "+inline", "+line_coverage"]

def _preserved_opts(opts: list[str]) -> cmd_args:
    """Options that should be preserved in the beam file despite +determinstic"""
    preserved = [opt.lstrip("+") for opt in opts if is_preservable_opt(opt)]

    joined = cmd_args(preserved, delimiter = ", ")
    return cmd_args(joined, format = "{options, [{}]}")

def generated_erl_path(appname: str, src: Artifact) -> str:
    """The output path for generated erl files."""
    return paths.join(
        _BUILD_DIR,
        "__generated_%s" % (appname,),
        "%s.erl" % (module_name(src),),
    )

def module_name(in_file: Artifact) -> str:
    """ Returns the basename of the artifact without extension """
    end = in_file.basename.rfind(".")
    return in_file.basename[:end]

def _is_hrl(in_file: Artifact) -> bool:
    """ Returns True if the artifact is a hrl file """
    return _is_ext(in_file, ".hrl")

def _is_erl(in_file: Artifact) -> bool:
    """ Returns True if the artifact is an erl file """
    return _is_ext(in_file, ".erl")

def _is_yrl(in_file: Artifact) -> bool:
    """ Returns True if the artifact is a yrl file """
    return _is_ext(in_file, ".yrl")

def _is_xrl(in_file: Artifact) -> bool:
    """ Returns True if the artifact is a xrl file """
    return _is_ext(in_file, ".xrl")

def _is_config(in_file: Artifact) -> bool:
    """ Returns True if the artifact is a config file """
    return _is_ext(in_file, ".config")

def _is_ext(in_file: Artifact, extension: str) -> bool:
    """ Returns True if the artifact has an extension listed in extensions """
    return in_file.basename.endswith(extension)

def _dep_file_name(src: Artifact) -> str:
    return paths.join(
        _BUILD_DIR,
        "__dep_files",
        "{}.dep".format(src.short_path),
    )

def _dep_final_name(src: Artifact) -> str:
    return paths.join(
        _BUILD_DIR,
        "__dep_files",
        "{}.final.dep".format(src.short_path),
    )

def _dep_merged_name(name: str) -> str:
    return paths.join(
        _BUILD_DIR,
        "__dep_files",
        "{}.merged.dep".format(name),
    )

def _dep_mapping_name(src: Artifact) -> str:
    return paths.join(
        _BUILD_DIR,
        "__dep_files",
        "{}.mapping".format(src.short_path),
    )

def _dep_info_name() -> str:
    return paths.join(
        _BUILD_DIR,
        "__dep_files",
        "app.info.dep",
    )

def _run_with_env(ctx: AnalysisContext, toolchain: Toolchain, args: cmd_args, **kwargs):
    """ run interfact that injects env"""

    # use os_env defined in target if present
    if getattr(ctx.attrs, "os_env", None) == None:
        env = toolchain.env
    else:
        env = ctx.attrs.os_env

    if "env" in kwargs:
        kwargs["env"].update(env)
    else:
        kwargs["env"] = env

    ctx.actions.run(args, **kwargs)

# mutates build_environment in place
def _peek_private_includes(
        ctx: AnalysisContext,
        build_environment: BuildEnvironment,
        force_peek: bool = False):
    if not (force_peek or ctx.attrs.peek_private_includes):
        return

    build_environment.private_includes.update(build_environment.hidden_private_includes)
    build_environment.private_include_dirs.update(build_environment.hidden_private_include_dirs)
    build_environment.hidden_private_includes.clear()
    build_environment.hidden_private_include_dirs.clear()

# export

erlang_build = struct(
    prepare_build_environment = _prepare_build_environment,
    build_steps = struct(
        generated_source_artifacts = _generated_source_artifacts,
        generate_include_artifacts = _generate_include_artifacts,
        generate_beam_artifacts = _generate_beam_artifacts,
    ),
    utils = struct(
        BUILD_DIR = _BUILD_DIR,
        is_hrl = _is_hrl,
        is_erl = _is_erl,
        is_yrl = _is_yrl,
        is_xrl = _is_xrl,
        is_config = _is_config,
        module_name = module_name,
        run_with_env = _run_with_env,
        peek_private_includes = _peek_private_includes,
    ),
)
