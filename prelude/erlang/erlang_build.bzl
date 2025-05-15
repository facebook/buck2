# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(":erlang_dependencies.bzl", "ErlAppDependencies")
load(
    ":erlang_info.bzl",
    "ErlangAppIncludeInfo",
    "ErlangAppInfo",
    "ErlangTestInfo",
)
load(
    ":erlang_toolchain.bzl",
    "Toolchain",  # @unused Used as type
)
load(":erlang_utils.bzl", "action_identifier")

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
#   from application to deps files it computed
DepsMapping = dict[str, PathArtifactMapping]

BuildEnvironment = record(
    includes = field(IncludesMapping, {}),
    include_dirs = field(PathArtifactMapping, {}),
    private_includes = field(IncludesMapping, {}),
    private_include_dirs = field(PathArtifactMapping, {}),
    beams = field(EbinMapping, {}),
    deps_files = field(DepsMapping, {}),
)

DepInfo = record(
    dep_file = field(Artifact),
    path = field(str),
)

def _prepare_build_environment(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        dependencies: ErlAppDependencies,
        includes_target: [ErlangAppIncludeInfo, None] = None) -> BuildEnvironment:
    """Prepare build environment and collect the context from all dependencies."""
    include_dirs = {}
    deps_files = {}
    all_deps_files = {}
    includes = {}
    beams = {}

    if includes_target:
        include_dirs[includes_target.name] = includes_target.include_dir[toolchain.name]
        includes[includes_target.name] = includes_target.includes[toolchain.name]
        all_deps_files = dict(includes_target.deps_files[toolchain.name])
        deps_files[includes_target.name] = dict(includes_target.deps_files[toolchain.name])

    for name in dependencies:
        dep = dependencies[name]

        if ErlangAppInfo in dep:
            dep_info = dep[ErlangAppInfo]

            if dep_info.virtual:
                # virtual applications don't directories we need to include
                # we skip this entire step
                continue

            # collect beams
            beams[name] = dep_info.beams[toolchain.name]

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
        include_dirs[name] = dep_info.include_dir[toolchain.name]
        includes[name] = dep_info.includes[toolchain.name]

        # collect deps_files
        new_deps = dep_info.deps_files[toolchain.name]
        for dep_file in new_deps:
            if dep_file in all_deps_files and all_deps_files[dep_file] != new_deps[dep_file]:
                _fail_dep_conflict(dep_file, all_deps_files[dep_file], new_deps[dep_file])
            all_deps_files[dep_file] = new_deps[dep_file]
        deps_files[name] = new_deps

    return BuildEnvironment(
        includes = includes,
        private_includes = {},
        private_include_dirs = {},
        beams = beams,
        include_dirs = include_dirs,
        deps_files = deps_files,
    )

def _fail_dep_conflict(artifact_name: str, dep1: Artifact, dep2: Artifact) -> None:
    app1_name = dep1.owner.name.removesuffix("_includes_only")
    app2_name = dep2.owner.name.removesuffix("_includes_only")
    fail("conflicting artifact `{}` found, defined in applications '{}' and '{}'".format(artifact_name, app1_name, app2_name))

def _generated_source_artifacts(ctx: AnalysisContext, toolchain: Toolchain, name: str) -> PathArtifactMapping:
    """Generate source output artifacts and build actions for generated erl files."""

    def build(src, custom_include_opt):
        return _build_xyrl(
            ctx,
            toolchain,
            src,
            custom_include_opt,
            ctx.actions.declare_output(generated_erl_path(toolchain, name, src)),
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
    dir_name = name + "_private" if is_private else name
    include_dir = ctx.actions.symlinked_dir(paths.join(_build_dir(toolchain), dir_name, "include"), include_files)

    # dep files
    _get_deps_files(ctx, toolchain, name, header_artifacts, build_environment)

    # construct updates build environment
    if not is_private:
        # fields for public include directory
        build_environment.includes[name] = include_files
        build_environment.include_dirs[name] = include_dir
    else:
        # fields for private include directory
        build_environment.private_includes[name] = include_files
        build_environment.private_include_dirs[name] = include_dir

# mutates build_environment in place
def _generate_beam_artifacts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: BuildEnvironment,
        name: str,
        src_artifacts: list[Artifact]):
    ebin = paths.join(_build_dir(toolchain), "ebin")

    beam_mapping = {}
    for erl in src_artifacts:
        module = module_name(erl)
        beam_mapping[module] = ctx.actions.declare_output(ebin, module + ".beam")

    # dep files
    _get_deps_files(ctx, toolchain, name, src_artifacts, build_environment)

    build_environment.beams[name] = beam_mapping

    dep_info_file = ctx.actions.write_json(_dep_info_name(toolchain), build_environment.deps_files, with_inputs = True)

    for erl in src_artifacts:
        _build_erl(ctx, toolchain, build_environment, dep_info_file, erl, beam_mapping[module_name(erl)])

# updates deps_deps in place
def _get_deps_files(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        name: str,
        srcs: list[Artifact],
        build_environment: BuildEnvironment):
    """Mapping from the output path to the deps file artifact for each srcs artifact and dependencies."""

    if name not in build_environment.deps_files:
        build_environment.deps_files[name] = {}

    deps = build_environment.deps_files[name]
    for src in srcs:
        key = _deps_key(src)
        file = _get_deps_file(ctx, toolchain, src)
        for app in build_environment.deps_files:
            if key in build_environment.deps_files[app]:
                _fail_dep_conflict(key, file, build_environment.deps_files[app][key])
        deps[key] = file

def _deps_key(src: Artifact) -> str:
    if _is_erl(src):
        return module_name(src) + ".beam"
    else:
        return src.basename

def _get_deps_file(ctx: AnalysisContext, toolchain: Toolchain, src: Artifact) -> Artifact:
    dependency_json = ctx.actions.declare_output(_dep_file_name(toolchain, src))

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
        build_environment: BuildEnvironment,
        dep_info_file: WriteJsonCliArgs,
        src: Artifact,
        output: Artifact) -> None:
    """Compile erl files into beams."""

    final_dep_file = ctx.actions.declare_output(_dep_final_name(toolchain, src))
    _run_with_env(
        ctx,
        toolchain,
        cmd_args(toolchain.dependency_finalizer, src, dep_info_file, final_dep_file.as_output()),
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
        mapping_file = ctx.actions.write_json(_dep_mapping_name(toolchain, src), mapping)
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
        build_environment: BuildEnvironment) -> (cmd_args, dict[str, (bool, [str, Artifact])]):
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
            module, _ = paths.split_extension(file)

            # we made sure earlier there are no conflicts, we'll find at most one
            for app in build_environment.beams:
                if module in build_environment.beams[app]:
                    beams.add(build_environment.beams[app][module])
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

def _preserved_opts(opts: list[str]) -> cmd_args:
    """Options that should be preserved in the beam file despite +determinstic"""
    preservable = set(["+beam_debug_info", "+debug_info", "+inline", "+line_coverage"])
    preserved = [opt.lstrip("+") for opt in preservable.intersection(opts)]

    joined = cmd_args(preserved, delimiter = ", ")
    return cmd_args(joined, format = "{options, [{}]}")

def generated_erl_path(toolchain: Toolchain, appname: str, src: Artifact) -> str:
    """The output path for generated erl files."""
    return paths.join(
        _build_dir(toolchain),
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

def _dep_file_name(toolchain: Toolchain, src: Artifact) -> str:
    return paths.join(
        _build_dir(toolchain),
        "__dep_files",
        src.short_path + ".dep",
    )

def _dep_final_name(toolchain: Toolchain, src: Artifact) -> str:
    return paths.join(
        _build_dir(toolchain),
        "__dep_files",
        src.short_path + ".final.dep",
    )

def _dep_mapping_name(toolchain: Toolchain, src: Artifact) -> str:
    return paths.join(
        _build_dir(toolchain),
        "__dep_files",
        src.short_path + ".mapping",
    )

def _dep_info_name(toolchain: Toolchain) -> str:
    return paths.join(
        _build_dir(toolchain),
        "__dep_files",
        "app.info.dep",
    )

def _merge(a: dict, b: dict) -> dict:
    """ sefely merge two dict """

    # avoid copy, if not mutating
    if not a:
        return b
    if not b:
        return a

    r = dict(a)
    r.update(b)
    return r

def _build_dir(toolchain: Toolchain) -> str:
    return paths.join("__build", toolchain.name)

def _run_with_env(ctx: AnalysisContext, toolchain: Toolchain, args: cmd_args, **kwargs):
    """ run interfact that injects env"""

    # use os_env defined in target if present
    if getattr(ctx.attrs, "os_env", None) == None:
        env = toolchain.env
    else:
        env = ctx.attrs.os_env

    if "env" in kwargs:
        env = _merge(kwargs["env"], env)
    else:
        env = env
    kwargs["env"] = env
    ctx.actions.run(args, **kwargs)

# mutates build_environment in place
def _peek_private_includes(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: BuildEnvironment,
        dependencies: ErlAppDependencies,
        force_peek: bool = False):
    if not (force_peek or ctx.attrs.peek_private_includes):
        return

    # get private deps from dependencies
    for dep in dependencies.values():
        if ErlangAppInfo in dep:
            dep_info = dep[ErlangAppInfo]
            if not dep_info.virtual:
                build_environment.private_includes[dep_info.name] = dep_info.private_includes[toolchain.name]
                build_environment.private_include_dirs[dep_info.name] = dep_info.private_include_dir[toolchain.name]

# export

erlang_build = struct(
    prepare_build_environment = _prepare_build_environment,
    build_steps = struct(
        generated_source_artifacts = _generated_source_artifacts,
        generate_include_artifacts = _generate_include_artifacts,
        generate_beam_artifacts = _generate_beam_artifacts,
    ),
    utils = struct(
        is_hrl = _is_hrl,
        is_erl = _is_erl,
        is_yrl = _is_yrl,
        is_xrl = _is_xrl,
        is_config = _is_config,
        module_name = module_name,
        build_dir = _build_dir,
        run_with_env = _run_with_env,
        peek_private_includes = _peek_private_includes,
    ),
)
