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
load(":erlang_utils.bzl", "action_identifier", "to_term_args")

# mapping
#   from include base name and application (e.g. ("app1", "header.hrl")
#   to symlinked include/ dir artifact
IncludesMapping = dict[(str, str), Artifact]

# mapping
#   from include base name (e.g. "header.hrl"
#   to artifact
PathArtifactMapping = dict[str, Artifact]

# mapping
#   from module name
#   to artifact
ModuleArtifactMapping = dict[str, Artifact]

# mapping
#   from input base name
#   path to input artifact from repo root
InputArtifactMapping = dict[str, Artifact]

BuildEnvironment = record(
    includes = field(IncludesMapping, {}),
    private_includes = field(PathArtifactMapping, {}),
    beams = field(ModuleArtifactMapping, {}),
    priv_dirs = field(PathArtifactMapping, {}),
    include_dirs = field(PathArtifactMapping, {}),
    private_include_dir = field(list[Artifact], []),
    ebin_dirs = field(PathArtifactMapping, {}),
    deps_files = field(PathArtifactMapping, {}),
    app_files = field(PathArtifactMapping, {}),
    full_dependencies = field(list[Artifact], []),
    # convenience storrage
    app_includes = field(IncludesMapping, {}),
    app_beams = field(ModuleArtifactMapping, {}),
    app_chunks = field(ModuleArtifactMapping, {}),
    # input artifact mapping
    input_mapping = field(InputArtifactMapping, {}),
)

def _prepare_build_environment(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        dependencies: ErlAppDependencies) -> BuildEnvironment:
    """Prepare build environment and collect the context from all dependencies."""
    priv_dirs = {}
    include_dirs = {}
    ebin_dirs = {}
    deps_files = {}
    includes = {}
    beams = {}
    app_files = {}
    full_dependencies = []
    input_mapping = {}

    # for duplication detection
    apps = set()

    for name, dep in dependencies.items():
        if name in apps:
            fail("duplicated application name found %s" % (name,))
        apps.add(name)

        if ErlangAppInfo in dep:
            dep_info = dep[ErlangAppInfo]

            if dep_info.virtual:
                # virtual applications don't directories we need to include
                # we skip this entire step
                continue

            # collect beams
            intersection = [key for key in beams if key in dep_info.beams[toolchain.name]]
            if len(intersection):
                fail("duplicated modules found in build: {}".format(repr(intersection)))

            beams.update(dep_info.beams[toolchain.name])

            # collect dirs
            priv_dirs[name] = dep_info.priv_dir[toolchain.name]
            ebin_dirs[name] = dep_info.ebin_dir[toolchain.name]

            # collect app files
            app_files[name] = dep_info.app_file[toolchain.name]

        elif ErlangAppIncludeInfo in dep:
            dep_info = dep[ErlangAppIncludeInfo]

            if dep_info.name == ctx.attrs.name:
                continue
        elif ErlangTestInfo in dep:
            # we only care about application deps
            continue
        else:
            fail("invalid dep {}", dep)

        # add transitive input mapping
        # Note: the build will fail if there is ambiguity in the basename
        input_mapping.update(dep_info.input_mapping[toolchain.name])

        # collect includes
        include_dirs[name] = dep_info.include_dir[toolchain.name]
        includes.update(dep_info.includes[toolchain.name])

        # collect deps_files
        deps_files.update(dep_info.deps_files[toolchain.name])

    return BuildEnvironment(
        includes = includes,
        beams = beams,
        priv_dirs = priv_dirs,
        include_dirs = include_dirs,
        ebin_dirs = ebin_dirs,
        deps_files = deps_files,
        app_files = app_files,
        full_dependencies = full_dependencies,
        input_mapping = input_mapping,
    )

def _generate_input_mapping(build_environment: BuildEnvironment, input_artifacts: list[Artifact]) -> BuildEnvironment:
    # collect input artifacts for current targets
    # Note: this must be after the dependencies to overwrite private includes
    input_mapping = dict(build_environment.input_mapping)

    for input_artifact in input_artifacts:
        key = input_artifact.basename
        if key in input_mapping and input_mapping[key] != input_artifact:
            fail("conflicting inputs for {}: {} {}".format(key, input_mapping[key], input_artifact))
        input_mapping[key] = input_artifact

    return BuildEnvironment(
        # updated field
        input_mapping = input_mapping,
        # copied fields
        includes = build_environment.includes,
        private_includes = build_environment.private_includes,
        beams = build_environment.beams,
        priv_dirs = build_environment.priv_dirs,
        include_dirs = build_environment.include_dirs,
        private_include_dir = build_environment.private_include_dir,
        ebin_dirs = build_environment.ebin_dirs,
        deps_files = build_environment.deps_files,
        app_files = build_environment.app_files,
        full_dependencies = build_environment.full_dependencies,
        app_includes = build_environment.app_includes,
        app_beams = build_environment.app_beams,
        app_chunks = build_environment.app_chunks,
    )

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

def _generate_include_artifacts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: BuildEnvironment,
        name: str,
        header_artifacts: list[Artifact],
        is_private: bool = False) -> BuildEnvironment:
    # anchor for include dir
    anchor = _make_dir_anchor(ctx, paths.join(_build_dir(toolchain), name, "include"))

    # output artifacts
    include_mapping = {
        _header_key(hrl, name, is_private): ctx.actions.declare_output(anchor_path(anchor, hrl.basename))
        for hrl in header_artifacts
    }

    # dep files
    include_deps = _get_deps_files(ctx, toolchain, anchor, header_artifacts)

    # generate actions
    for hrl in header_artifacts:
        _build_hrl(ctx, hrl, include_mapping[_header_key(hrl, name, is_private)])

    # construct updates build environment
    if not is_private:
        # fields for public include directory
        includes = _merge(include_mapping, build_environment.includes)
        private_includes = build_environment.private_includes
        include_dirs = _add(build_environment.include_dirs, name, anchor)
        private_include_dir = build_environment.private_include_dir
        app_includes = include_mapping
    else:
        # fields for private include directory
        includes = build_environment.includes
        private_includes = _merge(include_mapping, build_environment.private_includes)
        include_dirs = build_environment.include_dirs
        private_include_dir = [anchor] + build_environment.private_include_dir
        app_includes = build_environment.app_includes

    return BuildEnvironment(
        # updated fields
        includes = includes,
        private_includes = private_includes,
        include_dirs = include_dirs,
        private_include_dir = private_include_dir,
        deps_files = _merge(include_deps, build_environment.deps_files),
        app_includes = app_includes,
        # copied fields
        beams = build_environment.beams,
        priv_dirs = build_environment.priv_dirs,
        ebin_dirs = build_environment.ebin_dirs,
        app_beams = build_environment.app_beams,
        app_files = build_environment.app_files,
        full_dependencies = build_environment.full_dependencies,
        input_mapping = build_environment.input_mapping,
    )

def _header_key(hrl: Artifact, name: str, is_private: bool) -> [(str, str), str]:
    """Return the key for either public `("string", "string")` or private `"string"` include """
    return hrl.basename if is_private else (name, hrl.basename)

def _generate_beam_artifacts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: BuildEnvironment,
        name: str,
        src_artifacts: list[Artifact],
        output_mapping: [None, dict[Artifact, str]] = None) -> BuildEnvironment:
    # anchor for ebin dir
    anchor = _make_dir_anchor(ctx, paths.join(_build_dir(toolchain), name, "ebin"))

    # output artifacts
    def output_path(src: Artifact) -> str:
        if output_mapping:
            return output_mapping[src]
        else:
            return beam_path(anchor, src.basename)

    beam_mapping = {
        module_name(src): ctx.actions.declare_output(output_path(src))
        for src in src_artifacts
    }

    # dep files
    beam_deps = _get_deps_files(ctx, toolchain, anchor, src_artifacts, output_mapping)

    updated_build_environment = BuildEnvironment(
        # updated fields
        beams = _merge(beam_mapping, build_environment.beams),
        ebin_dirs = _add(build_environment.ebin_dirs, name, anchor),
        deps_files = _merge(beam_deps, build_environment.deps_files),
        app_beams = beam_mapping,
        # copied fields
        includes = build_environment.includes,
        private_includes = build_environment.private_includes,
        priv_dirs = build_environment.priv_dirs,
        include_dirs = build_environment.include_dirs,
        private_include_dir = build_environment.private_include_dir,
        app_includes = build_environment.app_includes,
        app_files = build_environment.app_files,
        full_dependencies = build_environment.full_dependencies,
        input_mapping = build_environment.input_mapping,
    )

    dep_info_content = _build_dep_info_data(updated_build_environment)
    dep_info_file = ctx.actions.write_json(_dep_info_name(toolchain), dep_info_content)

    for erl in src_artifacts:
        _build_erl(ctx, toolchain, updated_build_environment, dep_info_file, erl, beam_mapping[module_name(erl)])

    return updated_build_environment

def _build_dep_info_data(build_environment: BuildEnvironment) -> dict[str, dict[str, Artifact | str]]:
    """build input for dependency finalizer, this implements uniqueness checks for headers and beams"""
    seen = {}
    data = {}
    for artifact, dep_file in build_environment.deps_files.items():
        if paths.basename(artifact) in seen:
            fail("conflicting artifacts found in build: {} and {}".format(seen[paths.basename(artifact)], artifact))
        else:
            seen[paths.basename(artifact)] = artifact
            data[paths.basename(artifact)] = {"dep_file": dep_file, "path": artifact}
    return data

def _generate_chunk_artifacts(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: BuildEnvironment,
        name: str,
        src_artifacts: list[Artifact]) -> BuildEnvironment:
    anchor = _make_dir_anchor(ctx, paths.join(_build_dir(toolchain), name, "chunks"))

    chunk_mapping = {
        module_name(src): ctx.actions.declare_output(chunk_path(anchor, src.basename))
        for src in src_artifacts
    }

    updated_build_environment = BuildEnvironment(
        app_chunks = chunk_mapping,
        # copied fields
        includes = build_environment.includes,
        private_includes = build_environment.private_includes,
        beams = build_environment.beams,
        priv_dirs = build_environment.priv_dirs,
        include_dirs = build_environment.include_dirs,
        private_include_dir = build_environment.private_include_dir,
        ebin_dirs = build_environment.ebin_dirs,
        deps_files = build_environment.deps_files,
        app_files = build_environment.app_files,
        full_dependencies = build_environment.full_dependencies,
        app_includes = build_environment.app_includes,
        app_beams = build_environment.app_beams,
        input_mapping = build_environment.input_mapping,
    )

    preprocess_modules = toolchain.edoc_preprocess
    preprocess_all = "__all__" in preprocess_modules

    for erl in src_artifacts:
        preprocess = preprocess_all or module_name(erl) in preprocess_modules
        _build_edoc(ctx, toolchain, updated_build_environment, erl, chunk_mapping[module_name(erl)], preprocess)

    return updated_build_environment

def _make_dir_anchor(ctx: AnalysisContext, path: str) -> Artifact:
    return ctx.actions.write(
        paths.normalize(paths.join(path, ".hidden")),
        cmd_args([""]),
    )

def _get_deps_files(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        anchor: Artifact,
        srcs: list[Artifact],
        output_mapping: [None, dict[Artifact, str]] = None) -> dict[str, Artifact]:
    """Mapping from the output path to the deps file artifact for each srcs artifact."""

    def output_path(src: Artifact) -> str:
        return output_mapping[src] if output_mapping else _deps_key(anchor, src)

    return {
        output_path(src): _get_deps_file(ctx, toolchain, src)
        for src in srcs
    }

def _deps_key(anchor: Artifact, src: Artifact) -> str:
    name, ext = paths.split_extension(src.basename)
    if ext == ".erl":
        ext = ".beam"
    return anchor_path(anchor, name + ext)

def _get_deps_file(ctx: AnalysisContext, toolchain: Toolchain, src: Artifact) -> Artifact:
    dependency_json = ctx.actions.declare_output(_dep_file_name(toolchain, src))

    dependency_analyzer_args = cmd_args(
        [
            src,
            dependency_json.as_output(),
        ],
    )
    _run_escript(
        ctx,
        toolchain,
        toolchain.dependency_analyzer,
        dependency_analyzer_args,
        category = "dependency_analyzer",
        identifier = action_identifier(toolchain, src.short_path),
    )
    return dependency_json

def _build_hrl(
        ctx: AnalysisContext,
        hrl: Artifact,
        output: Artifact) -> None:
    """Copy the header file and add dependencies on other includes."""
    ctx.actions.copy_file(output.as_output(), hrl)
    return None

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
        dep_info_file: Artifact,
        src: Artifact,
        output: Artifact) -> None:
    """Compile erl files into beams."""

    trampoline = toolchain.erlc_trampoline
    erlc = toolchain.otp_binaries.erlc

    final_dep_file = ctx.actions.declare_output(_dep_final_name(toolchain, src))
    finalize_deps_cmd = cmd_args(
        src,
        dep_info_file,
        final_dep_file.as_output(),
        hidden = build_environment.deps_files.values(),
    )
    _run_escript(
        ctx,
        toolchain,
        toolchain.dependency_finalizer,
        finalize_deps_cmd,
        category = "dependency_finalizer",
        identifier = action_identifier(toolchain, src.basename),
    )

    def dynamic_lambda(ctx: AnalysisContext, artifacts, outputs):
        erl_opts = _get_erl_opts(ctx, toolchain, src)
        erlc_cmd = cmd_args(
            [
                trampoline,
                erlc,
                erl_opts,
                _erlc_dependency_args(
                    _dependency_include_dirs(build_environment),
                    _dependency_code_paths(build_environment),
                ),
                "-o",
                cmd_args(outputs[output].as_output(), parent = 1),
                src,
            ],
        )
        deps_args, mapping = _dependencies_to_args(artifacts, final_dep_file, build_environment)
        erlc_cmd.add(deps_args)
        full_deps_args = _full_dependencies(build_environment)
        erlc_cmd.add(full_deps_args)
        _run_with_env(
            ctx,
            toolchain,
            erlc_cmd,
            category = "erlc",
            identifier = action_identifier(toolchain, src.basename),
            env = {"BUCK2_FILE_MAPPING": _generate_file_mapping_string(mapping)},
            always_print_stderr = True,
        )

    ctx.actions.dynamic_output(dynamic = [final_dep_file], inputs = [src], outputs = [output.as_output()], f = dynamic_lambda)
    return None

def _build_edoc(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: BuildEnvironment,
        src: Artifact,
        output: Artifact,
        preprocess: bool) -> None:
    """Build edoc from erl files."""
    eval_cmd = cmd_args(
        [
            toolchain.otp_binaries.escript,
            toolchain.edoc,
            cmd_args(toolchain.edoc_options),
            "-app",
            ctx.attrs.name,
            "-files",
            src,
            "-chunks",
            "-pa",
            toolchain.utility_modules,
            "-o",
            cmd_args(output.as_output(), parent = 2),
        ],
    )

    if not preprocess:
        eval_cmd.add("-no-preprocess")

    args = _erlc_dependency_args(_dependency_include_dirs(build_environment), [], False)
    eval_cmd.add(args)

    eval_cmd_hidden = []
    for include in build_environment.includes.values():
        eval_cmd_hidden.append(include)

    for include in build_environment.private_includes.values():
        eval_cmd_hidden.append(include)

    eval_cmd.add(cmd_args(hidden = eval_cmd_hidden))

    _run_with_env(
        ctx,
        toolchain,
        eval_cmd,
        always_print_stderr = True,
        category = "edoc",
        identifier = action_identifier(toolchain, src.basename),
    )
    return None

def _dependencies_to_args(
        artifacts,
        final_dep_file: Artifact,
        build_environment: BuildEnvironment) -> (cmd_args, dict[str, (bool, [str, Artifact])]):
    """Add the transitive closure of all per-file Erlang dependencies as specified in the deps files to the `args` with .hidden.
    """
    args_hidden = []

    input_mapping = {}
    deps = artifacts[final_dep_file].read_json()

    # silently ignore not found dependencies and let erlc report the not found stuff
    for dep in deps:
        artifact = None
        file = dep["file"]
        if dep["type"] == "include_lib":
            app = dep["app"]
            if (app, file) in build_environment.includes:
                artifact = build_environment.includes[(app, file)]
                input_mapping[file] = (True, build_environment.input_mapping[artifact.basename])
            else:
                # the file might come from OTP
                input_mapping[file] = (False, paths.join(app, "include", file))
                continue

        elif dep["type"] == "include":
            # these includes can either reside in the private includes
            # or the public ones
            if file in build_environment.private_includes:
                artifact = build_environment.private_includes[file]

                if artifact.basename in build_environment.input_mapping:
                    input_mapping[file] = (True, build_environment.input_mapping[artifact.basename])
            else:
                # at this point we don't know the application the include is coming
                # from, and have to check all public include directories
                candidates = [key for key in build_environment.includes.keys() if key[1] == file]
                if len(candidates) > 1:
                    offending_apps = [app for (app, _) in candidates]
                    fail("-include(\"%s\") is ambiguous as the following applications declare public includes with the same name: %s" % (file, offending_apps))
                elif candidates:
                    artifact = build_environment.includes[candidates[0]]
                    input_mapping[file] = (True, build_environment.input_mapping[artifact.basename])
                else:
                    # we didn't find the include, build will fail during compile
                    continue

        elif (dep["type"] == "behaviour" or
              dep["type"] == "parse_transform" or
              dep["type"] == "manual_dependency"):
            module, _ = paths.split_extension(file)
            if module in build_environment.beams:
                artifact = build_environment.beams[module]
            else:
                continue

        else:
            fail("unrecognized dependency type %s", (dep["type"]))

        args_hidden.append(artifact)

    return cmd_args(hidden = args_hidden), input_mapping

def _full_dependencies(build_environment: BuildEnvironment) -> cmd_args:
    erlc_cmd_hidden = []
    for artifact in build_environment.full_dependencies:
        erlc_cmd_hidden.append(artifact)
    return cmd_args(hidden = erlc_cmd_hidden)

def _dependency_include_dirs(build_environment: BuildEnvironment) -> list[cmd_args]:
    includes = [
        cmd_args(include_dir_anchor, parent = 1)
        for include_dir_anchor in build_environment.private_include_dir
    ]

    for include_dir_anchor in build_environment.include_dirs.values():
        includes.append(cmd_args(include_dir_anchor, parent = 3))
        includes.append(cmd_args(include_dir_anchor, parent = 1))

    return includes

def _dependency_code_paths(build_environment: BuildEnvironment) -> list[cmd_args]:
    return [
        cmd_args(ebin_dir_anchor, parent = 1)
        for ebin_dir_anchor in build_environment.ebin_dirs.values()
    ]

def _erlc_dependency_args(
        includes: list[cmd_args],
        code_paths: list[cmd_args],
        path_in_arg: bool = True) -> cmd_args:
    """Build include and path options."""
    # Q: why not just change format here - why do we add -I/-pa as a separate argument?
    # A: the whole string would get passed as a single argument, as if it was quoted in CLI e.g. '-I include_path'
    # ...which the escript cannot parse, as it expects two separate arguments, e.g. '-I' 'include_path'

    args = cmd_args([], ignore_artifacts = True)

    # build -I options
    if path_in_arg:
        for include in includes:
            args.add(cmd_args(include, format = "-I{}"))
    else:
        for include in includes:
            args.add("-I")
            args.add(include)

    # build -pa options
    if path_in_arg:
        for code_path in code_paths:
            args.add(cmd_args(code_path, format = "-pa{}"))
    else:
        for code_path in code_paths:
            args.add("-pa")
            args.add(code_path)

    return args

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

    for parse_transform, (beam, resource_folder) in parse_transforms.items():
        args.add(
            "+{parse_transform, %s}" % (parse_transform,),
            cmd_args(beam, format = "-pa{}", parent = 1),
        )
        args.add(cmd_args(hidden = resource_folder))

    source = cmd_args(src, format = "{source, \"{}\"}")
    path_type = "{path_type, relative}"
    preserved_opts = _preserved_opts(opts)

    compile_info = cmd_args([source, path_type, preserved_opts], delimiter = ", ")

    # add relevant compile_info manually
    args.add(cmd_args(compile_info, format = "+{compile_info, [{}]}"))

    return args

def _preserved_opts(opts: list[str]) -> cmd_args:
    """Options that should be preserved in the beam file despite +determinstic"""
    preservable = set(["+inline", "+line_coverage"])
    preserved = [opt.lstrip("+") for opt in preservable.intersection(opts)]

    joined = cmd_args(preserved, delimiter = ", ")
    return cmd_args(joined, format = "{options, [{}]}")

def private_include_name(toolchain: Toolchain, appname: str) -> str:
    """The temporary appname private header files."""
    return paths.join(
        _build_dir(toolchain),
        "__private_includes_%s" % (appname,),
    )

def generated_erl_path(toolchain: Toolchain, appname: str, src: Artifact) -> str:
    """The output path for generated erl files."""
    return paths.join(
        _build_dir(toolchain),
        "__generated_%s" % (appname,),
        "%s.erl" % (module_name(src),),
    )

def anchor_path(anchor: Artifact, basename: str) -> str:
    """ Returns the output path for hrl files. """
    return paths.join(paths.dirname(anchor.short_path), basename)

def beam_path(anchor: Artifact, basename: str) -> str:
    """ Returns the output path for beam files. """
    return anchor_path(anchor, paths.replace_extension(basename, ".beam"))

def chunk_path(anchor: Artifact, basename: str) -> str:
    """Returns the output path for chunk files."""
    return anchor_path(anchor, paths.replace_extension(basename, ".chunk"))

def module_name(in_file: Artifact) -> str:
    """ Returns the basename of the artifact without extension """
    name, _ = paths.split_extension(in_file.basename)
    return name

def _is_hrl(in_file: Artifact) -> bool:
    """ Returns True if the artifact is a hrl file """
    return _is_ext(in_file, [".hrl"])

def _is_erl(in_file: Artifact) -> bool:
    """ Returns True if the artifact is an erl file """
    return _is_ext(in_file, [".erl"])

def _is_yrl(in_file: Artifact) -> bool:
    """ Returns True if the artifact is a yrl file """
    return _is_ext(in_file, [".yrl"])

def _is_xrl(in_file: Artifact) -> bool:
    """ Returns True if the artifact is a xrl file """
    return _is_ext(in_file, [".xrl"])

def _is_ext(in_file: Artifact, extensions: list[str]) -> bool:
    """ Returns True if the artifact has an extension listed in extensions """
    _, ext = paths.split_extension(in_file.basename)
    return ext in extensions

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

def _dep_info_name(toolchain: Toolchain) -> str:
    return paths.join(
        _build_dir(toolchain),
        "__dep_files",
        "app.info.dep",
    )

def _merge(a: dict, b: dict) -> dict:
    """ sefely merge two dict """
    r = dict(a)
    r.update(b)
    return r

def _add(a: dict, key: typing.Any, value: typing.Any) -> dict:
    """ safely add a value to a dict """
    b = dict(a)
    b[key] = value
    return b

def _build_dir(toolchain: Toolchain) -> str:
    return paths.join("__build", toolchain.name)

def _generate_file_mapping_string(mapping: dict[str, (bool, [str, Artifact])]) -> cmd_args:
    """produces an easily parsable string for the file mapping"""
    items = {}
    for file, (if_found, artifact) in mapping.items():
        items[file] = (if_found, artifact)

    return to_term_args(items)

def _run_with_env(ctx: AnalysisContext, toolchain: Toolchain, *args, **kwargs):
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
    ctx.actions.run(*args, **kwargs)

def _run_escript(ctx: AnalysisContext, toolchain: Toolchain, script: Artifact, args: cmd_args, **kwargs) -> None:
    """ run escript with env and providing toolchain-configured utility modules"""
    cmd = cmd_args([
        toolchain.otp_binaries.erl,
        "+A0",
        "+S1:1",
        "+sbtu",
        "+MMscs",
        "8",
        "+MMsco",
        "false",
        "-env",
        "MALLOC_ARENA_MAX",
        "2",
        "-mode",
        "minimal",
        "-noinput",
        "-noshell",
        "-pa",
        toolchain.utility_modules,
        "-run",
        "escript",
        "start",
        "--",
        script,
        args,
    ])
    _run_with_env(ctx, toolchain, cmd, **kwargs)

def _peek_private_includes(
        ctx: AnalysisContext,
        toolchain: Toolchain,
        build_environment: BuildEnvironment,
        dependencies: ErlAppDependencies,
        force_peek: bool = False) -> BuildEnvironment:
    # get mutable dict for private includes
    new_private_includes = dict(build_environment.private_includes)
    new_private_include_dir = list(build_environment.private_include_dir)

    # get private deps from dependencies
    for dep in dependencies.values():
        if ErlangAppInfo in dep:
            if dep[ErlangAppInfo].private_include_dir:
                new_private_include_dir = new_private_include_dir + dep[ErlangAppInfo].private_include_dir[toolchain.name]
                new_private_includes.update(dep[ErlangAppInfo].private_includes[toolchain.name])
    if force_peek or ctx.attrs.peek_private_includes:
        return BuildEnvironment(
            private_includes = new_private_includes,
            private_include_dir = new_private_include_dir,
            # copied fields
            includes = build_environment.includes,
            beams = build_environment.beams,
            priv_dirs = build_environment.priv_dirs,
            include_dirs = build_environment.include_dirs,
            ebin_dirs = build_environment.ebin_dirs,
            deps_files = build_environment.deps_files,
            app_files = build_environment.app_files,
            full_dependencies = build_environment.full_dependencies,
            app_includes = build_environment.app_includes,
            app_beams = build_environment.app_beams,
            app_chunks = build_environment.app_chunks,
            input_mapping = build_environment.input_mapping,
        )
    else:
        return build_environment

# export

erlang_build = struct(
    prepare_build_environment = _prepare_build_environment,
    build_steps = struct(
        generate_input_mapping = _generate_input_mapping,
        generated_source_artifacts = _generated_source_artifacts,
        generate_include_artifacts = _generate_include_artifacts,
        generate_beam_artifacts = _generate_beam_artifacts,
        generate_chunk_artifacts = _generate_chunk_artifacts,
    ),
    utils = struct(
        is_hrl = _is_hrl,
        is_erl = _is_erl,
        is_yrl = _is_yrl,
        is_xrl = _is_xrl,
        module_name = module_name,
        private_include_name = private_include_name,
        make_dir_anchor = _make_dir_anchor,
        build_dir = _build_dir,
        run_with_env = _run_with_env,
        run_escript = _run_escript,
        peek_private_includes = _peek_private_includes,
    ),
)
