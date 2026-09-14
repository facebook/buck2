# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load("@prelude//cxx:cxx_library.bzl", "cxx_compile_srcs")
load("@prelude//cxx:cxx_sources.bzl", "CxxSrcWithFlags")
load("@prelude//cxx:cxx_types.bzl", "CxxRuleConstructorParams")
load("@prelude//cxx:headers.bzl", "CxxHeadersLayout", "CxxHeadersNaming")
load("@prelude//linking:types.bzl", "Linkage")

GeneratedBuildInfo = record(
    json = Artifact,
    linker_flags = list[typing.Any],
    source = Artifact,
)

GeneratedBuildInfoData = record(
    json = Artifact,
)

_REQUIRED_GENERATED_BUILD_INFO_FIELDS = [
    "allow_cache_upload",
    "base_linker_flags",
    "enabled",
    "exported_symbols",
    "final_linker_flags",
    "finalize_build_info_at_link",
    "local_only",
]

def compile_generated_build_info(ctx: AnalysisContext, info: GeneratedBuildInfo) -> list[Artifact]:
    compiled = cxx_compile_srcs(
        actions = ctx.actions,
        target_label = ctx.label,
        cxx_toolchain_info = get_cxx_toolchain_info(ctx),
        impl_params = CxxRuleConstructorParams(
            rule_type = "generated_build_info",
            headers_layout = CxxHeadersLayout(
                namespace = "",
                naming = CxxHeadersNaming("regular"),
            ),
            srcs = [CxxSrcWithFlags(file = info.source)],
            _cxx_toolchain = ctx.attrs._cxx_toolchain,
        ),
        own_preprocessors = [],
        inherited_non_exported_preprocessor_infos = [],
        inherited_exported_preprocessor_infos = [],
        preferred_linkage = Linkage("shared"),
        add_coverage_instrumentation_compiler_flags = False,
    )
    return compiled.pic.objects

def _generated_build_info_config(ctx: AnalysisContext):
    spec = getattr(ctx.attrs, "_generated_build_info_spec", None)
    if not spec:
        return None

    missing_fields = [field for field in _REQUIRED_GENERATED_BUILD_INFO_FIELDS if field not in spec]
    if missing_fields:
        fail("generated build-info spec is missing required fields: {}".format(", ".join(missing_fields)))
    if not spec["enabled"]:
        return None

    tool = getattr(ctx.attrs, "_gen_build_info", None)
    if tool == None:
        fail("_gen_build_info must be set when generated build info is enabled")
    return (spec, tool[RunInfo])

def _generate_build_info_data(
    ctx: AnalysisContext,
    spec,
    tool: RunInfo,
    invalidation_inputs: list[typing.Any] = [],
) -> GeneratedBuildInfoData:
    output_dir = "__generated_build_info__"
    json = getattr(ctx.attrs, "_generated_build_info_data", None)
    # A caller that supplies the JSON artifact owns its invalidation edges;
    # `invalidation_inputs` apply only when this rule generates the JSON.
    if json == None:
        generator_spec = ctx.actions.write_json(
            output_dir + "/generator_spec.json",
            spec,
        )
        json = ctx.actions.declare_output(output_dir, "build_info.json")
        command = cmd_args(
            tool,
            "--spec-json",
            generator_spec,
            "--output-json",
            json.as_output(),
        )
        command.add(cmd_args(hidden = invalidation_inputs))
        ctx.actions.run(
            command,
            category = "generate_build_info_json",
            local_only = spec["local_only"],
            allow_cache_upload = spec["allow_cache_upload"],
        )
    return GeneratedBuildInfoData(json = json)

def generate_build_info_data(
    ctx: AnalysisContext,
    invalidation_inputs: list[typing.Any] = [],
) -> GeneratedBuildInfoData | None:
    config = _generated_build_info_config(ctx)
    if config == None:
        return None
    spec, tool = config
    return _generate_build_info_data(ctx, spec, tool, invalidation_inputs)

# Expected `_generated_build_info_spec` shape:
# {
#     "<generator-owned-field>": <JSON-compatible value>,
# }
# Generator-owned fields remain at the top level so configured selectors are
# resolved before the specification is serialized.
def generate_build_info(ctx: AnalysisContext, invalidation_inputs: list[typing.Any] = []) -> GeneratedBuildInfo | None:
    config = _generated_build_info_config(ctx)
    if config == None:
        return None
    spec, tool = config
    data = _generate_build_info_data(ctx, spec, tool, invalidation_inputs)

    output_dir = "__generated_build_info__"
    source = ctx.actions.declare_output(output_dir, "build_info.c")

    ctx.actions.run(
        cmd_args(
            tool,
            "--input-json",
            data.json,
            "--output-source",
            source.as_output(),
        ),
        category = "generate_build_info_source",
    )

    json_linker_flags = (
        (spec.get("final_linker_flags", []) + [cmd_args("--build-info-json=", data.json, delimiter = "")])
        if spec.get("finalize_build_info_at_link", True)
        else []
    )
    return GeneratedBuildInfo(
        json = data.json,
        linker_flags = json_linker_flags + spec["base_linker_flags"],
        source = source,
    )
