# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",  # @unused Used as a type
)

_AnalysisInput = record(
    argsfile = field(Artifact),
    identifier = field(int),
)

def get_debug_artifacts_validators(ctx, artifacts: ArtifactTSet) -> dict[str, Artifact]:
    label_to_input_artifacts = _get_analysis_input_artifacts(ctx, artifacts)
    if not label_to_input_artifacts:
        return {}

    name_to_validation_result = {}
    for key, validator in ctx.attrs.debug_artifacts_validators.items():
        analysis, reducer = validator
        label_to_analysis_artifacts = _analyze_artifacts(ctx, key, analysis[RunInfo], label_to_input_artifacts)
        name_to_validation_result[key] = _reduce_analysis_artifacts(ctx, key, reducer[RunInfo], label_to_analysis_artifacts)

    return name_to_validation_result

def _get_analysis_input_artifacts(ctx, artifacts: ArtifactTSet) -> dict[Label, list[_AnalysisInput]]:
    underlying_tset = artifacts._tset
    if underlying_tset == None:
        return {}

    results = {}
    identifier = 0
    for infos in underlying_tset.traverse():
        for info in infos:
            argsfile = ctx.actions.write(
                "artifacts-{}.txt".format(identifier),
                info.artifacts,
                with_inputs = True,
            )
            results.setdefault(info.label, []).append(
                _AnalysisInput(argsfile = argsfile, identifier = identifier),
            )
            identifier += 1
    return results

def _analyze_artifacts(
        ctx,
        key: str,
        analysis_tool: RunInfo,
        label_to_artifacts: dict[Label, list[_AnalysisInput]]) -> dict[Label, list[Artifact]]:
    label_to_analysis = {}
    for label, inputs in label_to_artifacts.items():
        for input in inputs:
            output = ctx.actions.declare_output("{}_{}.json".format(key, input.identifier))
            ctx.actions.run(
                cmd_args([
                    analysis_tool,
                    "--artifacts",
                    cmd_args(input.argsfile, format = "@{}"),
                    "--output",
                    output.as_output(),
                ]),
                category = "{}_analysis".format(key),
                identifier = "{}_{}".format(ctx.attrs.name, input.identifier),
            )
            label_to_analysis.setdefault(label, []).append(output)

    return label_to_analysis

def _reduce_analysis_artifacts(
        ctx,
        key: str,
        reducer_tool: RunInfo,
        label_to_artifacts: dict[Label, list[Artifact]]) -> Artifact:
    input_json = ctx.actions.write_json(
        "{}_reducer_args.json".format(key),
        label_to_artifacts,
        with_inputs = True,
    )

    output = ctx.actions.declare_output("{}.json".format(key))
    ctx.actions.run(
        cmd_args([
            reducer_tool,
            "--analysis-json-path",
            input_json,
            "--output",
            output.as_output(),
        ]),
        category = "{}_reduce".format(key),
        identifier = ctx.attrs.name,
    )
    return output
