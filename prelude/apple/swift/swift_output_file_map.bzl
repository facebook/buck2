# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//cxx:cxx_sources.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load(
    ":swift_incremental_support.bzl",
    "get_uses_experimental_content_based_path_hashing",
)

def add_dependencies_output(ctx: AnalysisContext, output_file_map: dict, cmd: cmd_args, category: str, inputs_tag: ArtifactTag) -> None:
    # Add a Makefile style dependency file output. This output is not tracked,
    # we need to process it first.
    uses_experimental_content_based_path_hashing = get_uses_experimental_content_based_path_hashing(ctx)
    buck_dep_file = ctx.actions.declare_output("__depfiles__/{}-{}.d".format(ctx.attrs.name, category), uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing).as_output()
    map = output_file_map.setdefault("", {})
    map["dependencies"] = cmd_args(buck_dep_file, delimiter = "", format = "{}.raw")
    map["emit-module-dependencies"] = cmd_args(buck_dep_file, delimiter = "", format = "{}.raw")
    cmd.add("-emit-dependencies")

    # Add the flags for the wrapper to process the dependency file to Buck format.
    cmd.add(
        "-Xwrapper",
        cmd_args(inputs_tag.tag_artifacts(buck_dep_file), format = "-dependencies-file-output={}"),
    )

def add_serialized_diagnostics_output(output_file_map: dict | None, cmd: cmd_args, diagnostics_output: OutputArtifact) -> None:
    if output_file_map == None:
        # Some actions, eg -emit-pcm, do not support output file maps. In this
        # case we need to pass the frontend flags directly.
        cmd.add(
            "-Xfrontend",
            cmd_args(diagnostics_output, format = "-serialize-diagnostics-path={}.dia"),
        )
    else:
        map = output_file_map.setdefault("", {})
        map["diagnostics"] = cmd_args(diagnostics_output, delimiter = "", format = "{}.dia")
        cmd.add(cmd_args("-serialize-diagnostics", hidden = [diagnostics_output]))

def add_output_file_map_flags(ctx: AnalysisContext, output_file_map: dict, cmd: cmd_args, category: str) -> Artifact:
    uses_experimental_content_based_path_hashing = get_uses_experimental_content_based_path_hashing(ctx)
    output_file_map_json = ctx.actions.write_json(
        "{}_output_file_map.json".format(category),
        output_file_map,
        pretty = True,
        uses_experimental_content_based_path_hashing = uses_experimental_content_based_path_hashing,
    )
    cmd.add("-output-file-map", output_file_map_json)
    return output_file_map_json

def get_modularization_dependency_graph_output_map(
        ctx: AnalysisContext,
        srcs: list[CxxSrcWithFlags]):
    output_file_map = {}
    output_modularization_dependency_graph_shards = []
    output_objects = []
    for src in srcs:
        file_name = src.file.basename
        object_file_artifact = ctx.actions.declare_output("__swift_modularization___/objects/" + file_name + ".o")
        modularization_dependency_graph_artifact = ctx.actions.declare_output("__swift_modularization___/dependency_graphs/" + file_name + ".modularizationdependencygraph")
        output_file_map[src.file] = {
            "modularization-dependency-graph": modularization_dependency_graph_artifact,
            "object": object_file_artifact,
        }
        output_objects.append(object_file_artifact)
        output_modularization_dependency_graph_shards.append(modularization_dependency_graph_artifact)

    return output_file_map, output_objects, output_modularization_dependency_graph_shards
