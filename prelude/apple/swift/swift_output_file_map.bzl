# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def add_dependencies_output(ctx: AnalysisContext, output_file_map: dict, cmd: cmd_args, category: str, inputs_tag: ArtifactTag) -> None:
    dep_file_path = "__depfiles__/{}-{}.d".format(ctx.attrs.name, category)
    dep_file = ctx.actions.declare_output(dep_file_path).as_output()
    tagged_dep_file = inputs_tag.tag_artifacts(dep_file)
    map = output_file_map.setdefault("", {})
    map["dependencies"] = cmd_args(tagged_dep_file, delimiter = "")
    map["emit-module-dependencies"] = cmd_args(tagged_dep_file, delimiter = "")
    cmd.add(cmd_args("-emit-dependencies", hidden = [tagged_dep_file]))

def add_serialized_diagnostics_output(output_file_map: dict, cmd: cmd_args, diagnostics_output: OutputArtifact) -> None:
    map = output_file_map.setdefault("", {})
    map["diagnostics"] = cmd_args(diagnostics_output, delimiter = "", format = "{}.dia")
    cmd.add(cmd_args("-serialize-diagnostics", hidden = [diagnostics_output]))

def add_output_file_map_flags(ctx: AnalysisContext, output_file_map: dict, cmd: cmd_args, category: str) -> Artifact:
    output_file_map_path = "{}_swift_{}_output_file_map.json".format(ctx.attrs.name, category)
    output_file_map_json = ctx.actions.write_json(
        output_file_map_path,
        output_file_map,
        pretty = True,
    )
    cmd.add("-output-file-map", output_file_map_json)
    return output_file_map_json
