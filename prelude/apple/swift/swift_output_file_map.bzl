# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def add_dependencies_output(ctx: AnalysisContext, output_file_map: dict, cmd: cmd_args, category: str, inputs_tag: ArtifactTag) -> None:
    # Add a Makefile style dependency file output.
    dep_file = ctx.actions.declare_output("__depfiles__/{}-{}.d".format(ctx.attrs.name, category)).as_output()
    map = output_file_map.setdefault("", {})
    map["dependencies"] = cmd_args(dep_file, delimiter = "")
    map["emit-module-dependencies"] = cmd_args(dep_file, delimiter = "")
    cmd.add(cmd_args("-emit-dependencies", hidden = [dep_file]))

    # Add the flags for the wrapper to process the dependency file to Buck format.
    buck_dep_file = ctx.actions.declare_output("__depfiles__/{}-{}.d.buck".format(ctx.attrs.name, category)).as_output()
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
    output_file_map_json = ctx.actions.write_json(
        "{}_output_file_map.json".format(category),
        output_file_map,
        pretty = True,
    )
    cmd.add("-output-file-map", output_file_map_json)
    return output_file_map_json
