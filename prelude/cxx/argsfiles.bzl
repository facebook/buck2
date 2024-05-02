# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

ARGSFILES_SUBTARGET = "argsfiles"

# Information on argsfiles created for compilation.
CompileArgsfile = record(
    # The generated argsfile (does not contain dependent inputs).
    file = field(Artifact),
    # This argsfile as a command form that would use the argsfile (includes dependent inputs).
    cmd_form = field(cmd_args),
    # Input args necessary for the argsfile to reference.
    input_args = field(list[cmd_args]),
    # Args as written to the argsfile (with shell quoting applied).
    args = field(cmd_args),
    # Args aggregated for the argsfile excluding file prefix args (excludes shell quoting).
    args_without_file_prefix_args = field(cmd_args),
)

CompileArgsfiles = record(
    # Relative path argsfiles used for build actions, mapped by extension.
    relative = field(dict[str, CompileArgsfile], default = {}),
    # Argsfiles used for Xcode integration, mapped by extension.
    xcode = field(dict[str, CompileArgsfile], default = {}),
)

def get_argsfiles_output(ctx: AnalysisContext, argsfile_by_ext: dict[str, CompileArgsfile], summary_name: str) -> DefaultInfo:
    argsfiles = []
    argsfile_names = []
    dependent_outputs = []
    for _, argsfile in argsfile_by_ext.items():
        argsfiles.append(argsfile.file)
        argsfile_names.append(cmd_args(argsfile.file, ignore_artifacts = True))
        dependent_outputs.extend(argsfile.input_args)

    argsfiles_summary = ctx.actions.write(summary_name, cmd_args(argsfile_names))

    return DefaultInfo(default_outputs = [argsfiles_summary] + argsfiles, other_outputs = dependent_outputs)
