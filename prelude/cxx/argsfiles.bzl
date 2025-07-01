# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

ARGSFILES_SUBTARGET = "argsfiles"

# Information on argsfiles created for compilation.
CompileArgsfile = record(
    # The generated argsfile (does not contain dependent inputs).
    file = field(Artifact),
    # This argsfile as a command form that would use the argsfile (includes dependent inputs).
    cmd_form = field(cmd_args),
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
    dependent_outputs = []
    for _, argsfile in argsfile_by_ext.items():
        argsfiles.append(argsfile.file)

        # To materialize the dependent `Artifact`s of `CompileArgsfile#file`,
        # `CompileArgsfile#cmd_form` is returned in `DefaultInfo#other_outputs`,
        # because it tracks the dependents through the `cmd_args` API.
        dependent_outputs.append(argsfile.cmd_form)

    argsfiles_summary = ctx.actions.write(summary_name, argsfiles)

    return DefaultInfo(default_outputs = [argsfiles_summary] + argsfiles, other_outputs = dependent_outputs)
