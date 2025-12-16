# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def package_validators_decorator(
        ctx: AnalysisContext,
        build_func,
        extension: str):
    package_validators = ctx.attrs.package_validators
    if not package_validators:
        return build_func

    def wrapped_build_func(**build_kwargs):
        final_filename = build_kwargs["output_filename"]
        build_kwargs["output_filename"] = final_filename + "_pre_validation"
        output = build_func(**build_kwargs)

        validation_outputs = _get_package_validation_outputs(
            actions = ctx.actions,
            package_validators = package_validators,
            package_output = output,
        )
        final_output = ctx.actions.declare_output(
            final_filename + extension,
        )
        ctx.actions.run(
            cmd_args(
                [
                    "cp",
                    output,
                    final_output.as_output(),
                ],
                hidden = validation_outputs,
            ),
            category = "package_validators",
            identifier = "symlink_original_output",
        )

        return final_output

    return wrapped_build_func

def _get_package_validation_outputs(
        actions: AnalysisActions,
        package_validators: list,
        package_output: Artifact) -> list[Artifact]:
    if not package_validators:
        return []

    outputs = []
    for idx, validator in enumerate(package_validators):
        validator, validator_args = validator

        output = actions.declare_output(validator.label.name + "_package_validator_{}".format(idx))
        outputs.append(output)

        actions.run(
            cmd_args([
                validator[RunInfo],
                "--package",
                package_output,
                "--output-path",
                output.as_output(),
                validator_args,
            ]),
            category = "package_validator",
            identifier = str(idx),
        )

    return outputs

# A wrapper around AnalysisContext that makes it easier to define subtargets without needing to pass information
# for them all the way back to the outermost analysis impl.
EnhancementContext = record(
    ctx = AnalysisContext,
    actions = AnalysisActions,
    attrs = typing.Any,
    label = Label,

    # methods
    debug_output = typing.Callable,
    get_sub_targets = typing.Callable,
)

def create_enhancement_context(ctx: AnalysisContext) -> EnhancementContext:
    extra_sub_targets = {}

    def debug_output(name: str, output: Artifact, other_outputs = [], sub_targets: dict[str, typing.Any] = {}):
        """Adds a subtarget to expose debugging outputs."""
        extra_sub_targets[name] = [DefaultInfo(default_outputs = [output], other_outputs = other_outputs, sub_targets = sub_targets)]

    def get_sub_targets():
        return extra_sub_targets

    return EnhancementContext(
        ctx = ctx,
        actions = ctx.actions,
        attrs = ctx.attrs,
        label = ctx.label,
        debug_output = debug_output,
        get_sub_targets = get_sub_targets,
    )
