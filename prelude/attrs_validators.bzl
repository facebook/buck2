# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

_RUNNABLE_EXEC_DEP = attrs.exec_dep(providers = [RunInfo])

ATTRS_VALIDATORS_NAME = "attrs_validators"
ATTRS_VALIDATORS_TYPE = attrs.dict(
    attrs.string(),
    attrs.tuple(
        # The list of attrs you want.
        # If the attr key isn't available, it'll fail.
        attrs.list(attrs.string()),
        # This will be passed two named args.
        # --target-attrs, a path to a JSON file with the serialized args.
        # --output, a path to the file the validation should be written to.
        _RUNNABLE_EXEC_DEP,
    ),
    default = {},
)

def get_attrs_validators_outputs(ctx: AnalysisContext) -> list[Artifact]:
    if not hasattr(ctx.attrs, ATTRS_VALIDATORS_NAME):
        return []

    artifacts = []
    for key, (requested_attrs, validator) in ctx.attrs.attrs_validators.items():
        output = ctx.actions.declare_output(key)
        ctx.actions.run(
            cmd_args([
                validator[RunInfo],
                "--target-attrs",
                _build_attr_json_args(ctx, key, requested_attrs),
                "--output",
                output.as_output(),
            ]),
            category = "attrs_validator",
            identifier = key,
        )
        artifacts.append(output)

    return artifacts

def _build_attr_json_args(ctx: AnalysisContext, key: str, requested_attrs: list[str]):
    attr_args = {}
    missing_attrs = []
    for attr in requested_attrs:
        if attr == "configured_target_label":
            attr_args["configured_target_label"] = ctx.label.configured_target()
        elif attr == "target_label":
            attr_args["target_label"] = ctx.label.raw_target()
        elif hasattr(ctx.attrs, attr):
            attr_args[attr] = getattr(ctx.attrs, attr)
        else:
            missing_attrs.append(attr)

    if missing_attrs:
        fail("The following attrs are not available on {}: {}".format(
            ctx.label,
            ", ".join(sorted(missing_attrs)),
        ))

    attr_args = ctx.actions.write_json(
        "{}-attrs.json".format(key),
        attr_args,
        with_inputs = True,
    )

    return attr_args
