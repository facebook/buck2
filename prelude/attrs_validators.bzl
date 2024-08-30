# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

AttrsValidatorsInfo = provider(
    fields = {
        "func": typing.Callable[[AnalysisActions, Label, struct], dict[str, Artifact]],
    },
)

ATTRS_VALIDATORS_NAME = "attrs_validators"
ATTRS_VALIDATORS_TYPE = attrs.option(attrs.list(attrs.dep(providers = [AttrsValidatorsInfo])), default = None)

def get_attrs_validators_outputs(ctx: AnalysisContext) -> (list[Provider], dict[str, list[Provider]]):
    validators = getattr(ctx.attrs, ATTRS_VALIDATORS_NAME, [])
    if not validators:
        return ([], {})

    specs = []
    sub_targets = {}
    for validator in validators:
        for name, output in validator[AttrsValidatorsInfo].func(ctx.actions, ctx.label, ctx.attrs).items():
            specs.append(ValidationSpec(name = name, validation_result = output))

            if name in sub_targets:
                fail("Collison: two attrs_validators with the same name '{}': {} and {}".format(
                    name,
                    output,
                    sub_targets[name],
                ))

            sub_targets[name] = [DefaultInfo(output)]

    return (
        [ValidationInfo(validations = specs)] if specs else [],
        {
            "attrs-validators": [
                DefaultInfo(
                    # It'll be expensive to put all the artifacts in here, just skip it.
                    default_outputs = None,
                    sub_targets = sub_targets,
                ),
            ],
        } if sub_targets else {},
    )
