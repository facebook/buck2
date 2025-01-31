# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

_ATTRS_VALIDATORS_NAME = "attrs_validators"

AttrsValidatorsInfo = provider(
    fields = {
        "func": typing.Callable[[AnalysisActions, Label, struct], dict[str, Artifact]],
    },
)

def get_attrs_validation_specs(ctx: AnalysisContext) -> list[ValidationSpec]:
    validators = getattr(ctx.attrs, _ATTRS_VALIDATORS_NAME, [])
    if not validators:
        return []

    specs = []
    for validator in validators:
        for name, output in validator[AttrsValidatorsInfo].func(ctx.actions, ctx.label, ctx.attrs).items():
            specs.append(ValidationSpec(name = name, validation_result = output))

    return specs

def _attrs_validators_arg():
    return {
        _ATTRS_VALIDATORS_NAME: attrs.option(
            attrs.list(attrs.dep(providers = [AttrsValidatorsInfo])),
            default = None,
        ),
    }

validation_common = struct(
    attrs_validators_arg = _attrs_validators_arg,
)
