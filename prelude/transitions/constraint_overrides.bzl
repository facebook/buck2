# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# NOTE: Currently, constraints can't be propagated via rule attrs and so need to be
#       hard-coded here. We use a read_config to avoid hard-coding these repo-specific
#       constraints into the prelude.

def _platform_overrides() -> list[str]:
    config = read_root_config("buck2", "platform_overrides", "")
    return [override.strip() for override in config.split(",") if override.strip()]

def _constraint_overrides() -> list[str]:
    overrides = read_root_config("buck2", "constraint_overrides", "")
    return [override.strip() for override in overrides.split(",") if override.strip()]

def _constraint_passthroughs() -> list[str]:
    passthroughs = read_root_config("buck2", "constraint_passthroughs", "")
    return [passthrough.strip() for passthrough in passthroughs.split(",") if passthrough.strip()]

_overrides = _platform_overrides() + _constraint_overrides()
_passthroughs = _constraint_passthroughs()

def _resolve(
        refs: struct,
        attrs: struct) -> dict[str, PlatformInfo | list[ConstraintValueInfo] | None]:
    args = {}

    # Resolve target platform override.
    override = None
    if hasattr(attrs, "platform_override"):
        override = getattr(attrs, "platform_override")
    args["platform"] = None
    if override:
        if not hasattr(refs, override):
            fail("Target platform override not supported: {override}".format(
                override = override,
            ))
        ref = getattr(refs, override)
        args["platform"] = ref[PlatformInfo]

    # Resolve constraint value overrides.
    overrides = []
    if hasattr(attrs, "constraint_overrides"):
        overrides = getattr(attrs, "constraint_overrides", [])
    args["constraints"] = []
    for override in overrides:
        if not hasattr(refs, override):
            fail("Constraint value override not supported: {override}".format(
                override = override,
            ))
        ref = getattr(refs, override)
        args["constraints"].append(ref[ConstraintValueInfo])

    return args

def _apply(
        old_platform: PlatformInfo,
        *,
        platform: PlatformInfo | None = None,
        constraints: list[ConstraintValueInfo] = []) -> PlatformInfo:
    # Store passthrough constraint values.
    old_constraints = []
    for constraint in _passthroughs:
        if constraint in old_platform.configuration.constraints:
            old_constraints.append(
                old_platform.configuration.constraints[constraint],
            )

    # Switch target platform.
    platform = platform or old_platform

    # Add passthrough constraint values and apply constraint value overrides.
    new_constraints = {
        label: constraint
        for label, constraint in platform.configuration.constraints.items()
    }
    for constraint in old_constraints:
        new_constraints[constraint.setting.label] = constraint
    for constraint in constraints:
        new_constraints[constraint.setting.label] = constraint

    new_platform = PlatformInfo(
        label = platform.label,
        configuration = ConfigurationInfo(
            constraints = new_constraints,
            values = platform.configuration.values,
        ),
    )

    return new_platform

def _impl(platform: PlatformInfo, refs: struct, attrs: struct) -> PlatformInfo:
    return _apply(platform, **_resolve(refs, attrs))

_refs = {override: override for override in _overrides}

_attributes = {
    "constraint_overrides": attrs.list(attrs.string(), default = []),
    "platform_override": attrs.option(attrs.string(), default = None),
}

_transition = transition(
    impl = _impl,
    refs = _refs,
    attrs = _attributes.keys(),
)

constraint_overrides = struct(
    refs = _refs,
    resolve = _resolve,
    apply = _apply,
    transition = _transition,
    attributes = _attributes,
)
