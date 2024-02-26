# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":tags_helper.bzl", "selects_for_tags", "tag_to_constrant_value")

def _cgo_enabled_transition(platform, refs, attrs):
    constraints = platform.configuration.constraints

    # Cancel transition if the value already set
    # to enable using configuration modifiers for overiding this option
    cgo_enabled_setting = refs.cgo_enabled_auto[ConstraintValueInfo].setting
    if cgo_enabled_setting.label in constraints:
        return platform

    if attrs.cgo_enabled == None:
        cgo_enabled_ref = refs.cgo_enabled_auto
    elif attrs.cgo_enabled == True:
        cgo_enabled_ref = refs.cgo_enabled_true
    else:
        cgo_enabled_ref = refs.cgo_enabled_false

    cgo_enabled_value = cgo_enabled_ref[ConstraintValueInfo]
    constraints[cgo_enabled_value.setting.label] = cgo_enabled_value

    new_cfg = ConfigurationInfo(
        constraints = constraints,
        values = platform.configuration.values,
    )

    return PlatformInfo(
        label = platform.label,
        configuration = new_cfg,
    )

def _compile_shared_transition(platform, refs, _):
    compile_shared_value = refs.compile_shared_value[ConstraintValueInfo]
    constraints = platform.configuration.constraints
    constraints[compile_shared_value.setting.label] = compile_shared_value
    new_cfg = ConfigurationInfo(
        constraints = constraints,
        values = platform.configuration.values,
    )

    return PlatformInfo(
        label = platform.label,
        configuration = new_cfg,
    )

def _race_transition(platform, refs, attrs):
    constraints = platform.configuration.constraints

    # Cancel transition if the value already set
    # to enable using configuration modifiers for overiding this option
    race_setting = refs.race_false[ConstraintValueInfo].setting
    if race_setting.label in constraints:
        return platform

    if attrs.race == True:
        race_ref = refs.race_true
    else:
        race_ref = refs.race_false

    race_value = race_ref[ConstraintValueInfo]
    constraints[race_value.setting.label] = race_value

    new_cfg = ConfigurationInfo(
        constraints = constraints,
        values = platform.configuration.values,
    )

    return PlatformInfo(
        label = platform.label,
        configuration = new_cfg,
    )

def _tags_transition(platform, refs, attrs):
    constraints = platform.configuration.constraints
    for tag in attrs.tags:
        ref_name = "tag_{}__value".format(tag)
        if not hasattr(refs, ref_name):
            fail("Add tags to .buckconfig attrubute `go.allowed_tags` to allow using it")

        tag_value = getattr(refs, ref_name)[ConstraintValueInfo]
        constraints[tag_value.setting.label] = tag_value

    new_cfg = ConfigurationInfo(
        constraints = constraints,
        values = platform.configuration.values,
    )

    return PlatformInfo(
        label = platform.label,
        configuration = new_cfg,
    )

def _chain_transitions(transitions):
    def tr(platform, refs, attrs):
        for t in transitions:
            platform = t(platform, refs, attrs)
        return platform

    return tr

_tansitions = [_cgo_enabled_transition, _compile_shared_transition, _race_transition, _tags_transition]

_refs = {
    "cgo_enabled_auto": "prelude//go/constraints:cgo_enabled_auto",
    "cgo_enabled_false": "prelude//go/constraints:cgo_enabled_false",
    "cgo_enabled_true": "prelude//go/constraints:cgo_enabled_true",
    "race_false": "prelude//go/constraints:race_false",
    "race_true": "prelude//go/constraints:race_true",
} | {
    "tag_{}__value".format(tag): constrant_value
    for tag, constrant_value in tag_to_constrant_value().items()
}

_attrs = ["cgo_enabled", "race", "tags"]

go_binary_transition = transition(
    impl = _chain_transitions(_tansitions),
    refs = _refs | {
        "compile_shared_value": "prelude//go/constraints:compile_shared_false",
    },
    attrs = _attrs,
)

go_test_transition = transition(
    impl = _chain_transitions(_tansitions),
    refs = _refs | {
        "compile_shared_value": "prelude//go/constraints:compile_shared_false",
    },
    attrs = _attrs,
)

go_exported_library_transition = transition(
    impl = _chain_transitions(_tansitions),
    refs = _refs | {
        "compile_shared_value": "prelude//go/constraints:compile_shared_true",
    },
    attrs = _attrs,
)

cgo_enabled_attr = attrs.default_only(attrs.option(attrs.bool(), default = select({
    "DEFAULT": None,
    "prelude//go/constraints:cgo_enabled_auto": None,
    "prelude//go/constraints:cgo_enabled_false": False,
    "prelude//go/constraints:cgo_enabled_true": True,
})))

compile_shared_attr = attrs.default_only(attrs.bool(default = select({
    "DEFAULT": False,
    "prelude//go/constraints:compile_shared_false": False,
    "prelude//go/constraints:compile_shared_true": True,
})))

race_attr = attrs.default_only(attrs.bool(default = select({
    "DEFAULT": False,
    "prelude//go/constraints:race_false": False,
    "prelude//go/constraints:race_true": True,
})))

tags_attr = attrs.default_only(attrs.list(attrs.string(), default = selects_for_tags()))
