# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl_tr(platform, refs, attrs):
    constraints = {
        s: v
        for (s, v) in platform.configuration.constraints.items()
        if s != refs.os[ConstraintSettingInfo].label
    }

    watchos = refs.watchos[ConstraintValueInfo]
    constraints[watchos.setting.label] = watchos
    new_cfg = ConfigurationInfo(
        constraints = constraints,
        values = platform.configuration.values,
    )
    return PlatformInfo(
        label = "<transitioned-from-" + attrs.device + ">",
        configuration = new_cfg,
    )

iphone_to_watch_transition = transition(
    impl = _impl_tr,
    refs = {
        "os": "root//:os",
        "watchos": "root//:watchos",
    },
    attrs = [
        "device",
    ],
)

def _impl(_ctx):
    return [DefaultInfo()]

test_rule = rule(impl = _impl, attrs = {"device": attrs.string()}, cfg = iphone_to_watch_transition)

test_rule_with_transition_dep = rule(impl = _impl, attrs = {"attr_with_transition": attrs.transition_dep(cfg = iphone_to_watch_transition), "device": attrs.string()})

any_rule = rule(impl = _impl, attrs = {"another_device": attrs.string()})
