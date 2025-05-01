# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(platform, refs):
    watchos = refs.watchos[ConstraintValueInfo]
    constraints = {
        s: v
        for (s, v) in platform.configuration.constraints.items()
        if s != refs.os[ConstraintSettingInfo].label
    }
    constraints[watchos.setting.label] = watchos
    new_cfg = ConfigurationInfo(
        constraints = constraints,
        values = platform.configuration.values,
    )
    return PlatformInfo(
        label = "<transitioned-to-watch>",
        configuration = new_cfg,
    )

iphone_to_watch_transition = transition(impl = _impl, refs = {
    "os": "root//:os",
    "watchos": "root//:watchos",
})
