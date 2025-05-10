# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

BuildModeInfo = provider(
    fields = {
        "cell": str,
        "mode": str | None,
    },
)

def stringify_build_mode_infos(infos: list[BuildModeInfo]) -> str | None:
    kvs = []
    for info in infos:
        if info.mode == None:
            continue
        kvs.append(info.cell + "=" + info.mode)

    remote_execution_action_key = None
    if kvs:
        remote_execution_action_key = " ".join(kvs)
    return remote_execution_action_key
