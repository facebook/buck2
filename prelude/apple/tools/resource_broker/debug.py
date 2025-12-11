# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import os
import shlex
import sys

REPLAY_ENVS_TO_REMOVE = [
    "DUMP_REPLAY",
]


def debug_dump_replay() -> None:
    if os.environ.get("DUMP_REPLAY") is None:
        return
    filtered_envs = dict(os.environ)
    for env in REPLAY_ENVS_TO_REMOVE:
        filtered_envs.pop(env, None)
    envs = " ".join(
        f"{key}={shlex.quote(value)}" for key, value in sorted(filtered_envs.items())
    )
    command = shlex.join(sys.argv)
    replay_file = "/tmp/resource_broker_replay.sh"
    with open(replay_file, "w") as f:
        f.write(f"{envs} {command}\n")
    os.chmod(replay_file, 0o755)
