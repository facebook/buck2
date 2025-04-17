# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//debugging:labels.bzl", "DBG_INFO_DISABLE_INCOMPATIBLE_SANITIZERS")

_DISABLE_LEAK_DETECTION = "detect_leaks=0"

def configure_env(env: dict[str, str], labels: list[str]) -> dict[str, str]:
    if DBG_INFO_DISABLE_INCOMPATIBLE_SANITIZERS in labels:
        return _inject_env(env, "ASAN_OPTIONS", _DISABLE_LEAK_DETECTION, lambda x: x + "," + _DISABLE_LEAK_DETECTION)
    return env

def _inject_env(env: dict[str, str], key: str, value: str, merge: typing.Callable[[str], str]) -> dict[str, str]:
    existing_env = env.get(key)
    if existing_env:
        env[key] = merge(existing_env)
    else:
        env[key] = value
    return env
