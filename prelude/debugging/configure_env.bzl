# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//debugging:labels.bzl", "DBG_INFO_DISABLE_INCOMPATIBLE_SANITIZERS")
load("@prelude//debugging:types.bzl", "UserMessage")

ConfiguredEnv = record(
    env = field(dict[str, str]),
    messages = field(list[UserMessage]),
)

_DISABLE_LEAK_DETECTION = "detect_leaks=0"

def configure_env(env: dict[str, str], labels: list[str]) -> ConfiguredEnv:
    messages = []
    if DBG_INFO_DISABLE_INCOMPATIBLE_SANITIZERS in labels:
        messages.append(
            UserMessage(
                title = "Leak sanitizer is disabled.",
                body = "You are trying to debug a binary with sanitizers. Certain sanitizers do not work under the debugger.\n",
            ),
        )
        env = _inject_env(env, "ASAN_OPTIONS", _DISABLE_LEAK_DETECTION, lambda x: x + "," + _DISABLE_LEAK_DETECTION)

    return ConfiguredEnv(env = env, messages = messages)

def _inject_env(env: dict[str, str], key: str, value: str, merge: typing.Callable[[str], str]) -> dict[str, str]:
    existing_env = env.get(key)
    if existing_env:
        env[key] = merge(existing_env)
    else:
        env[key] = value
    return env
