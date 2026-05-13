# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# OSS stub for `@fbcode//clifoundation/cli_target/buck_defs:cli.bzl`. The Meta
# CLI deployer infrastructure isn't ported to OSS — `cli.deployer(...)` is a
# no-op so its argument-builder helpers can return any placeholder value.

def _noop(**_kwargs):
    pass

def _placeholder(*_args, **_kwargs):
    return None

_TOKEN = struct()

cli = struct(
    deployer = _noop,
    distribution = _placeholder,
    bump_diffs = _placeholder,
    destination = _placeholder,
    repository = struct(FBSOURCE = _TOKEN),
    metadata = _placeholder,
    criticality = _placeholder,
    level = struct(NOT_CRITICAL = _TOKEN),
    packaging = _placeholder,
    dotslash = _placeholder,
    platform = struct(
        linux = _placeholder,
        macos = _placeholder,
        windows = _placeholder,
    ),
    architecture = struct(AARCH64 = _TOKEN, X86_64 = _TOKEN),
    release = _placeholder,
    artifact_ci_config = _placeholder,
    conveyor = _placeholder,
    frequency = struct(DAILY = _TOKEN),
    holiday_country = struct(UNITED_STATES = _TOKEN),
    srconveyor = _placeholder,
    rollout = struct(AT_ONCE = _TOKEN),
)
