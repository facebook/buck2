# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("//:asserts.bzl", _asserts = "asserts")
load("//:cfg_rules.bzl", _config_setting = "config_setting", _constraint_setting = "constraint_setting", _constraint_value = "constraint_value", _execution_platform = "execution_platform", _platform = "platform")
load("//:execution_platforms.bzl", _execution_platforms = "execution_platforms")
load("//:stub_rules.bzl", _stub = "stub", _stub_toolchain = "stub_toolchain", _trivial_build = "trivial_build")

asserts = _asserts
platform = _platform
config_setting = _config_setting
constraint_setting = _constraint_setting
constraint_value = _constraint_value
execution_platform = _execution_platform
execution_platforms = _execution_platforms
stub = _stub
stub_toolchain = _stub_toolchain
trivial_build = _trivial_build
