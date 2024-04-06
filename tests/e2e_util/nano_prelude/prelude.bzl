# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("//:cfg_rules.bzl", _config_setting = "config_setting", _constraint_setting = "constraint_setting", _constraint_value = "constraint_value", _platform = "platform")
load("//:stub_rules.bzl", _stub = "stub", _trivial_build = "trivial_build")

platform = _platform
config_setting = _config_setting
constraint_setting = _constraint_setting
constraint_value = _constraint_value
stub = _stub
trivial_build = _trivial_build
