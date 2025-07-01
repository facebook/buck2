# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@fbcode//buck2/cfg/experimental:set_cfg_modifiers.bzl", "set_cfg_modifiers")

def set_cfg_modifiers_not_from_package_file():
    if native.read_config("buck_e2e", "testing_failure", False):
        set_cfg_modifiers(cfg_modifiers = [])
