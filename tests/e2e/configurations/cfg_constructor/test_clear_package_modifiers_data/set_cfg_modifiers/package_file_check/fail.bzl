# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//buck2/cfg/experimental:set_cfg_modifiers.bzl", "set_cfg_modifiers")

def set_cfg_modifiers_not_from_package_file():
    if native.read_config("buck_e2e", "testing_failure", False):
        set_cfg_modifiers(cfg_modifiers = [])
