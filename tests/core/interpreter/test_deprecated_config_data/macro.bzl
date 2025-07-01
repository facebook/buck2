# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":defs.bzl", "test_rule")

def create_target():
    section = read_config("test", "section")
    conf = read_config("test", "conf")
    root = read_config("test", "root")
    if root == "true":
        read_root_config(section, conf)
    else:
        read_config(section, conf)
    test_rule(name = "test_target_{}_{}".format(section, conf))
