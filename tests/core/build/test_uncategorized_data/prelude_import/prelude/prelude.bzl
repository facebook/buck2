# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

config = read_config("test", "config")

def _check_config_impl(_ctx):
    # This checks that the config we read is the one that was read in the
    # prelude cell (our other cells have different configs).
    if config != "prelude":
        fail("Unexpected config!")
    return [DefaultInfo()]

check_config = rule(attrs = {}, impl = _check_config_impl)
