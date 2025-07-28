# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _check_run_uuid(_ctx):
    return [
        DefaultInfo(),
        RunInfo([
            "fbpython",
            "-c",
            'import os; assert "BUCK_RUN_BUILD_ID" in os.environ',
        ]),
    ]

check_run_uuid = rule(impl = _check_run_uuid, attrs = {})

def all_defs():
    check_run_uuid(name = "check_run_uuid")
