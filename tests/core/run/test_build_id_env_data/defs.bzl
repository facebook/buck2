# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _check_run_uuid(_ctx):
    return [
        DefaultInfo(),
        RunInfo([
            "python3",
            "-c",
            'import os; assert "BUCK_RUN_BUILD_ID" in os.environ',
        ]),
    ]

check_run_uuid = rule(impl = _check_run_uuid, attrs = {})

def all_defs():
    check_run_uuid(name = "check_run_uuid")
