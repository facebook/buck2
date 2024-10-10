# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl_ok(_ctx):
    script = """
import sys;
if '--list' in sys.argv:
    print('test1\\n')
sys.exit(0)
"""
    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", script],
            type = "lionhead",
        ),
    ]

ok_test = rule(attrs = {}, impl = _impl_ok)
