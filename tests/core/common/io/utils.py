# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck


async def get_files(buck: Buck) -> list[str]:
    res = await buck.targets("root//:")
    for x in res.stderr.splitlines():
        p = x.split("Files: ", 1)
        if len(p) > 1:
            return json.loads(p[1])
    raise Exception("Missing files output: " + res.stderr)
