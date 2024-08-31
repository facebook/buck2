# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform


@buck_test(inplace=True)
async def test_linker_argsfile_valid(buck: Buck) -> None:
    args = [
        "fbcode//buck2/tests/targets/rules/cxx/hello_world:welcome[linker.argsfile]",
        "--show-full-output",
        get_mode_from_platform(),
    ]
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()
    assert len(output_dict) == 1
    output_path = next(iter(output_dict.values()))
    # Ensure that the argsfile exists and is not empty.
    assert os.path.exists(output_path)
    assert os.path.getsize(output_path) > 0
