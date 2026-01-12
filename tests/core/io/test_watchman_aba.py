# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.core.common.io.file_watcher_tests import run_aba_test
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(setup_eden=False)
async def test_watchman_aba_no_eden(buck: Buck) -> None:
    await run_aba_test(buck)


@buck_test(setup_eden=True)
async def test_watchman_aba_eden(buck: Buck) -> None:
    await run_aba_test(buck)
