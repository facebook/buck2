# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import platform

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events


@buck_test()
async def test_cpu_instruction_count(buck: Buck) -> None:
    await buck.uquery("//:")
    span_end_load_event = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "Load",
    )
    assert len(span_end_load_event) == 1

    cpu_instruction_count = span_end_load_event[0]["cpu_instruction_count"]

    # We only populate counters on Linux
    if platform.system() == "Linux":
        if cpu_instruction_count is None:
            # warnings.warn(
            # pyre-ignore[29]: pytest.xfail is callable at runtime
            pytest.xfail(
                "cpu_instruction_count is None, but we expect it to be populated on Linux most of the time. This is not a failure."
            )
        else:
            assert cpu_instruction_count >= 1000
    else:
        assert cpu_instruction_count is None
