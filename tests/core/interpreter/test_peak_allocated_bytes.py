# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events


@buck_test()
async def test_peak_allocated_bytes(buck: Buck) -> None:
    await buck.uquery("//:EEE")
    span_end_load_event = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "Load",
    )
    assert len(span_end_load_event) == 1
    starlark_peak_allocated_bytes = span_end_load_event[0][
        "starlark_peak_allocated_bytes"
    ]
    # list occupies pointer size (8) * number of elements (~10MB) + some extra overhead for bookkeeping
    assert starlark_peak_allocated_bytes >= (8 * 10 * 1 << 20)
    # check that it is no more than +10%
    assert starlark_peak_allocated_bytes < (8 * 11 * 1 << 20)
