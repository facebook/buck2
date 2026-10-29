# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


class LazyDynamicValueTest(BuckTestCase):
    @buck_test()
    async def test_resolve(self, buck: Buck) -> None:
        await buck.bxl("//:lazy_dynamic_value.bxl:resolve")

    @buck_test()
    async def test_callback_failure_propagates_without_catch(self, buck: Buck) -> None:
        await expect_failure(
            buck.bxl(
                "//:lazy_dynamic_value.bxl:resolve_without_catch",
                "--",
                "--target",
                "callback_fails",
            ),
            stderr_regex="producer callback failed",
        )

    @buck_test()
    async def test_catch_callback_failure(self, buck: Buck) -> None:
        res = await buck.bxl(
            "//:lazy_dynamic_value.bxl:catch",
            "--",
            "--target",
            "callback_fails",
        )
        assert "producer callback failed" in res.stdout

    @buck_test()
    async def test_catch_upstream_artifact_failure(self, buck: Buck) -> None:
        res = await buck.bxl(
            "//:lazy_dynamic_value.bxl:catch",
            "--",
            "--target",
            "artifact_fails",
        )
        assert "failing_upstream" in res.stdout
        assert "producer callback should not run" not in res.stdout

    @buck_test()
    async def test_mixed_batch(self, buck: Buck) -> None:
        res = await buck.bxl("//:lazy_dynamic_value.bxl:mixed_batch")
        summary = json.loads(res.stdout)
        assert list(summary) == ["ok", "callback_fails", "ok2", "artifact_fails"]
        assert summary["ok"] == {"ok": "ok"}
        assert summary["ok2"] == {"ok": "ok2"}
        assert "producer callback failed" in summary["callback_fails"]["error"]
        assert "failing_upstream" in summary["artifact_fails"]["error"]
        assert (
            "producer callback should not run" not in summary["artifact_fails"]["error"]
        )

    @buck_test()
    async def test_resolve_anon_target_dynamic_value(self, buck: Buck) -> None:
        await buck.bxl("//:lazy_dynamic_value.bxl:anon_target_dynamic_value")

    @buck_test()
    async def test_rejects_bxl_dynamic_value(self, buck: Buck) -> None:
        await expect_failure(
            buck.bxl("//:lazy_dynamic_value.bxl:bxl_dynamic_value"),
            stderr_regex="does not accept dynamic value .* owned by a BXL invocation",
        )
