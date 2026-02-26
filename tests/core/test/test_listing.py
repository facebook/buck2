# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from enum import Enum
from typing import Any, Dict, List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import filter_events, random_string


class TestDiscovery(Enum):
    EXECUTED = 1
    CACHED = 2
    SKIPPED = 3


@buck_test()
async def test_discovery_output_dir(buck: Buck) -> None:
    args = [
        "//:ok",
    ]
    await run_test_and_check_discovery_presence(buck, TestDiscovery.EXECUTED, args)

    # TODO(ianc) We shouldn't add `buck-out/v2/test/` twice
    discovery_output_path = (
        buck.cwd / "buck-out" / "v2" / "test" / "buck-out" / "v2" / "test" / "discovery"
    ).resolve()

    assert discovery_output_path.exists()


@buck_test()
async def test_discovery_cached_on_dice(buck: Buck) -> None:
    args = [
        "//:ok",
    ]
    await run_test_and_check_discovery_presence(buck, TestDiscovery.EXECUTED, args)
    await run_test_and_check_discovery_presence(buck, TestDiscovery.SKIPPED, args)


@buck_test()
async def test_failed_discovery_not_cached_on_dice(buck: Buck) -> None:
    args = [
        "//:bad",
    ]
    await expect_failure(
        buck.test(*args),
        stderr_regex="Failed to list tests",
    )
    whatran = (await buck.log("what-ran")).stdout
    assert "test.discovery" in whatran
    assert "test.run" not in whatran

    await expect_failure(
        buck.test(*args),
        stderr_regex="Failed to list tests",
    )
    whatran = (await buck.log("what-ran")).stdout
    assert "test.discovery" in whatran


@buck_test()
async def test_listing_uncacheable(buck: Buck) -> None:
    seed = random_string()
    args = [
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.remote_enabled=false",
        "-c",
        "test.local_enabled=true",
        "-c",
        "test.remote_cache_enabled=true",
        "//:listing_uncacheable",
    ]
    # Check it executed locally consistently
    await run_test_and_check_discovery_presence(buck, TestDiscovery.EXECUTED, args)
    await buck.kill()
    await run_test_and_check_discovery_presence(buck, TestDiscovery.EXECUTED, args)
    # Check cache is not uploaded
    cached = await _cache_uploads(buck)
    assert len(cached) == 0


@buck_test()
async def test_discovery_cached_on_re(buck: Buck) -> None:
    seed = random_string()
    args = [
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.local_enabled=false",
        "-c",
        "test.remote_enabled=true",
        "-c",
        "test.remote_cache_enabled=true",
        "//:test",
    ]
    await run_test_and_check_discovery_presence(buck, TestDiscovery.EXECUTED, args)
    await buck.kill()
    await run_test_and_check_discovery_presence(buck, TestDiscovery.CACHED, args)
    await buck.kill()
    args = [
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.remote_enabled=false",
        "-c",
        "test.local_enabled=true",
        "-c",
        "test.remote_cache_enabled=true",
        "//:test",
    ]
    await run_test_and_check_discovery_presence(buck, TestDiscovery.CACHED, args)


@buck_test()
@env("BUCK2_TEST_SKIP_ACTION_CACHE_WRITE", "true")
async def test_local_discovery_uploaded_to_cache(buck: Buck) -> None:
    seed = random_string()
    args = [
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.allow_cache_uploads=true",
        "-c",
        "test.remote_cache_enabled=true",
        "//:ok",
    ]
    await run_test_and_check_discovery_presence(buck, TestDiscovery.EXECUTED, args)
    await _check_cache_uploaded(buck)


async def _check_cache_uploaded(buck: Buck) -> None:
    result = await _cache_uploads(buck)
    assert len(result) == 1
    assert result[0]["success"]


async def _cache_uploads(buck: Buck) -> List[Dict[str, Any]]:
    return await filter_events(buck, "Event", "data", "SpanEnd", "data", "CacheUpload")


async def run_test_and_check_discovery_presence(
    buck: Buck,
    discovery: TestDiscovery,
    args: List[str],
) -> None:
    await buck.test(*args)
    stdout = (await buck.log("what-ran")).stdout

    assert "test.run" in stdout
    match discovery:
        case TestDiscovery.EXECUTED:
            for line in stdout.splitlines():
                if "test.discovery" in line:
                    if "cached" in line:
                        raise Exception("test.discovery was cached")
                    else:
                        return
            raise Exception("test.discovery was not skipped")
        case TestDiscovery.CACHED:
            for line in stdout.splitlines():
                if "test.discovery" in line:
                    if "cache" in line:
                        return
                    else:
                        raise Exception("test.discovery was executed")
            raise Exception("test.discovery was not skipped")
        case TestDiscovery.SKIPPED:
            assert "test.discovery" not in stdout
        case _:
            raise Exception("Unexpected discovery type")
