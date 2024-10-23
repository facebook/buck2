# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from typing import Any, Dict, List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.utils import filter_events, random_string


@buck_test()
async def test_discovery_cached_on_dice(buck: Buck) -> None:
    args = [
        "-c",
        "buck2.cache_test_listings=//:ok",
        "//:ok",
    ]
    await run_test_and_check_discovery_presence(buck, False, args)
    await run_test_and_check_discovery_presence(buck, True, args)


@buck_test()
async def test_discovery_not_cached_for_not_matching_pattern(buck: Buck) -> None:
    args = [
        "-c",
        "buck2.cache_test_listings=//:not_ok",
        "//:ok",
    ]
    await run_test_and_check_discovery_presence(buck, False, args)
    await run_test_and_check_discovery_presence(buck, False, args)


@buck_test()
async def test_discovery_cache_turned_off(buck: Buck) -> None:
    args = [
        "//:ok",
    ]
    await run_test_and_check_discovery_presence(buck, False, args)
    await run_test_and_check_discovery_presence(buck, False, args)


@buck_test()
async def test_discovery_cached_on_re(buck: Buck) -> None:
    seed = random_string()
    args = [
        "-c",
        "buck2.cache_test_listings=//:ok",
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
    await run_test_and_check_discovery_presence(buck, False, args)
    await buck.kill()
    await run_test_and_check_discovery_presence(buck, True, args)
    await buck.kill()
    args = [
        "-c",
        "buck2.cache_test_listings=//:ok",
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
    await run_test_and_check_discovery_presence(buck, True, args)


@buck_test()
@env("BUCK2_TEST_SKIP_ACTION_CACHE_WRITE", "true")
async def test_local_discovery_uploaded_to_cache(buck: Buck) -> None:
    seed = random_string()
    args = [
        "-c",
        "buck2.cache_test_listings=//:ok",
        "-c",
        f"test.seed={seed}",
        "-c",
        "test.allow_cache_uploads=true",
        "-c",
        "test.remote_cache_enabled=true",
        "//:ok",
    ]
    await run_test_and_check_discovery_presence(buck, False, args)
    await _check_cache_uploaded(buck)


async def _check_cache_uploaded(buck: Buck) -> None:
    result = await _cache_uploads(buck)
    assert len(result) == 1
    assert result[0]["success"]


async def _cache_uploads(buck: Buck) -> List[Dict[str, Any]]:
    return await filter_events(buck, "Event", "data", "SpanEnd", "data", "CacheUpload")


async def run_test_and_check_discovery_presence(
    buck: Buck,
    is_absent: bool,
    args: List[str],
) -> None:
    await buck.test(*args)
    stdout = (await buck.log("what-ran")).stdout

    assert "test.run" in stdout
    if is_absent:
        assert "test.discovery" not in stdout
    else:
        assert "test.discovery" in stdout
