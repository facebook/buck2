# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import re
import typing

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


# Obtain hashes of `<astrologer>` and `<vagabond>` configurations.
async def _obtain_cfg_hashes(buck: Buck) -> typing.Tuple[str, str]:
    result = await buck.cquery(
        "root//:simple",
        "--target-universe",
        "root//:universe",
    )
    [astrologer, vagabond] = result.stdout.splitlines()
    assert astrologer.startswith("root//:simple (<astrologer>#")
    assert vagabond.startswith("root//:simple (<vagabond>#")
    astrologer_hash = re.sub(r".*#(.*)\)", r"\1", astrologer)
    vagabond_hash = re.sub(r".*#(.*)\)", r"\1", vagabond)
    assert re.fullmatch("[0-9a-f]{16}", astrologer_hash), astrologer
    assert re.fullmatch("[0-9a-f]{16}", vagabond_hash), vagabond
    return (astrologer_hash, vagabond_hash)


@buck_test()
async def test_build_configured_full_configuration(buck: Buck) -> None:
    (astrologer_hash, _) = await _obtain_cfg_hashes(buck)

    result = await buck.build(
        f"root//:simple (<astrologer>#{astrologer_hash})",
        "--target-universe",
        "root//:universe",
    )
    out = result.get_build_report().output_for_target("root//:simple").read_text()
    assert f"$$$root//:simple (<astrologer>#{astrologer_hash})$$$" == out


@buck_test()
async def test_build_configured_no_hash(buck: Buck) -> None:
    (_, vagabond_hash) = await _obtain_cfg_hashes(buck)
    result = await buck.build(
        "root//:simple (<vagabond>)",
        "--target-universe",
        "root//:universe",
    )
    out = result.get_build_report().output_for_target("root//:simple").read_text()
    assert f"$$$root//:simple (<vagabond>#{vagabond_hash})$$$" == out


@buck_test()
async def test_build_configured_wrong_hash(buck: Buck) -> None:
    result = await buck.build(
        "root//:simple (<vagabond>#0123456789abcdef)",
        "--target-universe",
        "root//:universe",
    )
    # TODO(nga): this should either fail or emit a warning.
    assert "root//:simple" not in json.loads(result.stdout)["results"]


@buck_test()
async def test_build_configured_no_universe(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//:simple (<vagabond>)",
        ),
        stderr_regex="Targets with explicit configuration can only be built when the",
    )
