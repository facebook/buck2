# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

"""
With `buck2.write_cas_probe` on, a write action asks the CAS for its content before writing it.
When the CAS has the content with enough TTL left, the output is declared as a CAS download and
nothing is written until something needs the file; otherwise the write proceeds as before.
"""

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import get_last_execution_kind, random_string

ACTION_EXECUTION_KIND_SIMPLE = 4
ACTION_EXECUTION_KIND_DEFERRED = 6


def presence_check_rpcs(res: BuckResult) -> int:
    """How many presence-check RPCs the daemon had issued by the end of the command."""
    return int(
        res.invocation_record()["last_snapshot"].get("re_presence_check_rpcs", 0)
    )


def presence_check_answers(res: BuckResult) -> int:
    """How many digests the presence check had answered by the end of the command, whether by
    asking the CAS or from an expiration the digest already carried."""
    snapshot = res.invocation_record()["last_snapshot"]
    return int(snapshot.get("re_presence_check_digests_queried", 0)) + int(
        snapshot.get("re_presence_check_instance_answers", 0)
    )


@buck_test(write_invocation_record=True)
async def test_write_whose_content_the_cas_lacks_is_written(buck: Buck) -> None:
    content = random_string()
    configs = ["-c", f"test.content={content}"]

    res = await buck.build("//:write", *configs)
    output = res.get_build_report().output_for_target("//:write")
    assert presence_check_rpcs(res) >= 1, "the probe asked the CAS"
    assert (
        await get_last_execution_kind(buck, category="write")
        == ACTION_EXECUTION_KIND_SIMPLE
    )
    assert output.read_text() == content


@buck_test(write_invocation_record=True)
async def test_write_whose_content_the_cas_has_is_not_written(buck: Buck) -> None:
    content = random_string()
    configs = ["-c", f"test.content={content}"]

    # Running an RE action on the write's output uploads the content to the CAS.
    await buck.build("//:consume", "--remote-only", "--no-remote-cache", *configs)

    # A second write of the same content finds it there: the output is declared as a CAS
    # download, and neither memory nor disk holds the bytes...
    target = "//:write_again"
    res = await buck.build(target, "--materializations=none", *configs)
    output = res.get_build_report().output_for_target(target)
    # The upload that seeded the CAS may already have recorded the blob's expiration on the
    # digest, in which case the probe answers from it without an RPC.
    assert presence_check_answers(res) >= 1, "the probe ran"
    assert (
        await get_last_execution_kind(buck, category="write")
        == ACTION_EXECUTION_KIND_DEFERRED
    )
    assert not output.exists()

    # ...until something needs the file, which then comes out of the CAS.
    await buck.build(target, *configs)
    assert output.read_text() == content
