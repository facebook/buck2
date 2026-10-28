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
nothing is written until something needs the file; otherwise the write proceeds as before. A copy
of a source file, alone or into a `copied_dir`, asks the same way before copying. None of them
asks under `--local-only` or `--no-remote-cache`.
"""

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
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


async def _written_without_a_probe(buck: Buck, *flags: str) -> None:
    content = random_string()
    res = await buck.build("//:write", *flags, "-c", f"test.content={content}")
    output = res.get_build_report().output_for_target("//:write")
    assert presence_check_rpcs(res) == 0, "the CAS was not asked"
    assert (
        await get_last_execution_kind(buck, category="write")
        == ACTION_EXECUTION_KIND_SIMPLE
    )
    assert output.read_text() == content


class WriteCasProbeTest(BuckTestCase):
    @buck_test(write_invocation_record=True)
    async def test_write_whose_content_the_cas_lacks_is_written(
        self, buck: Buck
    ) -> None:
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
    async def test_write_whose_content_the_cas_has_is_not_written(
        self, buck: Buck
    ) -> None:
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

    @buck_test(write_invocation_record=True)
    async def test_copy_of_a_source_file_the_cas_has_is_not_made(
        self, buck: Buck
    ) -> None:
        content = random_string()
        (buck.cwd / "src.txt").write_text(content)
        # The write targets in the same file need their content configured.
        configs = ["-c", f"test.content={random_string()}"]

        res = await buck.build("//:copy_src", "--materializations=none", *configs)
        assert presence_check_rpcs(res) >= 1, "the probe asked the CAS"
        assert (
            await get_last_execution_kind(buck, category="copy")
            == ACTION_EXECUTION_KIND_SIMPLE
        )

        # An RE action reading the source file uploads its content to the CAS.
        await buck.build(
            "//:consume_src", "--remote-only", "--no-remote-cache", *configs
        )

        # A copy of it now finds the content there: the output is declared as a CAS download and
        # nothing is copied...
        target = "//:copy_src_again"
        res = await buck.build(target, "--materializations=none", *configs)
        output = res.get_build_report().output_for_target(target)
        assert presence_check_answers(res) >= 1, "the probe ran"
        assert (
            await get_last_execution_kind(buck, category="copy")
            == ACTION_EXECUTION_KIND_DEFERRED
        )
        assert not output.exists()

        # ...until something needs the file, which then comes out of the CAS.
        await buck.build(target, *configs)
        assert output.read_text() == content

    @buck_test(write_invocation_record=True)
    async def test_copied_dir_of_a_source_file_the_cas_has_is_not_made(
        self, buck: Buck
    ) -> None:
        content = random_string()
        (buck.cwd / "src.txt").write_text(content)
        configs = ["-c", f"test.content={random_string()}"]
        # `copied_dir` reports the `symlinked_dir` category.
        category = "symlinked_dir"

        res = await buck.build("//:copied_dir_src", "--materializations=none", *configs)
        assert presence_check_rpcs(res) >= 1, "the probe asked the CAS"
        assert (
            await get_last_execution_kind(buck, category=category)
            == ACTION_EXECUTION_KIND_SIMPLE
        )

        await buck.build(
            "//:consume_src", "--remote-only", "--no-remote-cache", *configs
        )

        # The directory's one file is in the CAS now: nothing is copied...
        target = "//:copied_dir_src_again"
        res = await buck.build(target, "--materializations=none", *configs)
        output = res.get_build_report().output_for_target(target)
        assert presence_check_answers(res) >= 1, "the probe ran"
        assert (
            await get_last_execution_kind(buck, category=category)
            == ACTION_EXECUTION_KIND_DEFERRED
        )
        assert not output.exists()

        # ...until something needs it, which then comes out of the CAS.
        await buck.build(target, *configs)
        assert (output / "src.txt").read_text() == content

    @buck_test(write_invocation_record=True, skip_for_materializer=["deferred"])
    async def test_copied_dir_stashes_only_the_files_the_cas_lacks(
        self, buck: Buck
    ) -> None:
        # The probe asks about a copied directory's files one at a time: the directory itself is
        # in the CAS only if some action uploaded it in this exact shape, while its files are
        # inputs the CAS usually has. Here one file is uploaded and one is not, so only the second
        # enters the stash.
        content_a = random_string() * 32
        content_b = random_string() * 8
        (buck.cwd / "mixed_a.txt").write_text(content_a)
        (buck.cwd / "mixed_b.txt").write_text(content_b)
        configs = ["-c", f"test.content={random_string()}"]

        await buck.build(
            "//:consume_mixed_b", "--remote-only", "--no-remote-cache", *configs
        )

        target = "//:copied_dir_mixed"
        res = await buck.build(target, "--materializations=none", *configs)
        output = res.get_build_report().output_for_target(target)
        assert presence_check_answers(res) >= 2, "the probe asked about both files"
        assert (
            await get_last_execution_kind(buck, category="symlinked_dir")
            == ACTION_EXECUTION_KIND_SIMPLE
        )
        snapshot = res.invocation_record()["last_snapshot"]
        assert int(snapshot.get("blob_store_entries", 0)) == 1, (
            "one file entered the stash"
        )
        assert int(snapshot.get("blob_store_bytes", 0)) == len(content_a)
        assert not output.exists()

        await buck.build(target, *configs)
        assert (output / "mixed_a.txt").read_text() == content_a
        assert (output / "mixed_b.txt").read_text() == content_b

    @buck_test(write_invocation_record=True, skip_for_materializer=["deferred"])
    async def test_content_already_on_disk_is_copied_rather_than_downloaded(
        self, buck: Buck
    ) -> None:
        content = random_string()
        configs = ["-c", f"test.content={content}"]

        # An RE action on the write's output puts the content in the CAS, and the upload puts the
        # output on disk; the stash lets go of content the CAS holds.
        await buck.build("//:consume", "--remote-only", "--no-remote-cache", *configs)

        # The second write of the same content is a probe hit, so nothing is written, and its output
        # is a copy of the first's rather than a download.
        target = "//:write_again"
        res = await buck.build(target, *configs)
        output = res.get_build_report().output_for_target(target)
        assert output.read_text() == content
        snapshot = res.invocation_record()["last_snapshot"]
        assert int(snapshot.get("disk_state_materializer_copied_from_disk", 0)) >= 1

    @buck_test(write_invocation_record=True)
    async def test_a_local_only_build_does_not_probe(self, buck: Buck) -> None:
        await _written_without_a_probe(buck, "--local-only")

    @buck_test(write_invocation_record=True)
    async def test_a_build_without_remote_cache_does_not_probe(
        self, buck: Buck
    ) -> None:
        await _written_without_a_probe(buck, "--no-remote-cache")
