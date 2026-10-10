# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import hashlib

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import random_string


@buck_test()
async def test_cas_artifact_expiration_out_of_range(buck: Buck) -> None:
    # Regression test: this used to panic the daemon instead of reporting an error.
    await expect_failure(
        buck.build("root//:out_of_range_expiration"),
        stderr_regex="Out-of-range value `4611686018427387904` for expires_after_timestamp",
    )


@buck_test()
async def test_cas_artifact_missing_digest_is_a_clear_error(buck: Buck) -> None:
    content = random_string().encode()
    # patternlint-disable-next-line poor-choice-of-hash-function
    digest = f"{hashlib.sha1(content).hexdigest()}:{len(content)}"
    await expect_failure(
        buck.build("root//:missing", "-c", f"test.missing_digest={digest}"),
        stderr_regex=f"The digest `{digest}` was not found in the CAS under use case `buck2-testing`",
    )


@buck_test()
async def test_cas_artifact_from_another_namespace_is_copied_into_the_daemons(
    buck: Buck,
) -> None:
    # `buck2-default` and the `buck2-testing` use case this daemon runs under are separate CAS
    # namespaces (see the harness). Seed a blob in the former by running a remote action on it
    # there.
    content = random_string()
    configs = ["-c", f"test.content={content}"]
    await buck.build(
        "root//:seed",
        "--remote-only",
        "--no-remote-cache",
        "-c",
        "buck2_re_client.override_use_case=buck2-default",
        *configs,
    )
    seed = await buck.build("root//:seed_content", *configs)
    written = (
        seed.get_build_report().output_for_target("root//:seed_content").read_bytes()
    )
    # patternlint-disable-next-line poor-choice-of-hash-function
    digest = f"{hashlib.sha1(written).hexdigest()}:{len(written)}"
    configs = ["-c", f"test.digest={digest}"]

    # The seeding command ran under the other use case in this daemon and left that namespace's
    # expiration on the shared digest, from which the daemon would answer "present" without asking
    # RE. A fresh daemon asks. Its own namespace does not have the blob yet; without this check the
    # rest of the test would pass just as well if the two use cases shared a namespace and no copy
    # ever happened.
    await buck.kill()
    await expect_failure(
        buck.build("root//:canonical", *configs),
        stderr_regex=f"The digest `{digest}` was not found in the CAS under use case `buck2-testing`",
    )

    # The artifact names the other namespace's use case; the daemon copies the blob into its own.
    res = await buck.build("root//:reconciled", *configs)
    assert (
        res.get_build_report().output_for_target("root//:reconciled").read_bytes()
        == written
    )

    # Now the same digest resolves in the daemon's own namespace by itself...
    canonical = await buck.build("root//:canonical", *configs)
    assert (
        canonical.get_build_report().output_for_target("root//:canonical").read_bytes()
        == written
    )

    # ...and a remote action in that namespace can consume it, which the copy is for.
    check = await buck.build(
        "root//:check", "--remote-only", "--no-remote-cache", *configs
    )
    assert (
        check.get_build_report().output_for_target("root//:check").read_bytes()
        == written
    )


@buck_test()
async def test_cas_artifact_larger_than_an_inline_rpc_is_copied(buck: Buck) -> None:
    # As above, with a blob well past what any RPC inlines, so the copy has to go through the
    # file-shaped download and upload.
    configs = ["-c", f"test.seed={random_string()}"]
    await buck.build(
        "root//:seed_big",
        "--remote-only",
        "--no-remote-cache",
        "-c",
        "buck2_re_client.override_use_case=buck2-default",
        *configs,
    )
    built = await buck.build("root//:big_content", *configs)
    written = (
        built.get_build_report().output_for_target("root//:big_content").read_bytes()
    )
    assert len(written) == 5_000_000
    # patternlint-disable-next-line poor-choice-of-hash-function
    digest = f"{hashlib.sha1(written).hexdigest()}:{len(written)}"
    configs = ["-c", f"test.big_digest={digest}"]

    await buck.kill()
    await expect_failure(
        buck.build("root//:canonical_big", *configs),
        stderr_regex=f"The digest `{digest}` was not found in the CAS under use case `buck2-testing`",
    )

    res = await buck.build("root//:reconciled_big", *configs)
    assert (
        res.get_build_report().output_for_target("root//:reconciled_big").read_bytes()
        == written
    )

    canonical = await buck.build("root//:canonical_big", *configs)
    assert (
        canonical.get_build_report()
        .output_for_target("root//:canonical_big")
        .read_bytes()
        == written
    )

    check = await buck.build(
        "root//:check_big", "--remote-only", "--no-remote-cache", *configs
    )
    assert (
        check.get_build_report().output_for_target("root//:check_big").read_bytes()
        == written
    )


@buck_test(data_dir="sha1_under_blake3")
async def test_cas_artifact_with_a_sha1_digest_reconciles_under_a_blake3_daemon(
    buck: Buck,
) -> None:
    # Production daemons prefer BLAKE3-KEYED and accept sha1 for digests rules supply. The copy
    # verifies what it downloaded against the artifact's digest, which only works if it hashes
    # with that digest's own algorithm rather than the daemon's preferred one. Sources are hashed
    # with sha1 in this fixture, so a remote action's upload of a source file seeds a sha1-named
    # blob in the other namespace.
    content = random_string().encode()
    (buck.cwd / "seed.txt").write_bytes(content)
    await buck.build(
        "root//:seed",
        "--remote-only",
        "--no-remote-cache",
        "-c",
        "buck2_re_client.override_use_case=buck2-default",
    )
    # patternlint-disable-next-line poor-choice-of-hash-function
    digest = f"{hashlib.sha1(content).hexdigest()}:{len(content)}"
    configs = ["-c", f"test.digest={digest}"]

    await buck.kill()
    await expect_failure(
        buck.build("root//:canonical", *configs),
        stderr_regex=f"The digest `{digest}` was not found in the CAS under use case `buck2-testing`",
    )

    res = await buck.build("root//:reconciled", *configs)
    assert (
        res.get_build_report().output_for_target("root//:reconciled").read_bytes()
        == content
    )

    canonical = await buck.build("root//:canonical", *configs)
    assert (
        canonical.get_build_report().output_for_target("root//:canonical").read_bytes()
        == content
    )
