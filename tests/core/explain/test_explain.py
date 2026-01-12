# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import tempfile

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from manifold.clients.python.manifold_client_deprecated import Client as ManifoldClient

BUCKET_CONFIG = {"bucket": "buck2_logs", "apikey": "buck2_logs-key"}


async def manifold_exists(path: str) -> bool:
    with ManifoldClient(BUCKET_CONFIG) as client:
        return client.exists(bucket="buck2_logs", path=path)


@buck_test()
async def test_dummy_to_make_this_file_not_empty_on_windows(buck: Buck) -> None:
    pass


@buck_test(skip_for_os=["windows"])
async def test_explain(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as tmpdirname:
        output = f"{tmpdirname}/index.html"
        await buck.build("//:simple")
        res = await buck.explain("--output", output)
        assert "Using last build invocation `buck2 build //:simple" in res.stderr

        # check we wrote something
        with open(output, "rb") as f:
            assert len(f.read(10)) == 10


@buck_test(skip_for_os=["windows"])
async def test_explain_alias(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as tmpdirname:
        output = f"{tmpdirname}/index.html"
        await buck.build("other_alias")
        await buck.explain("--output", output)


@buck_test(skip_for_os=["windows"])
async def test_explain_no_cell(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as tmpdirname:
        output = f"{tmpdirname}/index.html"
        await buck.build(":simple")
        await buck.explain("--output", output)


@buck_test(skip_for_os=["windows"])
async def test_explain_universe(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as tmpdirname:
        output = f"{tmpdirname}/index.html"
        # check no universe fails for both
        await expect_failure(
            buck.build("//:doesnt_exist"),
            stderr_regex="Unknown target `doesnt_exist` from package",
        )
        await expect_failure(
            buck.explain("--output", output),
            stderr_regex="Unknown target `doesnt_exist` from package",
        )

        # both don't fail with universe
        buck.build("//:doesnt_exist -u :simple")
        buck.explain("--output", output)


@buck_test(skip_for_os=["windows"])
async def test_explain_only_builds(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as tmpdirname:
        output = f"{tmpdirname}/index.html"

        await buck.uquery("//:simple")
        await expect_failure(
            buck.explain("--output", output),
            stderr_regex="No recent build commands found",
        )

        await buck.build("//:simple")
        await buck.explain("--output", output)
        await buck.explain("--output", output)


@buck_test(skip_for_os=["windows"])
@env("BUCK2_TEST_MANIFOLD_TTL_S", str(84_000))  # 1 day
async def test_explain_upload(buck: Buck) -> None:
    uuid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
    env = {"BUCK_WRAPPER_UUID": uuid}
    await buck.build("//:simple", env=env)
    await buck.explain(env=env)

    assert await manifold_exists(path=f"flat/{uuid}-explain.html") is True


@buck_test(skip_for_os=["windows"])
async def test_explain_target_platform(buck: Buck) -> None:
    # no config fails
    await expect_failure(
        buck.build(":foo"),
        stderr_regex="Unknown target `foo`",
    )

    # with config works and so does `explain`
    await buck.build(":foo", "--config=test.config=foo")
    with tempfile.TemporaryDirectory() as tmpdirname:
        output = f"{tmpdirname}/index.html"
        await buck.explain("--output", output)
