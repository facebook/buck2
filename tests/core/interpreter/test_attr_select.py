# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException, BuckResult
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


@buck_test()
async def test_select_fail(buck: Buck) -> None:
    outputs: list[str] = []

    async def run(*args: str) -> BuckResult:
        outputs.append("$ buck " + " ".join(args))
        try:
            res = await buck.run_buck_command(*args)
            outputs.append(res.stdout)
        except BuckException as e:
            outputs.append(e.stderr)
            raise e
        return res

    await run("cquery", "root//:foo", "-v0", "--target-platforms=//:platform1")
    await expect_failure(
        run(
            "cquery",
            "root//:foo",
            "--console=none",
            "-v0",
            "--target-platforms=//:platform2",
        )
    )
    await expect_failure(
        run(
            "cquery",
            "root//:foo",
            "--console=none",
            "-v0",
            "--target-platforms=//:platform3",
        )
    )

    await run(
        "uquery",
        "root//:foo",
        "-v0",
        "--output-format=starlark",
    )
    await run(
        "uquery",
        "root//:foo",
        "-v0",
        "--output-format=json",
        "-B",
    )

    await run("uquery", "root//:foo2", "-v0", "--output-format=starlark")
    await expect_failure(
        run(
            "uquery",
            "root//:foo2",
            "--console=none",
            "-v0",
            "--output-format=starlark",
            "-c",
            "user.check_bad_coercion=1",
        )
    )

    golden(
        output="\n".join(outputs),
        rel_path="golden/test_select_fail.golden.stderr",
    )
