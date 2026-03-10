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


@buck_test()
async def test_select_incompatible(buck: Buck) -> None:
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

    # Compatible platform: constraint1 matches, target resolves normally.
    await run("cquery", "root//:incompat_foo", "-v0", "--target-platforms=//:platform1")

    # Incompatible platform: constraint2 matches select_incompatible branch.
    # Target should be skipped (empty output), not error.
    await run(
        "cquery",
        "root//:incompat_foo",
        "-v0",
        "--target-platforms=//:platform2",
    )

    # Default branch is select_incompatible: target should be skipped.
    await run(
        "cquery",
        "root//:incompat_foo",
        "-v0",
        "--target-platforms=//:platform3",
    )

    # Build with incompatible platform and explicit target: fails with incompatible message
    # (same as target_compatible_with behavior for explicit targets).
    await expect_failure(
        run(
            "build",
            "root//:incompat_foo",
            "--console=none",
            "-v0",
            "--target-platforms=//:platform2",
        )
    )

    # Build with --skip-incompatible-targets: should skip gracefully.
    await run(
        "build",
        "root//:incompat_foo",
        "-v0",
        "--target-platforms=//:platform2",
        "--skip-incompatible-targets",
    )

    # uquery starlark output: select_incompatible values preserved.
    await run(
        "uquery",
        "root//:incompat_foo",
        "-v0",
        "--output-format=starlark",
    )

    # uquery json output: select_incompatible serialized.
    await run(
        "uquery",
        "root//:incompat_foo",
        "-v0",
        "--output-format=json",
        "-B",
    )

    # select_incompatible used outside select is a coercion error (same as select_fail).
    await run("uquery", "root//:incompat_foo2", "-v0", "--output-format=starlark")
    await expect_failure(
        run(
            "uquery",
            "root//:incompat_foo2",
            "--console=none",
            "-v0",
            "--output-format=starlark",
            "-c",
            "user.check_bad_coercion_incompatible=1",
        )
    )

    # Dep propagation: depends_on_incompat depends on incompat_foo.
    # On platform1 (compatible), both targets resolve normally.
    await run(
        "cquery",
        "root//:depends_on_incompat",
        "-v0",
        "--target-platforms=//:platform1",
    )

    # On platform2 (incompatible), incompat_foo is incompatible,
    # so depends_on_incompat should also be incompatible (dep-only).
    # This is currently a soft error upgraded to hard error via BUCK2_HARD_ERROR.
    await expect_failure(
        run(
            "cquery",
            "root//:depends_on_incompat",
            "--console=none",
            "-v0",
            "--target-platforms=//:platform2",
        )
    )

    golden(
        output="\n".join(outputs),
        rel_path="golden/test_select_incompatible.golden.stderr",
    )
