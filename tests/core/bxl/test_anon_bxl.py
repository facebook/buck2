# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_define_anon_bxl(buck: Buck) -> None:
    await buck.bxl(
        "//anon_bxl.bxl:define_anon",
    )


@buck_test()
async def test_define_wrong_type_anon_bxl(buck: Buck) -> None:
    await expect_failure(
        buck.bxl("//wrong_type_anon_bxl.bxl:wrong_type"),
        stderr_regex="Type of parameter `impl` doesn't match,",
    )


@buck_test()
async def test_eval_anon_bxl(buck: Buck) -> None:
    await buck.bxl(
        "//anon_bxl.bxl:eval_anon_bxl",
    )


@buck_test()
async def test_check_anon_ouput_artifact(buck: Buck) -> None:
    await buck.bxl(
        "//anon_bxl.bxl:check_anon_ouput_artifact",
    )


@buck_test()
async def test_pass_string_to_arg_attr(buck: Buck) -> None:
    await buck.bxl("//anon_bxl.bxl:eval_of_anon_with_arg_bxl")


@buck_test(skip_for_os=["windows"])
async def test_content_based_output(buck: Buck) -> None:
    result = await buck.bxl(
        "//anon_bxl.bxl:eval_of_anon_with_content_based_output_impl"
    )
    # TODO(ianc) Support content-based paths in anon targets
    try:
        output_path = (buck.cwd / result.stdout.strip()).resolve()
        assert output_path.read_text() == "hello world"
        raise AssertionError(
            "This test currently fails because content-based paths are not yet supported"
        )
    except RuntimeError:
        pass
    except OSError:
        pass
