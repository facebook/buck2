# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


class InvalidFileNamesTest(BuckTestCase):
    @buck_test(skip_for_os=["windows"], setup_eden=True)
    async def test_invalid_file_invalidation(self, buck: Buck) -> None:
        """
        This is a regression test for T136963408.
        """

        await buck.build(":root")

        src = buck.cwd / "src"
        invalid = src / "\\"
        invalid_nested = src / "\\" / "a"
        invalid_nested_invalid = src / "\\" / "\\"

        # Create an invalid file. Build should work.
        invalid.touch()
        output = await buck.build(":root")
        assert "is not valid, the entry is ignored" in output.stderr

        # Delete it, build should work.
        invalid.unlink()
        await buck.build(":root")

        # Create an invalid dir. Build should still work.
        invalid_nested.mkdir(parents=True)
        output = await buck.build(":root")
        assert "is not valid, the entry is ignored" in output.stderr

        # And delete it. Things should work.
        invalid_nested.rmdir()
        invalid.rmdir()
        await buck.build(":root")

        # Finally, do an invalid file inside an invalid dir...
        invalid_nested_invalid.mkdir(parents=True)
        output = await buck.build(":root")
        assert "is not valid, the entry is ignored" in output.stderr

        # And delete it. Things should again.
        invalid_nested_invalid.rmdir()
        invalid.rmdir()
        await buck.build(":root")
