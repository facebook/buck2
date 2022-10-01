import tempfile

import pytest

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="bxl/simple")
@pytest.mark.parametrize(  # type: ignore
    "cmd",
    ["build", "targets", "cquery", "bxl", "uquery"],
)
async def test_write_uuid(buck: Buck, cmd: str) -> None:
    with tempfile.NamedTemporaryFile() as file:

        cmd_call = getattr(buck, cmd)
        await expect_failure(cmd_call("--write-build-id", file.name, "a"))

        assert len(file.read()) > 0


@buck_test(inplace=False, data_dir="bxl/simple")
@pytest.mark.parametrize(  # type: ignore
    "cmd",
    ["build", "targets", "cquery", "bxl", "uquery"],
)
async def test_ban_cell_override(buck: Buck, cmd: str) -> None:
    cmd_call = getattr(buck, cmd)
    await expect_failure(cmd_call("--config", "repositories.foo=bar", "a"))
