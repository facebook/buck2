import re
import textwrap
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_audit_config_json(buck: Buck) -> None:
    result = await buck.audit_config("--style=json")
    result_json = result.get_json()
    assert result_json is not None


@buck_test(inplace=True)
async def test_audit_configurations(buck: Buck) -> None:
    await buck.cquery("fbcode//buck2/tests/targets/audit/configurations:target")

    result = await buck.audit_configurations()

    expected = textwrap.dedent(
        """
fbcode//buck2/tests/targets/audit/configurations:platform1-[0-9a-f]*:
  fbcode//buck2/tests/targets/audit/configurations:constraint1 \\(fbcode//buck2/tests/targets/audit/configurations:setting1\\)
  fbcode//buck2/tests/targets/audit/configurations:constraint2 \\(fbcode//buck2/tests/targets/audit/configurations:setting2\\)
  fbcode//buck2/tests/targets/audit/configurations:constraint3 \\(fbcode//buck2/tests/targets/audit/configurations:setting3\\)"""
    )

    assert re.search(
        expected, result.stdout
    ), "expected output to contain:\n%s\n actual output:\n%s" % (
        expected,
        result.stdout,
    )


@buck_test(inplace=True)
async def test_audit_config_with_config_value(buck: Buck) -> None:
    result_config = await buck.audit_config(
        "python",
        "--style",
        "json",
        "-cpython.helpers=true",
    )
    result_config_json = result_config.get_json()
    result_override = await buck.audit_config(
        "python",
        "--style",
        "json",
        "--config",
        "python.check_srcs_ext=",
    )
    result_override_json = result_override.get_json()

    assert result_config_json is not None
    assert result_override_json is not None

    assert result_config_json.get("python.helpers") == "true"
    assert result_override_json.get("python.check_srcs_ext") is None


@buck_test(inplace=True)
async def test_audit_config_with_config_file(buck: Buck) -> None:
    result_file = await buck.audit_config(
        "@fbcode//mode/opt",
        "project.buck_out",
        "--style",
        "json",
    )
    result_file_json = result_file.get_json()

    assert result_file_json is not None
    assert result_file_json.get("project.buck_out") == "buck-out/opt"


@buck_test(inplace=True)
async def test_audit_config_executable_argfile(buck: Buck) -> None:
    result = await buck.audit_config(
        "@buck2/tests/targets/configurations_uncategorized/executable_argfiles/test_ex_argfile.py#iphonesimulator-x86_64",
        "cxx",
        "apple",
        "--style",
        "json",
    )
    result_json = result.get_json()

    assert result_json is not None
    assert result_json.get("cxx.default_platform") == "iphonesimulator-x86_64"
    assert result_json.get("apple.xctool_zip_target") is None
    assert result_json.get("apple.xctool_path") == "/usr/bin/true"


@buck_test(inplace=True)
async def test_audit_config_location_extended(buck: Buck) -> None:
    result = await buck.audit_config(
        "@fbcode//buck2/tests/targets/configurations_uncategorized/executable_argfiles/jackalope",
        "apple.xctool_path",
        "--location=extended",
    )
    assert "xctool_path = /usr/bin/true" in result.stdout
    assert (
        "buck2/tests/targets/configurations_uncategorized/executable_argfiles/jackalope-apple-toolchain.bcfg:2"
        in result.stdout
    )


@buck_test(inplace=True)
async def test_audit_config_gets_correct_cell_from_cwd(buck: Buck) -> None:
    result = await buck.audit_config("repositories.fbcode", "--style=json")
    assert result.get_json() == {"repositories.fbcode": "."}

    result = await buck.audit_config(
        "repositories.fbsource",
        "--style=json",
        rel_cwd=Path(".."),
    )
    assert result.get_json() == {"repositories.fbsource": "."}


@buck_test(inplace=False, data_dir="cells")
async def test_cell_relative_configs(buck: Buck) -> None:
    result_root_cell = await buck.audit_config(
        "foo",
        "--config",
        "root//bar.a=5",
        "--style",
        "json",
    )
    result_root_cell_json = result_root_cell.get_json()

    assert result_root_cell_json is not None
    assert result_root_cell_json.get("foo.b") == "5"

    result_nonroot_cell = await buck.audit_config(
        "foo",
        "--config",
        "code//bar.a=5",
        "--style",
        "json",
        "--cell",
        "code",
    )
    result_nonroot_cell_json = result_nonroot_cell.get_json()

    assert result_nonroot_cell_json is not None
    assert result_nonroot_cell_json.get("foo.b") == "5"

    result_diff_cell = await buck.audit_config(
        "foo",
        "--config",
        "code//bar.a=5",
        "--style",
        "json",
        "--cell",
        "source",
    )
    result_diff_cell_json = result_diff_cell.get_json()

    assert result_diff_cell_json is not None
    assert result_diff_cell_json.get("foo.b") == "1"

    result_all_cell = await buck.audit_config(
        "foo",
        "--config",
        "bar.a=5",
        "--style",
        "json",
        "--cell",
        "source",
    )
    result_all_cell_json = result_all_cell.get_json()

    assert result_all_cell_json is not None
    assert result_all_cell_json.get("foo.b") == "5"
