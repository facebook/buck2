import re
import tempfile
import textwrap
from pathlib import Path

import pytest

from buck2.tests.e2e.helper.assert_occurrences import assert_occurrences
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
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
async def test_audit_config_argfile_outside_repo(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile(mode="w") as argfile:
        argfile.write("@fbcode//mode/opt")
        argfile.flush()
        result_file = await buck.audit_config(
            f"@{argfile.name}",
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
async def test_audit_config_stdin_argfile_simple(buck: Buck) -> None:
    result_file = await buck.audit_config(
        "--style=json",
        "@-",
        input="\n".join(
            [
                "@fbcode//mode/opt",
                "project.buck_out",
            ]
        ).encode(),
    )
    result_file_json = result_file.get_json()

    assert result_file_json is not None
    assert result_file_json.get("project.buck_out") == "buck-out/opt"


@buck_test(inplace=True)
async def test_audit_config_stdin_argfile_cell_from_cwd(buck: Buck) -> None:
    result_file = await buck.audit_config(
        "--style=json",
        "@-",
        input="\n".join(
            [
                # Should resolve to `fbcode//mode/opt` because
                # the cwd is `fbcode/buck2`.
                "@//mode/opt",
                "project.buck_out",
            ]
        ).encode(),
        rel_cwd=Path("buck2"),
    )
    result_file_json = result_file.get_json()

    assert result_file_json is not None
    assert result_file_json.get("project.buck_out") == "buck-out/opt"


@buck_test(inplace=True)
async def test_audit_config_location_extended(buck: Buck) -> None:
    result = await buck.audit_config(
        "@fbcode//buck2/tests/targets/configurations_uncategorized/executable_argfiles/jackalope",
        "apple.xctool_path",
        "--location=extended",
    )
    assert "xctool_path = /usr/bin/true" in result.stdout
    assert (
        str(
            Path("buck2")
            / "tests"
            / "targets"
            / "configurations_uncategorized"
            / "executable_argfiles"
            / "jackalope-apple-toolchain.bcfg:2"
        )
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


@buck_test(inplace=True)
async def test_audit_config_with_cell_syntax(buck: Buck) -> None:
    result_file = await buck.audit_config(
        "fbcode//project.buck_out",
        "--style",
        "json",
    )
    result_file_json = result_file.get_json()

    assert result_file_json is not None
    assert result_file_json.get("fbcode//project.buck_out") == "buck-out/dev"


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


@buck_test(inplace=False, data_dir="visibility")
@pytest.mark.parametrize(
    "rule, passes",
    [
        ("self//:pass1", True),
        ("self//:pass2", True),
        ("self//:pass3", True),
        ("self//:pass4", True),
        ("self//:fail1", False),
        ("self//:fail2", False),
        ("self//:fail3", False),
        ("self//:fail4", False),
        ("self//:fail5", False),
        ("self//:fail6", False),
    ],
)
async def test_audit_visibility(buck: Buck, rule: str, passes: bool) -> None:
    if passes:
        out = await buck.audit_visibility(rule)
        assert out.stdout == ""
    else:
        await expect_failure(
            buck.audit_visibility(rule),
            stderr_regex=f"not visible to `{rule}`",
        )


@buck_test(inplace=True)
async def test_reuse_current_config_warnings(buck: Buck) -> None:
    res = await buck.audit_config(
        "@fbcode//mode/opt",
        "--config",
        "foo.bar=foobar",
        "--config-file",
        "fbsource//tools/buckconfigs/fbcode/modes/dev.bcfg",
        "project.buck_out",
        "--reuse-current-config",
    )

    no_previous_invocation_detected = "no previous invocation detected"
    default_to_use_current_config = "using current config instead"
    opt_override = "fbsource//tools/buckconfigs/fbcode/modes/opt.bcfg"
    dev_override = "fbsource//tools/buckconfigs/fbcode/modes/dev.bcfg"
    foobar_override = "foo.bar=foobar"
    assert_occurrences(no_previous_invocation_detected, res.stderr, 1)
    assert_occurrences(default_to_use_current_config, res.stderr, 1)
    assert_occurrences(opt_override, res.stderr, 1)
    assert_occurrences(dev_override, res.stderr, 1)
    assert_occurrences(foobar_override, res.stderr, 1)


@buck_test(inplace=True)
async def test_reuse_current_config_no_previous_config(buck: Buck) -> None:
    result_file = await buck.audit_config(
        "project.buck_out",
        "--style",
        "json",
        "--reuse-current-config",
    )
    result_file_json = result_file.get_json()

    assert result_file_json is not None
    assert result_file_json.get("project.buck_out") == "buck-out/dev"


@buck_test(inplace=True)
async def test_reuse_current_config(buck: Buck) -> None:
    result_file = await buck.audit_config(
        "@fbcode//mode/opt",
        "project.buck_out",
        "--style",
        "json",
    )
    result_file_json = result_file.get_json()

    assert result_file_json is not None
    assert result_file_json.get("project.buck_out") == "buck-out/opt"

    result_file = await buck.audit_config(
        "project.buck_out",
        "--style",
        "json",
        "--reuse-current-config",
    )
    result_file_json = result_file.get_json()

    assert result_file_json is not None
    # assert it does not revert back to @mode/dev by default
    assert result_file_json.get("project.buck_out") == "buck-out/opt"
