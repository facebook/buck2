# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(
    extra_buck_config={
        "test": {
            "foo": "bar",
        }
    },
)
async def test_extra_buck_config(buck: Buck) -> None:
    """
    Assert that our testing framework works as expected.
    """

    cfg = (await buck.audit_config("--style=json")).get_json()
    assert cfg.get("test.foo") == "bar"


@buck_test()
async def test_audit_config_json(buck: Buck) -> None:
    result = await buck.audit_config("--style=json")
    result_json = result.get_json()
    assert result_json is not None


@buck_test()
async def test_audit_config_cell_json(buck: Buck) -> None:
    out = await buck.audit_config(
        "--style",
        "json",
    )
    out_json = out.get_json() or {}
    assert out_json.get("test.is_root") == "yes"
    assert out_json.get("test.is_code") is None

    out = await buck.audit_config("--style", "json", "--cell", "code")
    out_json = out.get_json() or {}
    assert out_json.get("test.is_code") == "yes"
    assert out_json.get("test.is_root") is None

    out = await buck.audit_config(
        "--style",
        "json",
        rel_cwd=Path("code"),
    )
    out_json = out.get_json() or {}
    assert out_json.get("test.is_code") == "yes"
    assert out_json.get("test.is_root") is None


@buck_test(setup_eden=True)
async def test_audit_config_all_cells(buck: Buck) -> None:
    out = await buck.audit_config(
        "--all-cells",
        "--style",
        "json",
    )
    out_json = out.get_json() or {}
    print(out_json)
    assert out_json.get("code//bar.a") == "2"
    assert out_json.get("source//bar.a") == "1"
    assert out_json.get("root//bar.a") == "1"
    assert out_json.get("b//bar.a") is None

    out = await buck.audit_config(
        "--all-cells",
        "--style",
        "json",
        "code//bar.a",
    )
    out_json = out.get_json() or {}
    assert out_json.get("code//bar.a") == "2"
    assert out_json.get("source//bar.a") is None

    out = await buck.audit_config(
        "--all-cells",
    )
    assert "# Cell: source\n[bar]\n    a = 1\n" in out.stdout


@buck_test()
async def test_audit_config_with_config_value(buck: Buck) -> None:
    result_config = await buck.audit_config(
        "python",
        "--style",
        "json",
        "-cpython.helpers=true",
    )
    result_config_json = result_config.get_json()

    assert result_config_json.get("python.helpers") == "true"


@buck_test()
async def test_audit_config_with_config_file(buck: Buck, tmp_path: Path) -> None:
    configfile = tmp_path / "config.bcfg"
    configfile.write_text("[python]\n  helpers = true\n")

    result_file = await buck.audit_config(
        "--config-file",
        str(configfile),
        "--style",
        "json",
    )

    assert result_file.get_json().get("python.helpers") == "true"


@buck_test()
async def test_audit_config_location_extended(buck: Buck) -> None:
    result = await buck.audit_config(
        "bar.a",
        "--location=extended",
    )
    assert "a = 1" in result.stdout
    assert "included.bcfg:2" in result.stdout


@buck_test()
async def test_audit_config_with_cell_syntax(buck: Buck) -> None:
    result_file = await buck.audit_config(
        "code//test.is_code",
        "--style",
        "json",
    )
    result_file_json = result_file.get_json()

    assert result_file_json.get("code//test.is_code") == "yes"


@buck_test()
async def test_cell_relative_configs(buck: Buck) -> None:
    result_root_cell = await buck.audit_config(
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
