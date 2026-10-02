# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


@buck_test()
async def test_package_file_package_values(buck: Buck) -> None:
    # Build file does all the assertions.
    output = await buck.build("//:")
    assert "TEST PASSED" in output.stderr


@buck_test()
async def test_audit_package_values(buck: Buck) -> None:
    stdout = (await buck.audit("package-values", "//")).stdout
    golden(
        output=stdout,
        rel_path="audit-package-values.golden.json",
    )


@buck_test()
async def test_audit_package_values_select(buck: Buck) -> None:
    stdout = (await buck.audit("package-values", "//")).stdout
    result = json.loads(stdout)
    pkg = result["root//"]
    # Verify select value has expected JSON structure
    assert pkg["sel.ector"] == {
        "__type": "selector",
        "entries": {"//config:a": "val_a", "DEFAULT": "default_val"},
    }
    # Verify concat (select + select) has expected JSON structure
    assert pkg["sel.concat"]["__type"] == "concat"
    assert len(pkg["sel.concat"]["items"]) == 2
    # Verify visibility fields exist alongside package values
    assert "visibility" in pkg
    assert "within_view" in pkg
    assert "visibility_cap" in pkg
    assert "within_view_cap" in pkg
    assert "visibility_intersection" in pkg


@buck_test()
async def test_audit_package_values_visibility_intersection(
    buck: Buck,
) -> None:
    stdout = (await buck.audit("package-values", "//intersection/child")).stdout
    result = json.loads(stdout)
    pkg = result["root//intersection/child"]
    # Legacy collapsed shape is kept for backwards compatibility.
    legacy = pkg["visibility_cap"]
    assert isinstance(legacy, dict), (
        f"Expected dict for intersection, got {type(legacy)}"
    )
    assert "intersection" in legacy
    assert len(legacy["intersection"]) == 2
    # New per-layer shape never collapses.
    layers = pkg["visibility_intersection"]
    assert len(layers) == 2
    assert layers[0]["patterns"] == ["root//intersection/..."]
    assert layers[0]["exempt_targets"] == []
    assert layers[1]["patterns"] == ["root//intersection/child/..."]
    assert layers[1]["exempt_targets"] == []


@buck_test()
async def test_audit_package_values_visibility_intersection_exempt(
    buck: Buck,
) -> None:
    stdout = (await buck.audit("package-values", "//intersection/exempt")).stdout
    result = json.loads(stdout)
    pkg = result["root//intersection/exempt"]
    intersection = pkg["visibility_intersection"]
    assert intersection == [
        {"patterns": ["root//intersection/..."], "exempt_targets": []},
        {
            "patterns": ["root//intersection/exempt/..."],
            "exempt_targets": ["root//intersection/exempt:"],
        },
    ], f"Unexpected exempt intersection shape: {intersection}"
    # Legacy collapsed shape ignores exemptions.
    legacy = pkg["visibility_cap"]
    assert legacy == {
        "intersection": [["root//intersection/..."], ["root//intersection/exempt/..."]],
    }, f"Unexpected legacy shape: {legacy}"


@buck_test()
async def test_targets_package_values(buck: Buck) -> None:
    stdout = (await buck.targets("--package-values", "//...")).stdout
    golden(
        output=stdout,
        rel_path="targets-package-values.golden.json",
    )


@buck_test()
async def test_targets_package_values_regex(buck: Buck) -> None:
    # Empty string as regex matches all keys.
    out = (await buck.targets("--package-values-regex", "", "//...")).stdout
    json_result = json.loads(out)[0]
    pv = json_result["buck.package_values"]
    assert pv["aaa.bbb"] == "ccc"
    assert pv["xxx.yyy"] == "zzz"
    assert pv["sel.ector"]["__type"] == "selector"
    assert pv["sel.concat"]["__type"] == "concat"

    out = (await buck.targets("--package-values-regex", "aaa.bbb", "//...")).stdout
    json_result = json.loads(out)[0]
    expected = {"aaa.bbb": "ccc"}
    assert json_result["buck.package_values"] == expected

    out = (await buck.targets("--package-values-regex", "xxx", "//...")).stdout
    json_result = json.loads(out)[0]
    expected = {"xxx.yyy": "zzz"}
    assert json_result["buck.package_values"] == expected

    out = (
        await buck.targets(
            "--package-values-regex",
            "aaa.bbb",
            "--package-values-regex",
            "xxx.yyy",
            "//...",
        )
    ).stdout
    json_result = json.loads(out)[0]
    expected = {"aaa.bbb": "ccc", "xxx.yyy": "zzz"}
    assert json_result["buck.package_values"] == expected

    # Regex matching select keys.
    out = (await buck.targets("--package-values-regex", "sel", "//...")).stdout
    json_result = json.loads(out)[0]
    pv = json_result["buck.package_values"]
    assert len(pv) == 2
    assert pv["sel.ector"]["__type"] == "selector"
    assert pv["sel.concat"]["__type"] == "concat"

    out = (await buck.targets("--package-values-regex", "non_existent", "//...")).stdout
    json_result = json.loads(out)[0]
    expected = {}
    assert json_result["buck.package_values"] == expected

    args = ["allow", "only", "one", "arg", "per", "flag", "occurrence"]
    await expect_failure(
        buck.targets("--package-values-regex", *args, "//..."),
        stderr_regex="Error parsing root//arg",
    )


@buck_test()
async def test_targets_streaming_package_values(buck: Buck) -> None:
    stdout = (await buck.targets("--streaming", "--package-values", "//...")).stdout
    golden(
        output=_sort_streaming_targets(stdout),
        rel_path="targets-streaming-package-values.golden.json",
    )


def _sort_streaming_targets(stdout: str) -> str:
    decoder = json.JSONDecoder()
    index = stdout.index("[") + 1
    entries: list[tuple[dict[str, object], str]] = []
    while True:
        index += len(stdout[index:]) - len(stdout[index:].lstrip(" \t\r\n,"))
        if stdout[index] == "]":
            break
        entry, end = decoder.raw_decode(stdout, index)
        entries.append((entry, stdout[index:end]))
        index = end

    ordered = sorted(
        entries,
        key=lambda entry: (entry[0]["buck.package"], entry[0]["name"]),
    )
    if not ordered:
        return "[]\n"
    return "[\n" + ",\n".join(f"  {raw}" for _, raw in ordered) + "\n]\n"
