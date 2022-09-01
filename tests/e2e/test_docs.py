import json
import typing
from pathlib import Path

import pytest

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


"""
Tests to ensure that the `buck docs` command works as expected
"""


@buck_test(inplace=False, data_dir="docs")
async def test_docs_returns(buck: Buck) -> None:
    result = await buck.docs_starlark(query_patterns=[], builtins=False)
    result.check_returncode()
    decoded = json.loads(result.stdout)
    assert decoded == []


@buck_test(inplace=False, data_dir="docs")
async def test_builtin_docs(buck: Buck) -> None:
    result = await buck.docs_starlark(query_patterns=[], builtins=True)
    result.check_returncode()
    decoded = json.loads(result.stdout)
    builtins = next(
        x
        for x in decoded
        if x["id"]["name"] == "builtins" and x["id"]["location"] is None
    )
    assert builtins
    assert any(name for (name, _) in builtins["item"]["members"] if name == "map")


@buck_test(inplace=False, data_dir="docs")
async def test_prelude_docs(buck: Buck) -> None:
    result = await buck.docs_starlark(query_patterns=[], builtins=False, prelude=True)
    result.check_returncode()
    decoded = json.loads(result.stdout)
    native = next(
        x
        for x in decoded
        if x["id"]["name"] == "native" and x["id"]["location"] is not None
    )
    assert native
    assert any(name for (name, _) in native["item"]["members"] if name == "foo")

    foo = next(
        x
        for x in decoded
        if x["id"]["name"] == "foo" and x["id"]["location"] is not None
    )
    assert foo


EXPECTED: typing.Dict[str, object] = {
    "root//cell/dir:defs.bzl": {
        "id": {
            "location": {"path": "root//cell/dir:defs.bzl", "position": None},
            "name": "root//cell/dir:defs.bzl",
        },
        "item": {
            "kind": "module",
            "docs": {
                "details": "And these are its details",
                "summary": "This is the summary for the module",
            },
        },
        "custom_attrs": {},
    },
    "root//cell/dir:defs.bzl:bar": {
        "id": {
            "location": {"path": "root//cell/dir:defs.bzl", "position": None},
            "name": "bar",
        },
        "item": {
            "docs": {
                "details": "These are the details that go below.\n"
                "We'll query for this symbol, and verify it "
                "matches json as expected\n"
                "Don't document 'd'",
                "summary": "This is the summary line for 'bar'",
            },
            "kind": "function",
            "params": [
                {
                    "default_value": None,
                    "docs": {"details": None, "summary": "Docs for a"},
                    "kind": "arg",
                    "name": "a",
                    "type": None,
                },
                {
                    "default_value": None,
                    "docs": {"details": None, "summary": "Docs for b"},
                    "kind": "arg",
                    "name": "b",
                    "type": {"raw_type": '"string"'},
                },
                {"kind": "no_args"},
                {
                    "default_value": None,
                    "docs": {"details": None, "summary": "Docs for c"},
                    "kind": "arg",
                    "name": "c",
                    "type": {"raw_type": '"string"'},
                },
                {
                    "default_value": '"some_default"',
                    "docs": None,
                    "kind": "arg",
                    "name": "d",
                    "type": {"raw_type": '"string"'},
                },
            ],
            "ret": {"docs": None, "type": {"raw_type": '["string"]'}},
        },
        "custom_attrs": {},
    },
    "root//cell/dir:defs.bzl:baz": {
        "id": {
            "location": {"path": "root//cell/dir:defs.bzl", "position": None},
            "name": "baz",
        },
        "item": {
            "docs": {"details": None, "summary": "Simple docstring for baz"},
            "kind": "function",
            "params": [],
            "ret": {"docs": None, "type": None},
        },
        "custom_attrs": {},
    },
    "root//cell/dir:defs.bzl:quz": {
        "id": {
            "location": {"path": "root//cell/dir:defs.bzl", "position": None},
            "name": "quz",
        },
        "item": {
            "docs": None,
            "kind": "function",
            "params": [],
            "ret": {"docs": None, "type": None},
        },
        "custom_attrs": {},
    },
    "cell//dir:defs.bzl": {
        "id": {
            "location": {"path": "cell//dir:defs.bzl", "position": None},
            "name": "cell//dir:defs.bzl",
        },
        "item": {
            "kind": "module",
            "docs": {
                "details": "And these are its details",
                "summary": "This is the summary for the module",
            },
        },
        "custom_attrs": {},
    },
    "cell//dir:defs.bzl:bar": {
        "id": {
            "location": {"path": "cell//dir:defs.bzl", "position": None},
            "name": "bar",
        },
        "item": {
            "docs": {
                "details": "These are the details that go below.\n"
                "We'll query for this symbol, and verify it "
                "matches json as expected\n"
                "Don't document 'd'",
                "summary": "This is the summary line for 'bar'",
            },
            "kind": "function",
            "params": [
                {
                    "default_value": None,
                    "docs": {"details": None, "summary": "Docs for a"},
                    "kind": "arg",
                    "name": "a",
                    "type": None,
                },
                {
                    "default_value": None,
                    "docs": {"details": None, "summary": "Docs for b"},
                    "kind": "arg",
                    "name": "b",
                    "type": {"raw_type": '"string"'},
                },
                {"kind": "no_args"},
                {
                    "default_value": None,
                    "docs": {"details": None, "summary": "Docs for c"},
                    "kind": "arg",
                    "name": "c",
                    "type": {"raw_type": '"string"'},
                },
                {
                    "default_value": '"some_default"',
                    "docs": None,
                    "kind": "arg",
                    "name": "d",
                    "type": {"raw_type": '"string"'},
                },
            ],
            "ret": {"docs": None, "type": {"raw_type": '["string"]'}},
        },
        "custom_attrs": {},
    },
    "cell//dir:defs.bzl:baz": {
        "id": {
            "location": {"path": "cell//dir:defs.bzl", "position": None},
            "name": "baz",
        },
        "item": {
            "docs": {"details": None, "summary": "Simple docstring for baz"},
            "kind": "function",
            "params": [],
            "ret": {"docs": None, "type": None},
        },
        "custom_attrs": {},
    },
    "cell//dir:defs.bzl:quz": {
        "id": {
            "location": {"path": "cell//dir:defs.bzl", "position": None},
            "name": "quz",
        },
        "item": {
            "docs": None,
            "kind": "function",
            "params": [],
            "ret": {"docs": None, "type": None},
        },
        "custom_attrs": {},
    },
}


@buck_test(inplace=False, data_dir="docs")
async def test_docs_in_starlark(buck: Buck) -> None:
    result = await buck.docs_starlark(
        query_patterns=[
            "root//cell/dir:defs.bzl",
            "//cell/dir:defs.bzl",
            "cell//dir:defs.bzl",
        ],
        builtins=False,
    )
    result.check_returncode()
    decoded = json.loads(result.stdout)

    print(result.stdout)

    assert EXPECTED["root//cell/dir:defs.bzl"] in decoded
    assert EXPECTED["root//cell/dir:defs.bzl:bar"] in decoded
    assert EXPECTED["root//cell/dir:defs.bzl:baz"] in decoded
    assert EXPECTED["root//cell/dir:defs.bzl:quz"] in decoded

    assert EXPECTED["cell//dir:defs.bzl"] in decoded
    assert EXPECTED["cell//dir:defs.bzl:bar"] in decoded
    assert EXPECTED["cell//dir:defs.bzl:baz"] in decoded
    assert EXPECTED["cell//dir:defs.bzl:quz"] in decoded

    assert len(decoded) == 8


@buck_test(inplace=False, data_dir="docs")
async def test_docs_in_starlark_within_cell(buck: Buck) -> None:
    result = await buck.docs_starlark(
        query_patterns=["//dir:defs.bzl"], builtins=False, rel_cwd=Path("cell")
    )
    result.check_returncode()
    decoded = json.loads(result.stdout)

    assert EXPECTED["cell//dir:defs.bzl"] in decoded
    assert EXPECTED["cell//dir:defs.bzl:bar"] in decoded
    assert EXPECTED["cell//dir:defs.bzl:baz"] in decoded
    assert EXPECTED["cell//dir:defs.bzl:quz"] in decoded
    assert len(decoded) == 4


@buck_test(inplace=False, data_dir="docs")
async def test_docs_in_starlark_with_relative_path(buck: Buck) -> None:
    result = await buck.docs_starlark(
        query_patterns=[":defs.bzl"], builtins=False, rel_cwd=Path("cell/dir")
    )
    result.check_returncode()
    decoded = json.loads(result.stdout)

    assert EXPECTED["cell//dir:defs.bzl"] in decoded

    assert EXPECTED["cell//dir:defs.bzl:bar"] in decoded
    assert EXPECTED["cell//dir:defs.bzl:baz"] in decoded
    assert EXPECTED["cell//dir:defs.bzl:quz"] in decoded
    assert len(decoded) == 4


@pytest.mark.xfail(reason="until we ban non .bzl paths, this would be valid")
@buck_test(inplace=False, data_dir="docs")
async def test_docs_fail_with_invalid_patterns(buck: Buck) -> None:
    await expect_failure(
        buck.docs_starlark(query_patterns=["not_an_import_path"]),
        stderr_regex="Expected a cell path to a `.bzl` file, but got `root//not_an_import_path`",
    )
    await expect_failure(
        buck.docs_starlark(query_patterns=["//cell"]),
        stderr_regex="Expected a cell path to a `.bzl` file, but got `root//cell`",
    )


@buck_test(inplace=False, data_dir="docs")
async def test_providers_print_docs(buck: Buck) -> None:
    def expected_object(
        name: str, summary: bool, details: bool, doc_members: bool
    ) -> typing.Dict[str, object]:
        if summary:
            docs: typing.Dict[str, object] = {"summary": f"Summary for {name}"}
            if details:
                docs["details"] = f"Details for {name}"
            else:
                docs["details"] = None
        else:
            docs = None
        ret: typing.Dict[str, object] = {
            "id": {
                "name": name,
                "location": {"path": "root//:providers.bzl", "position": None},
            },
            "item": {
                "kind": "object",
                "docs": docs,
                "members": [
                    ["a", {"kind": "property", "docs": None, "type": None}],
                    [
                        "b",
                        {
                            "kind": "property",
                            "docs": {"summary": "b summary", "details": None},
                            "type": None,
                        },
                    ],
                    [
                        "c",
                        {
                            "kind": "property",
                            "docs": {
                                "summary": "c summary",
                                "details": "And c details",
                            },
                            "type": None,
                        },
                    ],
                ]
                if doc_members
                else [
                    ["a", {"kind": "property", "docs": None, "type": None}],
                    ["b", {"kind": "property", "docs": None, "type": None}],
                    ["c", {"kind": "property", "docs": None, "type": None}],
                ],
            },
            "custom_attrs": {},
        }
        return ret

    expected = [
        expected_object("SimpleNoDocstringInfo", False, False, False),
        expected_object(
            "SimpleSummaryInfo",
            True,
            False,
            False,
        ),
        expected_object(
            "SimpleDocumentedInfo",
            True,
            True,
            False,
        ),
        expected_object("NoDocstringInfo", False, False, True),
        expected_object(
            "SummaryInfo",
            True,
            False,
            True,
        ),
        expected_object(
            "FullyDocumentedInfo",
            True,
            True,
            True,
        ),
    ]

    result = await buck.docs_starlark(
        query_patterns=["root//:providers.bzl"], builtins=False
    )
    result.check_returncode()
    decoded = json.loads(result.stdout)

    for doc in expected:
        assert doc in decoded
    assert len(decoded) == 6
