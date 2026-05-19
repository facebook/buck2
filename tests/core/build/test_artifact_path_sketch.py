# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden
from buck2.tests.e2e_util.helper.utils import replace_digest, replace_hash


def _sanitize_timing_fields(obj: Any) -> None:
    """Replace timing-dependent fields with a placeholder to avoid flaky tests."""
    if isinstance(obj, dict):
        for key in list(obj.keys()):
            if key == "compute_time_ms":
                obj[key] = "<COMPUTE_TIME_MS>"
            else:
                _sanitize_timing_fields(obj[key])
    elif isinstance(obj, list):
        for item in obj:
            _sanitize_timing_fields(item)


def build_report_test(name: str, command: list[str]) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "build-report.json"
        await buck.build("--build-report", str(report), *command)

        with open(report) as file:
            report = json.loads(file.read())
        del report["trace_id"]
        del report["project_root"]
        _sanitize_timing_fields(report)

        golden(
            output=replace_digest(
                replace_hash(json.dumps(report, indent=2, sort_keys=True))
            ),
            rel_path="fixtures/" + name + ".golden.json",
        )

    globals()[name] = impl

    return buck_test()(impl)


async def _get_sketch_cardinalities_from_report(
    buck: Buck,
    target: str,
    extra_args: list[str] | None = None,
    report_key: str | None = None,
) -> tuple[float | None, float | None]:
    report_path = buck.cwd / "build-report.json"
    args = [
        target,
        "--build-report",
        str(report_path),
        "-c",
        "buck2.log_artifact_count_sketch=true",
        "-c",
        "buck2.log_artifact_size_sketch=true",
        "-c",
        "buck2.log_sketch_cardinalities=true",
    ]
    if extra_args:
        args.extend(extra_args)
    await buck.build(*args)

    key = report_key or target
    with open(report_path) as f:
        report = json.load(f)
    configured = report["results"][key]["configured"]
    entry = configured[list(configured.keys())[0]]
    return (
        entry.get("artifact_count_sketch_cardinality"),
        entry.get("artifact_size_sketch_cardinality"),
    )


build_report_test(
    "test_artifact_count_sketch_format",
    [
        "//:simple",
        "-c",
        "buck2.log_artifact_count_sketch=true",
    ],
)

build_report_test(
    "test_artifact_size_sketch_format",
    [
        "//:simple",
        "-c",
        "buck2.log_artifact_size_sketch=true",
    ],
)

build_report_test(
    "test_both_artifact_sketches_format",
    [
        "//:simple",
        "//:with_dep",
        "-c",
        "buck2.log_artifact_count_sketch=true",
        "-c",
        "buck2.log_artifact_size_sketch=true",
    ],
)


@buck_test()
async def test_estimated_single_target(buck: Buck) -> None:
    count, size = await _get_sketch_cardinalities_from_report(buck, "root//:simple")
    # simple_write: 1 immediate provider output (out.txt). Distinct paths = 1.
    assert count is not None
    assert count == pytest.approx(1.0, abs=0.5)
    # Size sketch estimates total bytes: "content" = 7 bytes
    assert size is not None
    assert size == pytest.approx(7.0, abs=1.5)


@buck_test()
async def test_estimated_with_dep(buck: Buck) -> None:
    count, size = await _get_sketch_cardinalities_from_report(buck, "root//:with_dep")
    # copy_dep target has 1 immediate provider output (out.txt). The base dep
    # is intentionally not counted because we only sketch immediate provider
    # outputs, not the action graph.
    # Distinct paths: {with_dep_out} = 1
    assert count is not None
    assert count == pytest.approx(1.0, abs=0.5)
    # Size: with_dep's out.txt is a copy of base's out.txt = "content" (7 bytes)
    assert size is not None
    assert size == pytest.approx(7.0, abs=1.5)


@buck_test()
async def test_estimated_content_based_paths(buck: Buck) -> None:
    count, size = await _get_sketch_cardinalities_from_report(
        buck, "root//:content_based"
    )
    # content_based_write: 1 output resolved via resolve_path with the content
    # hash from the ArtifactValue, so the sketched path is the real on-disk
    # content-hashed path. The ArtifactValue has no extra deps for a single-file
    # content-based write.
    # Distinct paths: 1
    assert count is not None
    assert count == pytest.approx(1.0, abs=0.5)
    # Size: "content" = 7 bytes
    assert size is not None
    assert size == pytest.approx(7.0, abs=1.5)


@buck_test()
async def test_estimated_projected_artifacts(buck: Buck) -> None:
    count, size = await _get_sketch_cardinalities_from_report(
        buck, "root//:projected_target"
    )
    # projected_output rule: default output is out_dir.project("a"). The
    # projected artifact resolves to a single sub-path (out_dir/a).
    # Distinct paths: {out_dir/a} = 1
    assert count is not None
    assert count == pytest.approx(1.0, abs=0.5)
    # Size: out_dir/a contains "a\n" from `echo a` = 2 bytes
    assert size is not None
    assert size == pytest.approx(2.0, abs=1.5)


@buck_test()
async def test_estimated_symlink_file(buck: Buck) -> None:
    count, size = await _get_sketch_cardinalities_from_report(
        buck, "root//:symlink_target"
    )
    # symlink_rule: immediate provider output is link.txt (a Symlink leaf entry).
    # The ArtifactValue's `deps` tree carries the symlink target (src.txt) at
    # its project-relative path because `add_symlinked` merged the source value
    # in at action-construction time.
    # Distinct paths: {link.txt, src.txt} = 2
    assert count is not None
    assert count == pytest.approx(2.0, abs=0.5)
    # Size: src.txt contents ("source_content" = 14 bytes) + link.txt symlink
    # target string (a project-relative path; ~10 bytes for "../src.txt").
    assert size is not None
    assert size == pytest.approx(24.0, abs=5.0)


@buck_test()
async def test_estimated_symlinked_dir(buck: Buck) -> None:
    count, size = await _get_sketch_cardinalities_from_report(
        buck, "root//:symlinked_dir_target"
    )
    # symlinked_dir_rule: immediate provider output is out_dir, a directory
    # whose entries are symlinks. Walking the entry yields out_dir/file1 and
    # out_dir/file2. The ArtifactValue's `deps` tree carries the underlying
    # source artifacts (src1.txt, src2.txt) because each symlink contributes
    # its source value's deps.
    # Distinct paths: {out_dir/file1, out_dir/file2, src1.txt, src2.txt} = 4
    assert count is not None
    assert count == pytest.approx(4.0, abs=0.5)
    # Size: src1.txt + src2.txt ("content1"/"content2" = 8 bytes each) + two
    # symlink target strings (~14 bytes each for "../../srcN.txt").
    assert size is not None
    assert size == pytest.approx(44.0, abs=6.0)
