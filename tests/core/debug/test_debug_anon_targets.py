# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden_replace_cfg_hash


@buck_test()
async def test_anon_targets_json(buck: Buck) -> None:
    result = await buck.debug("anon-targets", "//:root", "--json", "--with-requesters")
    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="anon_targets_json_with_requesters.golden.json",
    )
    entries = json.loads(result.stdout)

    # `//:with_anon` requests `_anon_mid`, which in turn requests `_anon_leaf`.
    assert len(entries) == 2, entries
    # Entries are keyed by the anon target's `BaseDeferredKey` rendering, which each
    # entry also carries in `buck.key`.
    for key, entry in entries.items():
        assert entry["buck.key"] == key, entries
        assert "(anon: " in key, entries
    by_rule = {entry["buck.type"].split(":")[-1]: entry for entry in entries.values()}
    assert sorted(by_rule) == ["_anon_leaf", "_anon_mid"], entries

    mid = by_rule["_anon_mid"]
    assert mid["buck.variant"] == "bzl"
    assert mid["level"] == 1
    assert any("with_anon" in requester for requester in mid["buck.requested_by"]), mid

    # The leaf was requested by the mid anon target, not by any configured target.
    leaf = by_rule["_anon_leaf"]
    assert leaf["suffix"] == "from-mid"
    assert any("_anon_mid" in requester for requester in leaf["buck.requested_by"]), (
        leaf
    )
    assert not any(
        "with_anon" in requester for requester in leaf["buck.requested_by"]
    ), leaf


@buck_test()
async def test_anon_targets_output_attribute(buck: Buck) -> None:
    # `-a` filters attributes with regexes in search mode and implies JSON.
    result = await buck.debug("anon-targets", "//:root", "-a", "^level$")
    entries = json.loads(result.stdout)
    assert len(entries) == 2, entries
    attr_sets = sorted(tuple(sorted(entry)) for entry in entries.values())
    assert attr_sets == [(), ("level",)], entries


@buck_test()
async def test_anon_targets_basic_attributes(buck: Buck) -> None:
    # `-B` selects the user-suppliable attrs plus `buck.package` and `buck.type`.
    result = await buck.debug("anon-targets", "//:root", "-B")
    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="anon_targets_basic_attributes.golden.json",
    )
    entries = json.loads(result.stdout)
    assert len(entries) == 2, entries
    for entry in entries.values():
        assert "buck.type" in entry, entry
        assert "buck.package" in entry, entry
        assert "buck.execution_configuration" not in entry, entry
    assert any("level" in entry for entry in entries.values()), entries
    assert any("suffix" in entry for entry in entries.values()), entries


@buck_test()
async def test_anon_targets_text(buck: Buck) -> None:
    result = await buck.debug("anon-targets", "//:root")
    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="anon_targets_text.golden.txt",
    )
    assert "_anon_mid" in result.stdout
    assert "_anon_leaf" in result.stdout
    # Entries are headed by the anon target's `BaseDeferredKey` rendering.
    assert "(anon: " in result.stdout
    # Without --with-requesters, requester edges are not printed.
    assert "requested_by" not in result.stdout


@buck_test()
async def test_anon_targets_none(buck: Buck) -> None:
    result = await buck.debug("anon-targets", "//:no_anon")
    assert result.stdout.strip() == ""


@buck_test()
async def test_anon_targets_requires_recording(buck: Buck) -> None:
    # Recording is on by default; turning it off must produce a hint rather
    # than silently-empty output.
    (buck.cwd / ".bucksettings.toml").write_text(
        "[analysis]\nrecord_requested_anon_targets = false\n"
    )
    await expect_failure(
        buck.debug("anon-targets", "//:root"),
        stderr_regex="Anon target recording is disabled",
    )
