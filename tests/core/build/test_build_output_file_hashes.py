# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_file_output(
    buck: Buck,
    tmp_path: Path,
) -> None:
    hashes_file_path = tmp_path / "hashes"
    await buck.build(
        "//:file.txt",
        f"--output-hashes-file={hashes_file_path}",
    )
    # Check json output
    with open(hashes_file_path) as f:
        data = json.loads(f.read())
        for line in data:
            assert line["path"] is not None
            assert line["kind"] in {"directory", "file", "symlink", "external_symlink"}
        # Check the entry for file.txt is what we expect
        file_entry = next(line for line in data if line["path"].endswith("file.txt"))
        assert file_entry is not None
        assert file_entry["kind"] == "file"
        assert file_entry["digest_kind"] == "SHA1"
        assert file_entry["digest"] == "fb19d5b1546753df5f7741efbabd0d24dcaacd65:20"
