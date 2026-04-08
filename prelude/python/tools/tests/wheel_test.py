# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import tempfile
import unittest
import zipfile
from pathlib import Path

import wheel


class WheelBuilderTest(unittest.TestCase):
    def test_readme_sets_metadata_body_and_content_type(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            output = tmp / "demo-1.0-py3-none-any.whl"
            readme = tmp / "README.md"
            module = tmp / "demo.py"

            readme.write_text("# Demo\n\nLong description.\n", encoding="utf-8")
            module.write_text("print('ok')\n", encoding="utf-8")

            with wheel.WheelBuilder(
                name="demo",
                version="1.0",
                output=str(output),
                readme=str(readme),
            ) as whl:
                whl.write("demo.py", str(module))

            with zipfile.ZipFile(output) as archive:
                metadata = archive.read("demo-1.0.dist-info/METADATA").decode("utf-8")

            self.assertIn("Description-Content-Type: text/markdown\n", metadata)
            self.assertTrue(metadata.endswith("\n# Demo\n\nLong description.\n"))
