# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import subprocess
import unittest
from unittest.mock import MagicMock, patch

from .merge_index_store import merge_directories, parse_arguments


class TestMergeIndexStore(unittest.TestCase):
    def test_parse_arguments(self) -> None:
        test_args = ["-d", "destination", "-s", "source1", "source2"]
        with patch("sys.argv", ["script"] + test_args):
            args = parse_arguments()
            self.assertEqual(args.dest, "destination")
            self.assertEqual(args.sources, ["source1", "source2"])

    @patch("os.path.isdir")
    @patch("subprocess.run")
    def test_merge_directories(
        self, mock_run: MagicMock, mock_isdir: MagicMock
    ) -> None:
        mock_isdir.return_value = True
        mock_run.return_value = MagicMock(returncode=0, stderr="")

        merge_directories("source", "destination")
        mock_run.assert_called_once_with(
            ["rsync", "-a", "--ignore-existing", "source/", "destination"],
            stderr=subprocess.PIPE,
            text=True,
        )

    @patch("os.path.isdir")
    @patch("subprocess.run")
    def test_merge_directories_failure(
        self, mock_run: MagicMock, mock_isdir: MagicMock
    ) -> None:
        mock_isdir.return_value = True
        mock_run.return_value = MagicMock(returncode=1, stderr="Error")

        with self.assertRaises(Exception) as context:
            merge_directories("source", "destination")
        self.assertTrue("Failed to merge" in str(context.exception))

    @patch("os.path.isdir")
    def test_merge_non_existing_directory(self, mock_isdir: MagicMock) -> None:
        mock_isdir.return_value = False
        with self.assertRaises(Exception) as context:
            merge_directories("source", "destination")
        self.assertTrue(
            "Directory source does not exist or is not a directory"
            in str(context.exception)
        )
