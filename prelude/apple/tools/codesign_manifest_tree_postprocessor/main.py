# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import shutil

from pathlib import Path


def _args_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Tool which postprocesses a codesign manifest tree."
    )
    parser.add_argument(
        "--codesign-manifest-tree",
        metavar="<codesign_manifest_tree.json>",
        type=Path,
        required=True,
        help="Path to the [codesign-manifest-tree] subtarget output.",
    )
    parser.add_argument(
        "--output",
        metavar="<output.json>",
        type=Path,
        required=True,
        help="Path to the postprocessed file.",
    )

    return parser


def _main() -> None:
    args_parser = _args_parser()
    args = args_parser.parse_args()
    shutil.copy2(args.codesign_manifest_tree, args.output)


if __name__ == "__main__":
    _main()
