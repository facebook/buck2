# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

"""
Removes files from a framework that are not necessary when distributing
inside app bundles. Specifically Modules/* Headers/* and Documentation/*

Example Usage:
xcframework_sanitizer.py --input original/Foo.framework \
--output output/Foo.framework 
"""

import argparse
import re
import shutil

from pathlib import Path
from typing import Callable, Iterable, Pattern


def _should_ignore(
    framework_root: str,
) -> Callable[[str, list[str]], Iterable[str]]:
    prohibited: list[str] = ["Modules", "Headers", "Documentation"]

    def _should_ignore_impl(root: str, contents: list[str]) -> Iterable[str]:
        if re.sub(r"/Versions/[A-Z]", "", root) == framework_root:
            return prohibited
        return []

    return _should_ignore_impl


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Prepare a framework for distribution by removing unnecessary files."
    )

    parser.add_argument("--input")
    parser.add_argument("--output")
    args = parser.parse_args()

    out_path = Path(args.output)

    shutil.copytree(
        args.input,
        out_path,
        symlinks=True,
        dirs_exist_ok=False,
        ignore=_should_ignore(args.input),
    )


if __name__ == "__main__":
    main()
