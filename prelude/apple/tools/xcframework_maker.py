# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import argparse
import shutil

from pathlib import Path


def main() -> None:
    parser = argparse.ArgumentParser(description="Tool to make an xcframework bundle.")
    parser.add_argument("--output-path")
    parser.add_argument("--framework-path")
    args = parser.parse_args()

    out_path = Path(args.output_path)
    out_path.mkdir(parents=True, exist_ok=False)

    plist_path = out_path / "Info.plist"
    plist_path.touch(exist_ok=False)

    framework_basename = Path(args.framework_path).name

    shutil.copytree(
        args.framework_path,
        out_path / framework_basename,
        symlinks=True,
        dirs_exist_ok=False,
    )


if __name__ == "__main__":
    main()
