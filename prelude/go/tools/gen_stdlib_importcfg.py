# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import os
import sys
from pathlib import Path


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--stdlib", type=Path, default=None)
    parser.add_argument("--output", type=Path, default=None)

    args = parser.parse_args()

    with open(args.output, "w") as f:
        for root, _dirs, files in os.walk(args.stdlib):
            for file in files:
                pkg_path = Path(root, file)
                pkg_name, _ = os.path.splitext(pkg_path.relative_to(args.stdlib))
                # package names always use unix slashes
                pkg_name = pkg_name.replace("\\", "/")
                f.write(f"packagefile {pkg_name}={pkg_path}\n")


if __name__ == "__main__":
    sys.exit(main(sys.argv))
