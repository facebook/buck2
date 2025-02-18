# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import argparse
import json
import os


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True)
    parser.add_argument("--manifest", dest="manifests", action="append", default=[])
    args = parser.parse_args()

    os.makedirs(args.output)

    for manifest in args.manifests:
        with open(manifest, "r") as f:
            for dst, src, _ in json.load(f):
                dst = os.path.join(args.output, dst)
                os.makedirs(os.path.dirname(dst), exist_ok=True)
                src = os.path.relpath(src, start=os.path.dirname(dst))
                os.symlink(src, dst)


if __name__ == "__main__":
    main()
