#!/usr/bin/env python3
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
import re
import sys
from typing import List


# pyre-fixme[3]: Return type must be annotated.
# pyre-fixme[2]: Parameter must be annotated.
def enumerate_manifest(args):
    entries = []
    with open(args.manifest, "r") as manifest:
        for dest, src, origin in json.load(manifest):
            if os.path.isdir(src):
                for root, dirs, files in os.walk(src):
                    # Ensure a consistent iteration order.
                    # Sorting dirs in place affects the future iteration
                    # order of os.walk.
                    dirs.sort()
                    files.sort()
                    for name in files:
                        path = os.path.join(root, name)
                        updated_dest = os.path.join(dest, os.path.relpath(path, src))
                        entries.append([updated_dest, path, origin])
            else:
                entries.append([dest, src, origin])
    json.dump(entries, args.output, indent=2)
    args.output.close()


def main(argv: List[str]) -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=argparse.FileType("w"), default=sys.stdout)
    parser.add_argument(
        "--manifest", help="existing manifest to replace directories in"
    )
    parser.add_argument("--origin", help="description of source origin")
    parser.add_argument("--prefix", help="prefix to prepend to destinations")
    parser.add_argument("--exclude", help="RE pattern to exclude files")
    parser.add_argument("--extracted", help="path to directory of sources")
    args = parser.parse_args(argv[1:])

    if args.manifest:
        return enumerate_manifest(args)
    assert (
        args.extracted is not None
    ), "Must provide either --manifest or --extracted to enumerate"

    exclude = None
    if args.exclude:
        exclude = re.compile(args.exclude)

    entries = []
    for root, dirs, files in os.walk(args.extracted):
        # Ensure a consistent iteration order.
        # Sorting dirs in place affects the future iteration
        # order of os.walk.
        dirs.sort()
        files.sort()
        for name in files:
            path = os.path.join(root, name)
            if exclude is not None and exclude.search(path):
                continue
            dest = os.path.relpath(path, args.extracted)
            if args.prefix is not None:
                dest = os.path.join(args.prefix, dest)
            entry = [dest, path]
            if args.origin is not None:
                entry.append(args.origin)
            entries.append(entry)

    json.dump(entries, args.output, indent=2)
    args.output.close()


sys.exit(main(sys.argv))
