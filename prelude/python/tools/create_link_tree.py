# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import argparse
import json
import os
from typing import Set


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True)
    parser.add_argument("--manifest", dest="manifests", action="append", default=[])
    args = parser.parse_args()

    os.makedirs(args.output)

    pkgs: Set[str] = set()
    pkgs_with_init: Set[str] = set()

    def _add_pkg(pkg: str) -> None:
        pkgs.add(pkg)
        parent = os.path.dirname(pkg)
        if parent:
            _add_pkg(parent)

    for manifest in args.manifests:
        with open(manifest, "r") as f:
            for dst, src, _ in json.load(f):
                # Record pkgs and the ones w/ `__init__.py` files already.
                if dst.endswith((".py", ".so")):
                    pkg = os.path.dirname(dst)
                    _add_pkg(pkg)
                    if os.path.basename(dst) == "__init__.py":
                        pkgs_with_init.add(pkg)

                # Create symlink.
                dst = os.path.join(args.output, dst)
                os.makedirs(os.path.dirname(dst), exist_ok=True)
                src = os.path.relpath(src, start=os.path.dirname(dst))
                os.symlink(src, dst)

    # Create any missing ones.
    for pkg in pkgs - pkgs_with_init:
        with open(os.path.join(args.output, pkg, "__init__.py"), "wb") as _:
            pass


if __name__ == "__main__":
    main()
