# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Assemble and merge package trees for the prelude's Node.js rules.

package_tree.py flat-package --emit DIR [--package-json FILE] --out DIR
package_tree.py merge --deps DIR --prebuilt DIR --out DIR
"""

from __future__ import annotations

import argparse
import os
import shutil
import sys
from pathlib import Path


def _copy_entry(entry: os.DirEntry[str], target: Path, keep_symlinks: bool) -> None:
    if keep_symlinks and entry.is_symlink():
        os.symlink(os.readlink(entry.path), target, target_is_directory=entry.is_dir())
    elif entry.is_dir():
        copy_tree(Path(entry.path), target, keep_symlinks)
    else:
        shutil.copy(entry.path, target)


def copy_tree(src: Path, dst: Path, keep_symlinks: bool = True) -> None:
    """Copy the contents of `src` into `dst`, keeping symlinks as symlinks unless `keep_symlinks` is false."""
    dst.mkdir(parents=True, exist_ok=True)
    for entry in os.scandir(src):
        _copy_entry(entry, dst / entry.name, keep_symlinks)


def flat_package(emit: Path, package_json: Path | None, out: Path) -> None:
    """Lay out `emit` at the root of `out`, with `package_json` as its `package.json`."""
    copy_tree(emit, out)
    if package_json is not None:
        shutil.copy(package_json, out / "package.json")


def merge(
    deps: Path,
    prebuilt: Path,
    out: Path,
    keep_symlinks: bool = sys.platform != "win32",
) -> list[str]:
    """Merge two `node_modules` trees into `out`, or return the entries both define.

    A scope directory present in both trees is merged into one real directory,
    even when one tree links it, and its packages are compared instead. Nothing is written when any entry collides. Symlinks are
    copied as their targets on Windows, where a re-created link does not resolve
    in the consuming action.
    """
    clashes = []
    scopes = set()
    for entry in sorted(os.scandir(prebuilt), key=lambda e: e.name):
        other = deps / entry.name
        if entry.name.startswith("@") and entry.is_dir() and other.is_dir():
            scopes.add(entry.name)
            clashes += sorted(
                entry.name + "/" + child.name
                for child in os.scandir(entry.path)
                if os.path.lexists(other / child.name)
            )
        elif os.path.lexists(other):
            clashes.append(entry.name)
    if not clashes:
        out.mkdir(parents=True, exist_ok=True)
        for tree in (deps, prebuilt):
            for entry in os.scandir(tree):
                if entry.name in scopes:
                    copy_tree(Path(entry.path), out / entry.name, keep_symlinks)
                else:
                    _copy_entry(entry, out / entry.name, keep_symlinks)
    return clashes


def _path(arg: str) -> Path:
    # `node_modules` trees routinely exceed MAX_PATH on Windows.
    if sys.platform == "win32":
        return Path("\\\\?\\" + os.path.abspath(arg))
    return Path(arg)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    flat = commands.add_parser("flat-package")
    flat.add_argument("--emit", type=_path, required=True)
    flat.add_argument("--package-json", type=_path)
    flat.add_argument("--out", type=_path, required=True)
    merged = commands.add_parser("merge")
    merged.add_argument("--deps", type=_path, required=True)
    merged.add_argument("--prebuilt", type=_path, required=True)
    merged.add_argument("--out", type=_path, required=True)
    args = parser.parse_args()

    if args.command == "flat-package":
        flat_package(args.emit, args.package_json, args.out)
        return 0
    clashes = merge(args.deps, args.prebuilt, args.out)
    for name in clashes:
        print(
            f"node_modules entry {name} is in both the deps tree and the prebuilt tree",
            file=sys.stderr,
        )
    return 1 if clashes else 0


if __name__ == "__main__":
    sys.exit(main())
