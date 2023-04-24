#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Generate API documentation for the website.
"""

import argparse
import os
import subprocess
import tempfile
from pathlib import Path


def read_file(path):
    with open(path, "r") as f:
        return f.read()


def write_file(path, contents):
    with open(path, "w") as f:
        f.write(contents)


def buck_command(args):
    if args.prod:
        return "buck2"
    elif args.cargo:
        return "cargo run --bin=buck2 --"
    else:
        return "./buck2.sh"


# Given the path to the documentation, e.g. native/bxl/analysis_result
# produce a new name which is the destination, e.g. bxl/analysis_result
def doc_name(x):
    if x.startswith("native/bxl/"):
        return x[7:]  # drop the native
    elif x.endswith("/rules.bzl"):
        return "rules"
    elif x.endswith("/function"):
        # Uninteresting docs we'd rather not have generated
        return None
    elif x.startswith("native/standard/") or x.startswith("native/extension/"):
        return "starlark/" + x.split("/")[-1]
    elif x.startswith("native/"):
        return "build/" + x[7:]
    else:
        raise RuntimeError("Unknown name: " + x)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--prod",
        action="store_true",
        default=False,
        help="Whether to use the production `buck2` binary",
    )
    parser.add_argument(
        "--cargo",
        action="store_true",
        default=False,
        help="Whethern to use a `cargo` built binary.",
    )
    args = parser.parse_args()

    # Change to buck2 directory
    buck2_dir = Path(__file__).parent.absolute()
    os.chdir(str(buck2_dir))

    # Clear the docs folder first so that if we change the names of any
    # objects, we'll remove old docs
    for x in Path("docs").rglob("*.generated.md"):
        os.remove(x)

    # Copy the starlark docs over. docusaurus does not handle upward path traversal very well.
    for x in Path("starlark-rust/docs").glob("*.md"):
        name = Path(x).stem
        prefix = "---\nid: " + name + "\n---\n"
        write_file(
            "docs/developers/starlark/" + name + ".generated.md", prefix + read_file(x)
        )

    with tempfile.TemporaryDirectory() as tmp:
        # Actually generate the docs
        print("Running Buck...")
        subprocess.run(
            buck_command(args)
            + " docs starlark --format=markdown_files --markdown-files-destination-dir="
            + tmp
            + " --builtins prelude//docs:rules.bzl",
            shell=True,
            check=True,
        )

        for orig in Path(tmp).rglob("*.md"):
            src = read_file(orig)
            path = os.path.relpath(orig, tmp)
            if path.endswith(".md"):
                path = path[:-3]

            name = doc_name(path)
            if name is None:
                continue

            prefix = "---\nid: " + name.rsplit("/")[-1] + "\n---\n"
            if name == "rules":
                prefix += "# Rules\n\nThese rules are available as standard in Buck2.\n"
                src = "\n".join(src.splitlines()[1:])

            dest = "docs/api/" + name + ".generated.md"
            os.makedirs(Path(dest).parent, exist_ok=True)
            write_file(dest, prefix + src)


if __name__ == "__main__":
    main()
