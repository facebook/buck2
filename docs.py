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
import shutil
import subprocess
from pathlib import Path


def buck_command(args):
    if args.prod:
        return "buck2"
    elif args.cargo:
        return "cargo run --bin=buck2 --"
    else:
        return "./buck2.sh"


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
    if os.path.exists("docs/generated"):
        shutil.rmtree("docs/generated")

    # Copy the starlark docs over. docusaurus does not handle upward path traversal very well.
    os.mkdir("docs/generated")
    shutil.copytree("starlark-rust/docs", "docs/generated/starlark_rust_docs")

    # Actually generate the docss
    subprocess.run(
        buck_command(args)
        + " docs starlark --format=markdown_files --markdown-files-destination-dir=docs/generated --builtins prelude//docs:rules.bzl",
        shell=True,
        check=True,
    )

    # Random hacks to fix things up
    shutil.move("docs/generated/native/bxl", "docs/generated/bxl")


if __name__ == "__main__":
    main()
