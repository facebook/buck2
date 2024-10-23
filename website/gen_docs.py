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
    if args.buck2:
        return args.buck2
    elif args.prod:
        return "buck2"
    elif args.cargo:
        return "cargo run --bin=buck2 --"
    else:
        return "./buck2.sh"


# Given the path to the documentation, e.g. native/bxl/analysis_result
# produce a new name which is the destination, e.g. bxl/analysis_result
def doc_name(x):
    if x.startswith("native/bxl/"):
        return "api/" + x[7:]  # drop the native
    elif x.endswith("/function"):
        # Uninteresting docs we'd rather not have generated
        return None
    elif x.startswith("native/standard/") or x.startswith("native/extension/"):
        return "api/starlark/" + x.split("/")[-1]
    elif x.startswith("native/"):
        return "api/build/" + x[7:]
    else:
        raise RuntimeError("Unknown name: " + x)


def copy_starlark_docs():
    # Copy the starlark docs over. docusaurus does not handle upward path traversal very well.
    for x in Path("starlark-rust/docs").glob("*.md"):
        name = Path(x).stem
        prefix = "---\nid: " + name + "\n---\n"
        write_file(
            "docs/developers/starlark/" + name + ".generated.md", prefix + read_file(x)
        )


def generate_api_docs(buck):
    with tempfile.TemporaryDirectory() as tmp:
        # Actually generate the docs
        print("Running Buck...")
        subprocess.run(
            buck
            + " docs starlark --format=markdown_files --markdown-files-destination-dir="
            + tmp
            + " prelude//docs:rules.bzl",
            shell=True,
            check=True,
        )

        src = read_file(Path(tmp) / "starlark" / "prelude" / "docs" / "rules.bzl.md")
        dest = "docs/prelude/globals.generated.md"

        prefix = "---\nid: globals\n---\n"
        prefix += "# Rules\n\nThese rules are available as standard in Buck2.\n"
        src = "\n".join(src.splitlines()[1:])

        os.makedirs(Path(dest).parent, exist_ok=True)
        write_file(dest, prefix + src)

    with tempfile.TemporaryDirectory() as tmp:
        subprocess.run(
            buck
            + " docs starlark --format=markdown_files --markdown-files-destination-dir="
            + tmp
            + " --builtins",
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

            dest = "docs/" + name + ".generated.md"
            os.makedirs(Path(dest).parent, exist_ok=True)
            write_file(dest, prefix + src)

            # copy build APIs to BXL
            if name.startswith("api/build/"):
                name_without_build = "/".join(name.split("/")[2:])
                bxl_dest = "docs/api/bxl/" + name_without_build + ".generated.md"
                os.makedirs(Path(bxl_dest).parent, exist_ok=True)
                write_file(bxl_dest, prefix + src)


def parse_subcommands(output):
    res = []
    seen_subcommands = False
    for x in output.splitlines():
        if x == "Commands:":
            seen_subcommands = True
        if seen_subcommands and x.startswith("  ") and len(x) > 2 and x[2].isalpha():
            sub = x.strip().split()[0]
            if sub != "help":
                res.append(sub)
    return res


def generate_help_docs_subcommand(buck, args):
    cmd = buck + " " + " ".join(args) + " --help"
    print("Running " + cmd + " ...")
    res = subprocess.run(cmd, shell=True, check=True, capture_output=True)
    root = res.stdout.decode()
    return (
        "\n\n```text\n"
        + root
        + "\n```"
        + "\n\n".join(
            [
                generate_help_docs_subcommand(buck, args + [sub])
                for sub in parse_subcommands(root)
            ]
        )
    )


def generate_help_docs(buck):
    cmd = buck + " --help"
    print("Running " + cmd + " ...")
    res = subprocess.run(cmd, shell=True, check=True, capture_output=True)
    os.makedirs("docs/users/commands", exist_ok=True)
    for sub in parse_subcommands(res.stdout.decode()):
        res = generate_help_docs_subcommand(buck, [sub])
        write_file(
            "docs/users/commands/" + sub + ".generated.md",
            "---\nid: "
            + sub
            + "\ntitle: "
            + sub
            + "\n---\nThese are the flags/commands under `buck2 "
            + sub
            + "` and their `--help` output:"
            + res,
        )


def generate_query_docs(buck):
    os.makedirs("docs/users/query", exist_ok=True)
    for x in ["uquery", "cquery", "aquery"]:
        cmd = buck + " docs " + x + " --format=markdown"
        print("Running " + cmd + " ...")
        res = subprocess.run(cmd, shell=True, check=True, capture_output=True)
        write_file(
            "docs/users/query/" + x + ".generated.md",
            "---\nid: "
            + x
            + "\ntitle: "
            + x.title()
            + " Environment\n---\n"
            + res.stdout.decode(),
        )


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
        help="Whether to use a `cargo` built binary.",
    )
    parser.add_argument(
        "--buck2",
        nargs="?",
        help="Whether to use provided binary.",
    )
    args = parser.parse_args()

    # Change to buck2 directory
    buck2_dir = Path(__file__).absolute().parent.parent
    os.chdir(str(buck2_dir))

    # Clear the docs folder first so that if we change the names of any
    # objects, we'll remove old docs
    for x in Path("docs").rglob("*.generated.md"):
        os.remove(x)

    buck = buck_command(args)
    copy_starlark_docs()
    generate_api_docs(buck)
    generate_help_docs(buck)
    generate_query_docs(buck)


if __name__ == "__main__":
    main()
