#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

"""
Generate API documentation for the website.
"""

import argparse
import os
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import List


def read_file(path: Path) -> str:
    with open(path, "r") as f:
        return f.read()


def write_file(path: Path, contents: str) -> None:
    with open(path, "w") as f:
        f.write(contents)


def setup_gen_dir(path: Path) -> None:
    shutil.rmtree(path, ignore_errors=True)
    path.mkdir(parents=True, exist_ok=True)
    (path / "README.txt").write_text(
        """
This directory contains generated files.

Re-generate by running `fbcode/buck2/website/gen_docs.py`.
"""
    )


def buck_command(args: argparse.Namespace) -> str:
    if args.buck2:
        return args.buck2
    elif args.prod:
        return "buck2"
    elif args.cargo:
        return "cargo run --bin=buck2 --"
    else:
        return "./buck2.py"


def copy_starlark_docs() -> None:
    base_path = Path("docs") / "developers" / "starlark"
    setup_gen_dir(base_path)
    # Copy the starlark docs over. docusaurus does not handle upward path traversal very well.
    for x in Path("starlark-rust/docs").glob("*.md"):
        name = Path(x).stem
        prefix = "---\nid: " + name + "\n---\n"
        write_file(base_path / (name + ".generated.md"), prefix + read_file(x))


def generate_api_docs(buck: str) -> None:
    with tempfile.TemporaryDirectory() as tmp:
        base_dir = Path("docs") / "prelude"
        setup_gen_dir(base_dir)
        # Actually generate the docs
        print("Running Buck...")
        subprocess.run(
            buck
            + " docs starlark --format=markdown_files --output-dir="
            + tmp
            + " prelude//docs:rules.bzl",
            shell=True,
            check=True,
        )

        src = read_file(Path(tmp) / "prelude" / "docs" / "rules.bzl.md")
        dest = base_dir / "globals.generated.md"

        prefix = "---\nid: globals\n---\n"
        prefix += "# Rules\n\nThese rules are available as standard in Buck2.\n"
        src = "\n".join(src.splitlines()[1:])

        os.makedirs(dest.parent, exist_ok=True)
        write_file(dest, prefix + src)

    with tempfile.TemporaryDirectory() as tmp:
        base_dir = Path("docs") / "api"
        setup_gen_dir(base_dir)
        subprocess.run(
            buck + " docs starlark-builtins --output-dir " + tmp,
            shell=True,
            check=True,
        )

        for orig in Path(tmp).rglob("*.md"):
            path = orig.relative_to(tmp)
            dest = base_dir.joinpath(path)
            dest.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(orig, dest)


def generate_bxl_utils_api_docs(buck: str) -> None:
    with tempfile.TemporaryDirectory() as tmp:
        base_dir = Path("docs") / "api" / "bxl_utils"
        setup_gen_dir(base_dir)
        bxl_utils_foler = Path("prelude") / "bxl"
        bxl_utils_file_prefix = "prelude//bxl"

        os.makedirs(base_dir, exist_ok=True)

        for file in bxl_utils_foler.rglob("*.bxl"):
            relative_file = file.relative_to(bxl_utils_foler)
            relative_folder_name = (
                "" if file.parent == bxl_utils_foler else str(relative_file.parent)
            )
            buck_bxl_file_name = (
                bxl_utils_file_prefix + relative_folder_name + ":" + relative_file.name
            )

            subprocess.run(
                buck
                + " docs starlark --format=markdown_files --output-dir="
                + tmp
                + " "
                + buck_bxl_file_name,
                shell=True,
                check=True,
            )

            content = read_file(
                Path(tmp) / str(bxl_utils_foler) / (str(relative_file) + ".md")
            )

            # Remove the first line of the file, which is the file path of the bxl file
            content = "\n".join(content.splitlines()[1:])

            prefix = f"""# {file.stem} \n\nThe following functions are defined in the bxl file: `{buck_bxl_file_name}`. \
            You can import them in your bxl file by using `load("@{buck_bxl_file_name}", "function_name")`\n\n"""

            dest = base_dir / (str(relative_file.with_suffix(".md")))

            write_file(dest, prefix + content)


def parse_subcommands(output: str) -> List[str]:
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


def generate_help_docs_subcommand(buck: str, args: List[str]) -> str:
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


def generate_help_docs(buck: str) -> None:
    base_dir = Path("docs") / "users" / "commands"
    setup_gen_dir(base_dir)

    cmd = buck + " --help"
    print("Running " + cmd + " ...")
    res = subprocess.run(cmd, shell=True, check=True, capture_output=True)
    for sub in parse_subcommands(res.stdout.decode()):
        output = generate_help_docs_subcommand(buck, [sub])
        write_file(
            base_dir / (sub + ".generated.md"),
            "---\nid: "
            + sub
            + "\ntitle: "
            + sub
            + "\n---\nThese are the flags/commands under `buck2 "
            + sub
            + "` and their `--help` output:"
            + output,
        )


def generate_query_docs(buck: str) -> None:
    base_dir = Path("docs") / "users" / "query"
    setup_gen_dir(base_dir)

    for x in ["uquery", "cquery", "aquery"]:
        cmd = buck + " docs " + x + " --format=markdown"
        print("Running " + cmd + " ...")
        res = subprocess.run(cmd, shell=True, check=True, capture_output=True)
        write_file(
            base_dir / (x + ".generated.md"),
            "---\n"
            + f"id: {x}\n"
            + f"title: {x.title()} Environment\n"
            + "toc_max_heading_level: 4\n"
            + "---\n"
            + "\nimport useBaseUrl from '@docusaurus/useBaseUrl';"
            + "\nimport { FbInternalOnly } from 'docusaurus-plugin-internaldocs-fb/internal';\n\n"
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
    generate_bxl_utils_api_docs(buck)
    generate_help_docs(buck)
    generate_query_docs(buck)


if __name__ == "__main__":
    main()
