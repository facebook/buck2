#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# credits to implementation in https://www.internalfb.com/code/fbsource/fbcode/eden/addons/build-tar.py

import argparse
import functools
import glob
import os
import shlex
import shutil
import subprocess
import sys

from typing import List

rm_rf = functools.partial(shutil.rmtree, ignore_errors=True)
print_err = functools.partial(print, file=sys.stderr)
glob_r = functools.partial(glob.glob, recursive=True)


def run(command: List[str], cwd=None, env=None):
    print_err(f"{cwd if cwd else ' '} $ {shlex.join(command)}")

    if env is not None:
        env = {**os.environ, **env}

    # shell=True with a List `command` seems buggy on *nix.
    # It might run ['sh', '-c', 'a', 'b'] instead of ['sh', '-c', 'a b'].
    subprocess.run(command, shell=(os.name == "nt"), check=True, cwd=cwd, env=env)


def realpath_args(args: List[str]) -> List[str]:
    return [os.path.realpath(arg) if os.path.exists(arg) else arg for arg in args]


def copy_writable(src, dst, *, follow_symlinks=True):
    """shutil.copy, but ensure that yarn.lock is writable
    - RE might make src/ read-only with its "restrictive mode".
    - When copying the RE "restrictive" src/, yarn.lock is read-only.
    - yarn wants yarn.lock to be writable, even with --frozen-lockfile.
    """
    shutil.copy(src, dst, follow_symlinks=follow_symlinks)
    if dst.endswith("yarn.lock") and os.name != "nt":
        os.chmod(dst, 0o666)


def main():
    parser = argparse.ArgumentParser(description="Creates a html from explain source.")
    parser.add_argument(
        "-o",
        "--output",
        nargs="?",
        default="explain.html",
        help="Path to the output '.html' file.",
    )

    args = parser.parse_args()

    out = args.output

    src_join = functools.partial(os.path.join, "js")

    run(["yarn", "--cwd", src_join(), "run", "build"], env={"CI": "false"})

    # inline js into html file
    with open(src_join("dist/app.js"), "r") as f:
        js_content = f.read()
    with open(src_join("index.html"), "r") as f:
        html_content = f.read()
    html_content = html_content.replace(
        '<script src="dist/app.js"></script>', f"<script>{js_content}</script>"
    )
    with open(out, "w") as out_file:
        out_file.write(html_content)


if __name__ == "__main__":
    main()
