#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import argparse
import os
import subprocess
import sys
from pathlib import Path


def main(argv):
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("--cgo", action="append", default=[])
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("srcs", type=Path, nargs="*")
    args = parser.parse_args(argv[1:])

    output = args.output.resolve(strict=False)
    # the only reason we need this whapper is to create `-objdir`,
    # because neither `go tool cgo` nor buck can create it.
    os.makedirs(output, exist_ok=True)

    env = os.environ.copy()

    cmd = []
    cmd.extend(args.cgo)
    cmd.append(f"-objdir={output}")
    cmd.append("--")

    cmd.extend(args.srcs)
    return subprocess.call(cmd, env=env)


sys.exit(main(sys.argv))
