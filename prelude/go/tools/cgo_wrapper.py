#!/usr/bin/env python3

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
    parser.add_argument("--cpp", action="append", default=[])
    parser.add_argument("srcs", type=Path, nargs="*")
    args = parser.parse_args(argv[1:])

    output = args.output.resolve(strict=False)
    os.makedirs(output, exist_ok=True)

    os.environ["CC"] = args.cpp[0]

    cmd = []
    cmd.extend(args.cgo)
    # cmd.append("-importpath={}")
    # cmd.append("-srcdir={}")
    cmd.append(f"-objdir={output}")
    # cmd.append(cgoCompilerFlags)
    cmd.append("--")
    # cmd.append(cxxCompilerFlags)
    cmd.extend(args.cpp[1:])
    cmd.extend(args.srcs)
    subprocess.check_call(cmd)


sys.exit(main(sys.argv))
