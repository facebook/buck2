#!/usr/bin/env python3

"""
Python wrapper around `clang` intended for use by the parallel opt phase of
a Distributed ThinLTO compilation. This script works around a LLVM bug where
LLVM will return a zero exit code in the case where ThinLTO fails with a
fatal error.

Instead of trusting the exit code of the compiler, this script checks the
output file and returns 1 if the file has zero size.
"""

import argparse
import os
import subprocess
import sys
from typing import List


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out")
    parser.add_argument("opt_args", nargs=argparse.REMAINDER)
    args = parser.parse_args(argv[1:])

    subprocess.check_call(args.opt_args[1:])
    if os.stat(args.out).st_size == 0:
        print("error: opt produced empty file")
        return 1
    return 0


sys.exit(main(sys.argv))
