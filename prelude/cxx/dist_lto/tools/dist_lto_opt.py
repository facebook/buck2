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

# Clean up clang flags by obtaining the cc1 flags and filtering out those unwanted.
# clang_flags is mutated after calling this function.
def _cleanup_flags(clang_flags: List[str]) -> List[str]:
    for i, arg in enumerate(clang_flags):
        if arg.startswith("--cc="):
            # Find the clang binary path.
            clang_flags[i] = arg.replace("--cc=", "")
        elif arg.startswith("-Wl,-plugin-opt,sample-profile="):
            # change linker flag `-Wl,-plugin-opt,sample-profile=` to `"-fprofile-sample-use="`
            # to make it affect in opt phase
            clang_flags[i] = arg.replace(
                "-Wl,-plugin-opt,sample-profile=", "-fprofile-sample-use="
            )

    # Get the cc1 flag dump with '-###'
    output = (
        subprocess.check_output(clang_flags + ["-###"], stderr=subprocess.STDOUT)
        .decode()
        .splitlines()
    )

    # Flags that may conflict with the existing bitcode attributes.
    # The value indicates if the flag is followed with a value.
    flags_to_delete = {
        "-triple": True,
        "-mframe-pointer=none": False,
        "-fmath-errno": False,
        "-fno-rounding-math": False,
        "-mconstructor-aliases": False,
        "-munwind-tables": False,
        "-target-cpu": True,
        "-tune-cpu": True,
    }

    clean_output = []
    skip_next = False
    for f in output[-1].split()[1:]:
        if skip_next:
            skip_next = False
        else:
            f = f.strip('"')
            if f in flags_to_delete:
                skip_next = flags_to_delete[f]
            else:
                clean_output.append(f)
    return clean_output


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out")
    parser.add_argument("--debug", action="store_true")
    parser.add_argument("opt_args", nargs=argparse.REMAINDER)
    args = parser.parse_args(argv[1:])

    fbcc_cmd = args.opt_args[1:3] + _cleanup_flags(args.opt_args[2:])
    if args.debug:
        # Print fbcc commandline and exit.
        print(" ".join(fbcc_cmd))
        return 0

    subprocess.check_call(fbcc_cmd)
    if os.stat(args.out).st_size == 0:
        print("error: opt produced empty file")
        return 1
    return 0


sys.exit(main(sys.argv))
