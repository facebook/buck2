#!/usr/bin/env python3

"""
Compile the given Go sources into a Go package.

Example:

 $ ./compile_wrapper.py \
       --compiler compile \
       --assember assemble \
       --output srcs.txt src/dir/

"""

import argparse
import os
import subprocess
import sys
from pathlib import Path
from typing import List


def _compile(compile_prefix: List[str], output: Path, srcs: List[Path]):
    cmd = []
    cmd.extend(compile_prefix)
    cmd.append("-trimpath={}".format(os.getcwd()))
    cmd.append("-o")
    cmd.append(output)
    cmd.extend(srcs)
    subprocess.check_call(cmd)


def _pack(pack_prefix: List[str], output: Path, items: [Path]):
    cmd = []
    cmd.extend(pack_prefix)
    cmd.append("r")
    cmd.append(output)
    cmd.extend(items)
    subprocess.check_call(cmd)


def main(argv):
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("--compiler", action="append", default=[])
    parser.add_argument("--assembler", action="append", default=[])
    parser.add_argument("--packer", action="append", default=[])
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("srcs", type=Path, nargs="*")
    args = parser.parse_args(argv[1:])

    # If there's no srcs, just leave an empty file.
    if not args.srcs:
        args.output.touch()
        return

    go_files = [s for s in args.srcs if s.suffix == ".go"]
    s_files = [s for s in args.srcs if s.suffix == ".s"]

    if go_files:
        compile_prefix = []
        compile_prefix.extend(args.compiler)

        # If we have assembly files, generate the symabi file to compile against.
        if s_files:
            symabis = args.output.with_suffix(".symabis")
            _compile(args.assembler + ["-gensymabis"], symabis, s_files)
            compile_prefix.extend(["-symabis", symabis])

        _compile(compile_prefix, args.output, go_files)

    else:
        args.output.touch()

    # If there are assembly files, assemble them to an object and add into the
    # output archive.
    if s_files:
        s_object = args.output.with_suffix(".o")
        _compile(args.assembler, s_object, s_files)
        _pack(args.packer, args.output, [s_object])


sys.exit(main(sys.argv))
