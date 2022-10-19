#!/usr/bin/env python3

import argparse
import os
import re
import subprocess
from pathlib import Path


def key_value_arg(s):
    key_value = s.split("=", maxsplit=1)
    if len(key_value) == 2:
        return (key_value[0], key_value[1])
    raise argparse.ArgumentTypeError(f"expected the form `key=value` for `{s}`")

def arg_parse():
    # Command line is <action.py> [args] -- rustc command line
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode")
    parser.add_argument("--buildscript")
    parser.add_argument("--output")
    parser.add_argument("--package-name")
    parser.add_argument("--version")
    parser.add_argument("--feature", action="append", default=[])
    parser.add_argument("--cfg", action="append")
    parser.add_argument("--env", action="append", type=key_value_arg)
    parser.add_argument("--target")
    return parser.parse_args()

def mk_feature(x):
    return "CARGO_FEATURE_" + x.upper().replace("-", "_")

def mk_env(args, parent):
    env = os.environ.copy()
    out_dir = Path(args.output)
    if parent:
        out_dir = out_dir.parent
    env["OUT_DIR"] = out_dir
    env["RUSTC"] = "rustc"
    for x in args.feature:
        env[mk_feature(x)] = "1"
    env["HOST"] = "x86_64-unknown-linux-gnu"
    env["TARGET"] = "x86_64-unknown-linux-gnu"
    env["CARGO_CFG_TARGET_ARCH"] = "x86_64"
    env["CARGO_CFG_TARGET_OS"] = "linux"
    env["CARGO_CFG_TARGET_POINTER_WIDTH"] = "64"
    env["CARGO_CFG_TARGET_ENDIAN"] = "little"
    return env

def run_args(args):
    result = subprocess.run(args.buildscript, stdout=subprocess.PIPE, text=True, check=True, env=mk_env(args, True))
    prog = re.compile("^cargo:rustc-cfg=(.*)")
    with open(args.output, "w") as file:
        for line in result.stdout.split("\n"):
            m = prog.match(line)
            if m:
                file.write("--cfg={}\n".format(m.group(1)))


def run_srcs(args):
    os.mkdir(args.output)
    result = subprocess.run(args.buildscript, check=True, text=True, env=mk_env(args, False))

def main():
    args = arg_parse()

    if args.mode == "args":
        run_args(args)
    elif args.mode == "srcs":
        run_srcs(args)
    else:
        raise RuntimeError("Unknown mode: " + repr(args.mode))

main()
