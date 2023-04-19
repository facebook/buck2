#!/usr/bin/env python3

import argparse
import os
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

def os_from_target(target):
    # `_mk_cmd` in `rust_third_party.bzl` passes the result of
    # `_get_native_host_triple()` here.
    os = target.split("-")[2]
    # The result is used to set `CARGO_CFG_TARGET_OS` so should be a value in
    # the set "windows", "macos", "ios", "linux", "android", "freebsd",
    # "dragonfly", "openbsd", "netbsd".
    return "macos" if os == "darwin" else os

def mk_env(args, parent):
    env = os.environ.copy()
    out_dir = Path(args.output)
    if parent:
        out_dir = out_dir.parent
    env["OUT_DIR"] = str(out_dir)
    env["RUSTC"] = "rustc"
    for x in args.feature:
        env[mk_feature(x)] = "1"
    env["HOST"] = args.target
    env["TARGET"] = args.target
    env["CARGO_CFG_TARGET_ARCH"] = args.target.split("-")[0]
    env["CARGO_CFG_TARGET_OS"] = os_from_target(args.target)
    env["CARGO_CFG_TARGET_POINTER_WIDTH"] = "64"
    env["CARGO_CFG_TARGET_ENDIAN"] = "little"
    for k, v in args.env or []:
        env[k] = v
    return env

def run_args(args):
    result = subprocess.run(args.buildscript, stdout=subprocess.PIPE, text=True, check=True, env=mk_env(args, True))
    with open(args.output, "w") as file:
        for line in result.stdout.split("\n"):
            pre, _, post = line.partition("=")
            if pre == "cargo:rustc-cfg":
                file.write("--cfg={}\n".format(post))

def run_srcs(args):
    Path(args.output).mkdir(exist_ok=True)
    subprocess.run(args.buildscript, check=True, text=True, env=mk_env(args, False))

def main():
    args = arg_parse()

    if args.mode == "args":
        run_args(args)
    elif args.mode == "srcs":
        run_srcs(args)
    else:
        raise RuntimeError("Unknown mode: " + repr(args.mode))

main()
