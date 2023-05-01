# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Run a crate's Cargo buildscript.
"""

import argparse
import os
import re
import subprocess
import sys
from pathlib import Path
from typing import Dict, IO, NamedTuple


def cfg_env(rustc_cfg: Path) -> Dict[str, str]:
    with rustc_cfg.open(encoding="utf-8") as f:
        lines = f.readlines()

    cfgs: Dict[str, str] = {}
    for line in lines:
        if (
            line.startswith("unix")
            or line.startswith("windows")
            or line.startswith("target_")
        ):
            keyval = line.strip().split("=")
            key = keyval[0]
            val = keyval[1].replace('"', "") if len(keyval) > 1 else "1"

            key = "CARGO_CFG_" + key.upper()
            if key in cfgs:
                cfgs[key] = cfgs[key] + "," + val
            else:
                cfgs[key] = val

    return cfgs


def run_buildscript(buildscript: str, env: Dict[str, str], cwd: str) -> str:
    try:
        proc = subprocess.run(
            os.path.abspath(buildscript),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8",
            check=True,
            env=env,
            cwd=cwd,
        )
    except OSError as ex:
        print(f"Failed to run {buildscript} because {ex}", file=sys.stderr)
        sys.exit(1)
    except subprocess.CalledProcessError as ex:
        print(ex.stderr, file=sys.stderr)
        sys.exit(ex.returncode)
    return proc.stdout


class Args(NamedTuple):
    buildscript: str
    rustc_cfg: Path
    outfile: IO[str]


def arg_parse() -> Args:
    parser = argparse.ArgumentParser(description="Run Rust build script")
    parser.add_argument("--buildscript", type=str, required=True)
    parser.add_argument("--rustc-cfg", type=Path, required=True)
    parser.add_argument("--outfile", type=argparse.FileType("w"), required=True)

    return Args(**vars(parser.parse_args()))


def main() -> None:  # noqa: C901
    args = arg_parse()

    env = cfg_env(args.rustc_cfg)

    out_dir = os.getenv("OUT_DIR")
    os.makedirs(out_dir, exist_ok=True)
    env["OUT_DIR"] = os.path.abspath(out_dir)

    manifest_dir = os.getenv("CARGO_MANIFEST_DIR")
    env["CARGO_MANIFEST_DIR"] = os.path.abspath(manifest_dir)

    env = dict(os.environ, **env)
    script_output = run_buildscript(args.buildscript, env, cwd=manifest_dir)

    cargo_rustc_cfg_pattern = re.compile("^cargo:rustc-cfg=(.*)")
    flags = ""
    for line in script_output.split("\n"):
        cargo_rustc_cfg_match = cargo_rustc_cfg_pattern.match(line)
        if cargo_rustc_cfg_match:
            flags += "--cfg={}\n".format(cargo_rustc_cfg_match.group(1))
        else:
            print(line)
    args.outfile.write(flags)


if __name__ == "__main__":
    main()
