# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import os
import subprocess
import sys
from pathlib import Path


def main(argv):
    """
    This is a wrapper script around the `go` binary.
    - It fixes GOROOT and GOCACHE
    """
    if len(argv) < 2:
        print("usage: go_wrapper.py <wrapped binary> <args>", file=sys.stderr)
        return 1

    wrapped_binary = Path(argv[1])

    parser = argparse.ArgumentParser()
    parser.add_argument("--cc", action="append", default=[])
    parser.add_argument("--cgo_cflags", action="append", default=[])
    parser.add_argument("--cgo_cppflags", action="append", default=[])
    parser.add_argument("--cgo_ldflags", action="append", default=[])
    parsed, unknown = parser.parse_known_args(argv[2:])

    env = os.environ.copy()
    # Make paths absolute, otherwise go build will fail.
    if "GOROOT" in env:
        env["GOROOT"] = os.path.realpath(env["GOROOT"])

    env["GOCACHE"] = os.path.realpath(env["BUCK_SCRATCH_PATH"])

    cwd = os.getcwd()
    if len(parsed.cc) > 0:
        env["CC"] = " ".join([arg.replace("%cwd%", cwd) for arg in parsed.cc])

    if len(parsed.cgo_cflags) > 0:
        env["CGO_CFLAGS"] = " ".join(
            [arg.replace("%cwd%", cwd) for arg in parsed.cgo_cflags]
        )

    if len(parsed.cgo_cppflags) > 0:
        env["CGO_CPPFLAGS"] = " ".join(
            [arg.replace("%cwd%", cwd) for arg in parsed.cgo_cppflags]
        )

    if len(parsed.cgo_ldflags) > 0:
        env["CGO_LDFLAGS"] = " ".join(
            [arg.replace("%cwd%", cwd) for arg in parsed.cgo_ldflags]
        )

    return subprocess.call([wrapped_binary] + unknown, env=env)


sys.exit(main(sys.argv))
