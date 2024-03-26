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
    parser = argparse.ArgumentParser()
    parser.add_argument("--go", default="go", type=Path)
    parser.add_argument("--workdir", type=Path)
    parser.add_argument("--output", type=argparse.FileType("w"), default=sys.stdout)
    parsed, unknown = parser.parse_known_args(argv[1:])

    env = os.environ.copy()
    # Make paths absolute, otherwise go build will fail.
    if "GOROOT" in env:
        env["GOROOT"] = os.path.realpath(env["GOROOT"])

    env["GOCACHE"] = os.path.realpath(env["BUCK_SCRATCH_PATH"])

    retcode = subprocess.call(
        [parsed.go.resolve(), "list"] + unknown,
        env=env,
        stdout=parsed.output,
        cwd=parsed.workdir,
    )
    parsed.output.close()
    return retcode


sys.exit(main(sys.argv))
