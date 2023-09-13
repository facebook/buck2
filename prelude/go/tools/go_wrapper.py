# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

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

    env = os.environ.copy()
    # Make paths absolute, otherwise go build will fail.
    env["GOROOT"] = os.path.realpath(env["GOROOT"])
    env["GOCACHE"] = os.path.realpath(env["BUCK_SCRATCH_PATH"])

    return subprocess.call([wrapped_binary] + argv[2:], env=env)


sys.exit(main(sys.argv))
