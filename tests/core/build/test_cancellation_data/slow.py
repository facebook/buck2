# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os
import sys
import time


def _touch(f):
    with open(f, "w"):
        pass


def main(args):
    duration, pids, output = args
    _touch(os.path.join(pids, str(os.getpid())))
    time.sleep(int(duration))
    _touch(output)


if __name__ == "__main__":
    main(sys.argv[1:])
