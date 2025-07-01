# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import sys
from time import sleep


def main() -> None:
    sleep(2)
    print("Slow action finally failed", file=sys.stderr)
    exit(1337)


if __name__ == "__main__":
    main()
