# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import sys


def main():
    (out,) = sys.argv[1:]

    with open(out, "w"):
        pass


if __name__ == "__main__":
    main()
