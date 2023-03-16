# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import sys

import makefile_to_dep_file


def main():
    """
    Determines which dep file processor to use according to compiler type found in the first arg
    """
    compiler_type = sys.argv[1]  # noqa
    makefile_to_dep_file.makefile_to_depfile(sys.argv[2:])


if __name__ == "__main__":
    main()
