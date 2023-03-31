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
    Determines which dep file processor to use according to compiler type. Arguments expected are, in order:
    dep_tracking_mode -> show_includes | makefile | show_headers | none
    intermediary_dep_file -> None | path as this is only needed when using clang for c++ files
    dep_file -> expected output path
    cmd_args -> args to be executed in order to get dependencies
    """
    dep_tracking_mode = sys.argv[1]  # noqa
    makefile_to_dep_file.makefile_to_depfile(sys.argv[2:])


if __name__ == "__main__":
    main()
