# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import argparse

from pathlib import Path


def main() -> None:
    parser = argparse.ArgumentParser(description="Tool to make an xcframework bundle.")
    args = parser.parse_args()


if __name__ == "__main__":
    main()
