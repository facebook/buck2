# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import os


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--out1",
        type=str,
        required=True,
    )
    parser.add_argument(
        "--out2",
        type=str,
        required=True,
    )

    args = parser.parse_args()

    if os.path.exists(args.out1):
        with open(args.out1, "r") as file:
            content = f"{file.read()}a"
    else:
        content = "a"

    with open(args.out1, "w") as f:
        f.write(f"{content}")

    if os.path.exists(args.out2):
        with open(args.out2, "r") as file:
            content = f"{file.read()}b"
    else:
        content = "b"

    with open(args.out2, "w") as f:
        f.write(f"{content}")


if __name__ == "__main__":
    main()
