#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import json
import os


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "env_file", help="A JSON file containing the environment to inject"
    )
    parser.add_argument("executable")
    parser.add_argument("args", nargs="*")

    args = parser.parse_args()

    with open(args.env_file) as env_file:
        env_from_file = json.load(env_file)
        env = {**os.environ, **env_from_file}

    os.execve(args.executable, [args.executable, *args.args], env)


if __name__ == "__main__":
    main()
