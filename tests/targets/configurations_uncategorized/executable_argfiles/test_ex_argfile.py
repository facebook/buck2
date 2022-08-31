#!/usr/bin/env python3
# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import argparse

if __name__ == "__main__":
    result = []
    parser = argparse.ArgumentParser()
    parser.add_argument("--flavors", default=None)
    ns = parser.parse_args()

    if ns.flavors is not None:
        result.append("--config\ncxx.default_platform=iphonesimulator-x86_64")

    cwd = "fbcode//buck2/tests/targets/configurations_uncategorized/executable_argfiles"
    result.append(f"--flagfile\n{cwd}/jackalope")

    [print(config) for config in result]
