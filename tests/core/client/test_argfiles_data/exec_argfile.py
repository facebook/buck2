#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import os

if __name__ == "__main__":
    result = []
    parser = argparse.ArgumentParser()
    parser.add_argument("--flavors", default=None)
    ns = parser.parse_args()

    assert ns.flavors == "iphonesimulator-x86_64"
    assert os.environ["BUCK2_ARG_FILE"] == "1"

    print("--config=foo.bar=1")
