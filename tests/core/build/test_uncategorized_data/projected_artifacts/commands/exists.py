#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os
import sys
from pathlib import Path

out = sys.argv[1]

for i in sys.argv[2:]:
    print("check {}".format(i), file=sys.stderr)
    if not os.path.exists(i):
        exit(1)


Path(out).touch()
