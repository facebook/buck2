#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import sys
from pathlib import Path

out = Path(sys.argv[1])
Path.mkdir(out)
for f in sys.argv[2:]:
    Path(out / f).write_text("This is " + f)
