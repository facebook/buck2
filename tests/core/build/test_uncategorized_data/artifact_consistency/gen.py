#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import sys
from pathlib import Path

out = Path(sys.argv[1])
Path.mkdir(out)
for f in sys.argv[2:]:
    Path(out / f).write_text("This is " + f)
