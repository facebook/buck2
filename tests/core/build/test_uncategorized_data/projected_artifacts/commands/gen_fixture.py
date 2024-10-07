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

out = Path(sys.argv[1])
Path.mkdir(out)

Path(out / "a").touch()

Path.mkdir(out / "b")
Path(out / "b" / "b").write_text("This is b")

Path.mkdir(out / "c")
Path(out / "c" / "c").write_text("This is c")

os.symlink(Path("..") / "b" / "b", out / "c" / "b")
