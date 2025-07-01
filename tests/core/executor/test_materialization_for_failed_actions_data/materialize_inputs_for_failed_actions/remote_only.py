# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import pathlib
import sys

re_worker_path = "/run/re_worker/beacon"
if not pathlib.Path(re_worker_path).exists():
    print("This only runs on RE", file=sys.stderr)
    sys.exit(1)

with open(sys.argv[1], "w") as f:
    f.write("yay!")
