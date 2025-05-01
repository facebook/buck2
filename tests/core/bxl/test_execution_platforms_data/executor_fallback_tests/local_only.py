# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import pathlib
import sys

re_worker_path = "/run/re_worker/beacon"
if pathlib.Path(re_worker_path).exists():
    print("This only runs on local", file=sys.stderr)
    sys.exit(1)

out = sys.argv[1]
pathlib.Path(out).touch()
