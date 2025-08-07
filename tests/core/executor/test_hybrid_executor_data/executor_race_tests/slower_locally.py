# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# This command runs slow on both local and RE, but it runs slower locally. The
# upshot is that local should start before RE is done, but RE will finish
# first, and then will cancel local. If it doesn't cancel local, the test will
# fail.

import pathlib
import sys
import time

re_worker_path = "/run/re_worker/beacon"
if pathlib.Path(re_worker_path).exists():
    time.sleep(5)
    out = sys.argv[1]
    pathlib.Path(out).touch()
    sys.exit(0)

time.sleep(20)
sys.exit(1)
