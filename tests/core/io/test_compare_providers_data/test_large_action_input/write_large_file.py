# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os
import sys

chunk_size = 1024 * 1024  # 1MB chunks
total_size = 3 * 1024 * 1024 * 1024  # 3GB
with open(sys.argv[1], "wb") as f:
    for _ in range(0, total_size, chunk_size):
        chunk = os.urandom(chunk_size)
        f.write(chunk)
