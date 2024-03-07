# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import os
import signal
from dataclasses import dataclass
from io import TextIOWrapper


@dataclass
class IdbCompanion:
    socket_address: str
    pid: int
    stderr: TextIOWrapper

    def cleanup(self) -> None:
        os.kill(self.pid, signal.SIGTERM)
        self.stderr.close()
