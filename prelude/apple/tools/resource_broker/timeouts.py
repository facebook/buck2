# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

DEFAULT_OPERATION_TIMEOUT = 10

# Simulator boot is an expensive command and can take a long time to complete
# depending on machine configuration and current machine load.
SIMULATOR_BOOT_TIMEOUT = 90
