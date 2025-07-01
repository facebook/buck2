# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# kotlinc is strict about the target that you can pass, e.g.
# error: unknown JVM target version: 8.  Supported versions: 1.6, 1.8, 9, 10, 11, 12
def get_kotlinc_compatible_target(target: str) -> str:
    return "1.6" if target == "6" else "1.8" if target == "8" else target
