# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Assert that a given value has a specific type, and return that value.
# Fails at runtime if the value does not have the right type.
def cast(value, type):
    def inner(_: type):
        pass

    inner(value)
    return value
