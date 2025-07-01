# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _equals(expected, actual, msg = None):
    if expected != actual:
        if msg == None:
            fail("expected: {}, got: {}".format(expected, actual))
        else:
            fail("{}: expected: {}, got: {}".format(msg, expected, actual))

def _true(condition, msg = None):
    if not condition:
        if msg != None:
            fail(msg)
        else:
            fail("Condition is not met")

def _false(condition, msg = None):
    if condition:
        if msg != None:
            fail(msg)
        else:
            fail("Condition is expected to be false")

asserts = struct(
    equals = _equals,
    true = _true,
    false = _false,
)
