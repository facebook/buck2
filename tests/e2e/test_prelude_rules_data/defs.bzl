# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _value_impl(_ctx):
    return [DefaultInfo()]

# Carries a string computed at load time, so a test can read it with `uquery --output-attribute`.
value = rule(
    impl = _value_impl,
    attrs = {"value": attrs.string()},
)
