# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cfg/modifier:types.bzl", "Modifier", "is_modifiers_match")

def verify_normalized_target(target: str):
    # Do some basic checks that target looks reasonably valid and normalized
    # Targets should always be fully qualified to improve readability.
    if "//" not in target or target.startswith("//") or ":" not in target:
        fail(
            "Must specify fully qualified target (ex. `cell//foo:bar`). Found `{}`".format(
                target,
            ),
        )

def verify_normalized_modifier(modifier: Modifier):
    if modifier == None:
        pass
    elif is_modifiers_match(modifier):
        # TODO(scottcao): Add a test case for this once `bxl_test` supports testing failures
        for key, sub_modifier in modifier.items():
            if key != "_type":
                verify_normalized_modifier(sub_modifier)
    elif isinstance(modifier, str):
        verify_normalized_target(modifier)
    else:
        fail("Found unexpected modifier `{}` type `{}`".format(modifier, type(modifier)))
