# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _unicorn_selector_library(_ctx):
    return [DefaultInfo()]

unicorn_selector_library = rule(
    impl = _unicorn_selector_library,
    attrs = {
        "flags": attrs.list(attrs.string()),
    },
)
