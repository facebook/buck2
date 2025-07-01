# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

AppleErrorCategory = record(
    # If you pass a string, it will use `matcher in stderr.lower()`.
    # If you pass a regex(), it will run `regex("exp").match(stderr.lower())`.
    matcher = str | BuckRegex,
    # List of category tags to be applied in the event of this error.
    # Categories are automatically prefixed with "apple_" for historical reasons.
    category = str,
    # Based on the error, you can inject something like a wiki link/etc.
    message = field([str, None], default = None),
)
