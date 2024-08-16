# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

AppleErrorCategories = record(
    # Lowercase string that should (preferably uniquely) match the lowercased
    # stderr output caused by an error of interest.
    string_match = str,
    # List of category tags to be applied in the event of this error.
    categories = list[str],
)
