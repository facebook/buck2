# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import re


def truncate(x: str, limit: int) -> str:
    if len(x) <= limit:
        return x
    else:
        return x[: limit // 2] + " <<TRUNCATED>> " + x[-(limit // 2) :]


def print_occurences_msg(
    needle: str, haystack: str, occurrences: int, success: bool
) -> None:
    OUTPUT_LIMIT = 10000
    # Hacky way to actually make sure we print the full output when a string
    # does not appear the correct number of times.
    assert success, "Expected to find {} occurrences of `{}` in `{}`".format(
        occurrences, needle, truncate(repr(haystack), OUTPUT_LIMIT)
    )


def assert_occurrences(needle: str, haystack: str, occurrences: int) -> None:
    print_occurences_msg(
        needle, haystack, occurrences, haystack.count(needle) == occurrences
    )


def assert_occurrences_regex(needle: str, haystack: str, occurrences: int) -> None:
    print_occurences_msg(
        needle,
        haystack,
        occurrences,
        len(re.findall(needle, haystack, re.MULTILINE)) == occurrences,
    )
