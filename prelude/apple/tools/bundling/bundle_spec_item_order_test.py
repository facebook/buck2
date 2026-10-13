# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import itertools
import unittest

from .assemble_bundle_types import BundleSpecItem


class TestBundleSpecItemOrder(unittest.TestCase):
    def test_items_can_each_be_less_than_the_other(self) -> None:
        first = BundleSpecItem(src="a/x", dst="Resources/c")
        second = BundleSpecItem(src="b/x", dst="Resources/b")
        # The field comparisons are joined with `or`, so the smaller `dst` of `second` wins
        # against its larger `src`.
        self.assertTrue(first < second)
        self.assertTrue(second < first)

        third = BundleSpecItem(src="c/x", dst="Resources/a")
        sorted_sources = {
            tuple(item.src for item in sorted(permutation))
            for permutation in itertools.permutations([first, second, third])
        }
        self.assertGreater(len(sorted_sources), 1)
