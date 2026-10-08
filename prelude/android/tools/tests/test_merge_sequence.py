# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Tests for merge_sequence.py."""

import unittest

from android.tools.merge_sequence import (
    ApkModuleGraph,
    get_native_linkables_by_merge_sequence,
    Label,
    LinkableGraphNode,
    MergeSequenceGroupSpec,
)


def _node(target: str, deps: list) -> LinkableGraphNode:
    return LinkableGraphNode(
        raw_target=target,
        soname="lib.so",
        deps=[Label(d) for d in deps],
        can_be_asset=True,
        force_static=False,
        labels=[],
    )


class AssignNamesTest(unittest.TestCase):
    def _final_names(self) -> tuple:
        # `libfoo.so` splits into two libraries, because only `//:b` reaches module `m`,
        # and the second entry is named like the counter suffix the split produces.
        graph = {
            Label("//:a"): _node("//:a", []),
            Label("//:b"): _node("//:b", ["//:c"]),
            Label("//:c"): _node("//:c", []),
            Label("//:d"): _node("//:d", []),
        }
        sequence = [
            MergeSequenceGroupSpec(("libfoo.so", ["^//:a$", "^//:b$"])),
            MergeSequenceGroupSpec(("libfoo_1.so", ["^//:d$"])),
        ]
        modules = ApkModuleGraph(
            {"//:a": "dex", "//:b": "dex", "//:c": "m", "//:d": "dex"}
        )
        node_data, names, _ = get_native_linkables_by_merge_sequence(
            graph, sequence, [], modules, False
        )
        return {str(t): names[node_data[t].final_lib_key] for t in graph}, names

    def test_counter_suffix_collides_with_an_entry_name(self) -> None:
        final, names = self._final_names()
        self.assertEqual(final["//:a"], "libfoo_1.so")
        self.assertEqual(final["//:d"], "libfoo_1.so")
        self.assertLess(len(set(names.values())), len(names))
