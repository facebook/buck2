#!/usr/bin/env python3

import difflib
import os
import sys

if __name__ == "__main__":
    stream = os.popen(sys.argv[1])
    stdout = sorted(stream.read().splitlines())
    expected = sorted(
        [
            "`` bar_name_override: bar_b",
            "`namespaced` baz_name_override: baz_b",
            "`` Foo: foo_a",
        ]
    )
    assert stdout == expected
