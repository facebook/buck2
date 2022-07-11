#!/usr/bin/env python3

import difflib
import os
import sys

if __name__ == "__main__":
    stream = os.popen(sys.argv[1])
    stdout = sorted(stream.read().splitlines())
    expected = sorted(
        [
            '`` bar_name_override: Object(Object { docs: None, members: [("bar_b", Function(Function { docs: Some(DocString { summary: "bar_b docs", details: None }), params: [], ret: Return { docs: None, typ: Some(Type { raw_type: "\\"\\"" }) } }))] })',
            '`namespaced` baz_name_override: Object(Object { docs: None, members: [("baz_b", Function(Function { docs: Some(DocString { summary: "baz_b docs", details: None }), params: [], ret: Return { docs: None, typ: Some(Type { raw_type: "\\"\\"" }) } }))] })',
            '`` Foo: Object(Object { docs: None, members: [("foo_a", Function(Function { docs: Some(DocString { summary: "foo_a docs", details: None }), params: [], ret: Return { docs: None, typ: Some(Type { raw_type: "\\"\\"" }) } }))] })',
        ]
    )
    assert stdout == expected
