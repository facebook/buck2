#!/usr/bin/env python3

import difflib
import os
import sys

if __name__ == "__main__":
    stream = os.popen(sys.argv[1])
    stdout = stream.read().splitlines().sort()
    expected = 'bar_name_override: Object(Object { docs: None, members: [("bar_b", Function(Function { docs: Some(DocString { summary: "bar_b docs", details: None }), params: [], ret: Return { docs: None, typ: Some(Type { raw_type: "Value < \'v >" }) } }))] })\nFoo: Object(Object { docs: None, members: [("foo_a", Function(Function { docs: Some(DocString { summary: "foo_a docs", details: None }), params: [], ret: Return { docs: None, typ: Some(Type { raw_type: "Value < \'v >" }) } }))] })\n'.splitlines().sort()

    assert stdout == expected
