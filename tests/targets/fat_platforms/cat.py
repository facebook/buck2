#!/usr/bin/env python3
import sys

out = ""
for f in sys.argv[2:]:
    with open(f) as f:
        out += f.read()

print(out, file=open(sys.argv[1], "w"))
