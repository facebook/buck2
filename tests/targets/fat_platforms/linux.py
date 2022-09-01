#!/usr/bin/env python3
import sys

if sys.platform not in ("linux", "linux2"):
    print("bad platform `%s`" % sys.platform)
    sys.exit(1)

print("linux")
