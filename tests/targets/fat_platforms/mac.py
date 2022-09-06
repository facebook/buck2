#!/usr/bin/env python3
import sys

if sys.platform != "darwin":
    print("bad platform `%s`" % sys.platform)
    sys.exit(1)

print("darwin")
