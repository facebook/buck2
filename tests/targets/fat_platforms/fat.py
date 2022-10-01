#!/usr/bin/env python3
import argparse
import subprocess
import sys

parser = argparse.ArgumentParser()
parser.add_argument("--mac")
parser.add_argument("--linux")
parser.add_argument("output")
args = parser.parse_args()

binary = None
if sys.platform == "darwin":
    binary = args.mac
elif sys.platform in ("linux", "linux2"):
    binary = args.linux
else:
    print("unsupported platform {}".format(sys.platform))

output = subprocess.check_output(binary)
print(output, file=open(args.output, "w"))
