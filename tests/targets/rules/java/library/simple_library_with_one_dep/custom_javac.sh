#!/bin/bash
set -eu
echo "Hello from custom javac script!!!"
/usr/local/java-runtime/impl/11/bin/javac "$@"
