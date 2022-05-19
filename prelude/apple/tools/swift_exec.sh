#!/bin/bash

# Apply a debug prefix map for the current directory
# to make debug info relocatable.
exec "$@" -debug-prefix-map "$PWD"=.
