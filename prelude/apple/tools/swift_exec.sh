#!/bin/bash

# - Apply a debug prefix map for the current directory
# to make debug info relocatable.
# - Use $TMPDIR for the module cache location. This
# will be set to a unique location for each RE action
# which will avoid sharing modules across RE actions.
# This is necessary as the inputs to the modules will
# be transient and can be removed at any point, causing
# module validation errors to fail builds.
exec "$@" -debug-prefix-map "$PWD"=. -module-cache-path "$TMPDIR"/module-cache
