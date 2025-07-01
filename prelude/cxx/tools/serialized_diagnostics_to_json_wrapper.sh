#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Wrapper script to convert serialized diagnostics output to JSON
# for Buck error handler consumption. Usage:
#  serialized_diagnostics_to_json_wrapper.sh <serialized_diags_to_json> <output_json> compile args
serialized_diags_to_json="$1"
shift
output_json="$1"
output_serialized="$1.dia"
shift
"$@" --serialize-diagnostics "$output_serialized"
ret=$?
if [ $ret -eq 0 ]; then
    # We always need to output a JSON file, even if the compiler succeeded.
    echo "[]" > "$output_json"
else
    $serialized_diags_to_json "$output_serialized" > "$output_json"
fi
exit $ret
