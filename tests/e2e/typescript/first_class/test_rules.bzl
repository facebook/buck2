# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _fake_bundler_tool_impl(ctx: AnalysisContext) -> list[Provider]:
    input_checks = ""
    for required_path in ctx.attrs.required_source_paths:
        input_checks += (
            """
found=false
for root in $source_roots; do
    if test -f "$root/"""
            + required_path
            + """"; then
        cat "$root/"""
            + required_path
            + """" >> "$out_dir/consumed-inputs.txt"
        found=true
    fi
done
test "$found" = true
"""
        )
    if ctx.attrs.required_module_entry:
        input_checks += (
            """
artifact=$(awk -F'"' '/"artifact":/ {print $4; exit}' "$manifest")
test -f "$artifact/"""
            + ctx.attrs.required_module_entry
            + """"
cat "$artifact/"""
            + ctx.attrs.required_module_entry
            + """" >> "$out_dir/consumed-inputs.txt"
"""
        )
    executable = ctx.actions.write(
        "fake-bundler.sh",
        """#!/bin/sh
set -eu
out_dir=
while [ "$#" -gt 0 ]; do
    case "$1" in
        --manifest)
            shift
            manifest="$1"
            ;;
        --out-dir)
            shift
            out_dir="$1"
            ;;
    esac
    shift
done
test -f "$manifest"
mkdir -p "$out_dir"
source_roots=$(awk -F'"' '/"source_root":/ {print $4}' "$manifest")
"""
        + input_checks
        + """
printf '%s\n' "console.log('"""
        + ctx.attrs.message
        + """');" > "$out_dir/bundle.js"
""",
        is_executable = True,
    )
    return [DefaultInfo(default_output = executable), RunInfo(args = cmd_args(executable))]

fake_bundler_tool = rule(
    impl = _fake_bundler_tool_impl,
    attrs = {
        "message": attrs.string(),
        "required_module_entry": attrs.string(default = ""),
        "required_source_paths": attrs.list(attrs.string(), default = []),
    },
)

def _fake_compiler_tool_impl(ctx: AnalysisContext) -> list[Provider]:
    output_directory = "/".join(ctx.attrs.output_entry_point.split("/")[:-1])
    input_checks = ""
    for required_path in ctx.attrs.required_source_paths:
        input_checks += (
            """
found=false
for root in $source_roots; do
    if test -f "$root/"""
            + required_path
            + """"; then
        cat "$root/"""
            + required_path
            + """" >> "$out_dir/consumed-inputs.txt"
        found=true
    fi
done
test "$found" = true
"""
        )
    executable = ctx.actions.write(
        "fake-compiler.sh",
        """#!/bin/sh
set -eu
out_dir=
while [ "$#" -gt 0 ]; do
    case "$1" in
        --manifest)
            shift
            manifest="$1"
            ;;
        --out-dir)
            shift
            out_dir="$1"
            ;;
    esac
    shift
done
test -f "$manifest"
mkdir -p "$out_dir/"""
        + output_directory
        + """"
source_roots=$(awk -F'"' '/"source_root":/ {print $4}' "$manifest")
"""
        + input_checks
        + """
printf '%s\n' "console.log('"""
        + ctx.attrs.message
        + """');" > "$out_dir/"""
        + ctx.attrs.output_entry_point
        + """"
""",
        is_executable = True,
    )
    return [DefaultInfo(default_output = executable), RunInfo(args = cmd_args(executable))]

fake_compiler_tool = rule(
    impl = _fake_compiler_tool_impl,
    attrs = {
        "message": attrs.string(),
        "output_entry_point": attrs.string(),
        "required_source_paths": attrs.list(attrs.string(), default = []),
    },
)

def _fake_runtime_tool_impl(ctx: AnalysisContext) -> list[Provider]:
    executable = ctx.actions.write(
        "fake-runtime.sh",
        """#!/bin/sh
set -eu
test -f \"$1\"
echo '"""
        + ctx.attrs.message
        + """'
""",
        is_executable = True,
    )
    return [DefaultInfo(default_output = executable), RunInfo(args = cmd_args(executable))]

fake_runtime_tool = rule(
    impl = _fake_runtime_tool_impl,
    attrs = {
        "message": attrs.string(),
    },
)
