# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load(
    "@prelude//utils:build_target_pattern.bzl",
    "parse_build_target_pattern",
)

AppleSelectiveDebuggingInfo = provider(fields = [
    "scrub_binary",  # function
    "include_build_target_patterns",  # BuildTargetPattern.type
    "include_regular_expressions",  # regex
    "exclude_build_target_patterns",  # BuildTargetPattern.type
    "exclude_regular_expressions",  # regex
])

# The type of selective debugging json input to utilze.
_SelectiveDebuggingJsonTypes = [
    # Use a targets json file containing all targets to include.
    "targets",
    # Use a spec json file specifying the targets to include
    # and exclude via build target patterns and regular expressions.
    "spec",
]

_SelectiveDebuggingJsonType = enum(
    _SelectiveDebuggingJsonTypes[0],
    _SelectiveDebuggingJsonTypes[1],
)

def _impl(ctx: "context") -> ["provider"]:
    json_type = _SelectiveDebuggingJsonType(ctx.attrs.json_type)

    # process inputs and provide them up the graph with typing
    include_build_target_patterns = [parse_build_target_pattern(pattern) for pattern in ctx.attrs.include_build_target_patterns]
    include_regular_expressions = [experimental_regex(expression) for expression in ctx.attrs.include_regular_expressions]
    exclude_build_target_patterns = [parse_build_target_pattern(pattern) for pattern in ctx.attrs.exclude_build_target_patterns]
    exclude_regular_expressions = [experimental_regex(expression) for expression in ctx.attrs.exclude_regular_expressions]

    scrubber = ctx.attrs._apple_tools[AppleToolsInfo].selective_debugging_scrubber

    cmd = cmd_args(scrubber)
    if json_type == _SelectiveDebuggingJsonType("targets"):
        # If a targets json file is not provided, write an empty json file:
        targets_json_file = ctx.attrs.targets_json_file or ctx.actions.write_json("targets_json.txt", {"targets": []})
        cmd.add("--targets-file")
        cmd.add(targets_json_file)
    elif json_type == _SelectiveDebuggingJsonType("spec"):
        json_data = {
            "exclude_build_target_patterns": ctx.attrs.exclude_build_target_patterns,
            "exclude_regular_expressions": ctx.attrs.exclude_regular_expressions,
            "include_build_target_patterns": ctx.attrs.include_build_target_patterns,
            "include_regular_expressions": ctx.attrs.include_regular_expressions,
        }
        spec_file = ctx.actions.write_json("selective_debugging_spec.json", json_data)
        cmd.add("--spec-file")
        cmd.add(spec_file)
    else:
        fail("Expected json_type to be either `targets` or `spec`.")

    def scrub_binary(inner_ctx, executable: "artifact") -> "artifact":
        inner_cmd = cmd_args(cmd)
        output = inner_ctx.actions.declare_output("debug_scrubbed/{}".format(executable.short_path))
        inner_cmd.add(["--input", executable])
        inner_cmd.add(["--output", output.as_output()])
        inner_ctx.actions.run(inner_cmd, category = "scrub_binary", identifier = executable.short_path)
        return output

    return [
        DefaultInfo(),
        AppleSelectiveDebuggingInfo(
            scrub_binary = scrub_binary,
            include_build_target_patterns = include_build_target_patterns,
            include_regular_expressions = include_regular_expressions,
            exclude_build_target_patterns = exclude_build_target_patterns,
            exclude_regular_expressions = exclude_regular_expressions,
        ),
    ]

registration_spec = RuleRegistrationSpec(
    name = "apple_selective_debugging",
    impl = _impl,
    attrs = {
        "exclude_build_target_patterns": attrs.list(attrs.string(), default = []),
        "exclude_regular_expressions": attrs.list(attrs.string(), default = []),
        "include_build_target_patterns": attrs.list(attrs.string(), default = []),
        "include_regular_expressions": attrs.list(attrs.string(), default = []),
        "json_type": attrs.enum(_SelectiveDebuggingJsonTypes),
        "targets_json_file": attrs.option(attrs.source(), default = None),
        "_apple_tools": attrs.exec_dep(default = "fbsource//xplat/buck2/platform/apple:apple-tools", providers = [AppleToolsInfo]),
    },
)

def filter_debug_info(debug_info: "transitive_set_iterator", selective_debugging_info: AppleSelectiveDebuggingInfo.type) -> ["artifact"]:
    selected_debug_info = []
    for info in debug_info:
        if selective_debugging_info.include_build_target_patterns or selective_debugging_info.include_regular_expressions:
            is_included = _check_if_label_matches_patterns_or_expressions(info.label, selective_debugging_info.include_build_target_patterns, selective_debugging_info.include_regular_expressions)
        else:
            is_included = True

        if is_included and not _check_if_label_matches_patterns_or_expressions(info.label, selective_debugging_info.exclude_build_target_patterns, selective_debugging_info.exclude_regular_expressions):
            selected_debug_info.extend(info.artifacts)
    return selected_debug_info

def _check_if_label_matches_patterns_or_expressions(label: "label", patterns: ["BuildTargetPattern"], expressions: ["regex"]) -> bool.type:
    for pattern in patterns:
        if pattern.matches(label):
            return True
    for expression in expressions:
        if expression.match(str(label)):
            return True
    return False
