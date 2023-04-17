# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load(
    "@prelude//utils:build_target_pattern.bzl",
    "parse_build_target_pattern",
)

AppleFocusedDebuggingInfo = provider(fields = [
    "include_build_target_patterns",  # BuildTargetPattern.type
    "include_regular_expressions",  # regex
    "exclude_build_target_patterns",  # BuildTargetPattern.type
    "exclude_regular_expressions",  # regex
])

def _impl(ctx: "context") -> ["provider"]:
    # process inputs and provide them up the graph with typing
    include_build_target_patterns = [parse_build_target_pattern(pattern) for pattern in ctx.attrs.include_build_target_patterns]
    include_regular_expressions = [experimental_regex(expression) for expression in ctx.attrs.include_regular_expressions]
    exclude_build_target_patterns = [parse_build_target_pattern(pattern) for pattern in ctx.attrs.exclude_build_target_patterns]
    exclude_regular_expressions = [experimental_regex(expression) for expression in ctx.attrs.exclude_regular_expressions]

    json_data = {
        "exclude_build_target_patterns": [pattern.as_string() for pattern in exclude_build_target_patterns],
        "exclude_regular_expressions": [str(expression) for expression in exclude_regular_expressions],
        "include_build_target_patterns": [pattern.as_string() for pattern in include_build_target_patterns],
        "include_regular_expressions": [str(expression) for expression in include_regular_expressions],
    }

    scrubber_run_info = ctx.attrs.scrubber.get(RunInfo)
    if scrubber_run_info == None:
        scrubber_run_info = RunInfo(
            args = ctx.attrs.scrubber[DefaultInfo].default_outputs,
        )

    spec_file = ctx.actions.write_json("focused_debugging_spec.json", json_data)
    args = cmd_args([
        scrubber_run_info,
        "--spec-file",
        spec_file,
    ])

    return [
        DefaultInfo(),
        # In order to conform to the link_postprocessor API, the rule must provide a RunInfo
        RunInfo(args = args),
        AppleFocusedDebuggingInfo(
            include_build_target_patterns = include_build_target_patterns,
            include_regular_expressions = include_regular_expressions,
            exclude_build_target_patterns = exclude_build_target_patterns,
            exclude_regular_expressions = exclude_regular_expressions,
        ),
    ]

registration_spec = RuleRegistrationSpec(
    name = "apple_focused_debugging",
    impl = _impl,
    attrs = {
        "exclude_build_target_patterns": attrs.list(attrs.string(), default = []),
        "exclude_regular_expressions": attrs.list(attrs.string(), default = []),
        "include_build_target_patterns": attrs.list(attrs.string(), default = []),
        "include_regular_expressions": attrs.list(attrs.string(), default = []),
        "scrubber": attrs.dep(),
    },
)

def filter_debug_info(debug_info: "transitive_set_iterator", focused_debugging_info: AppleFocusedDebuggingInfo.type) -> ["artifact"]:
    selected_debug_info = []
    for info in debug_info:
        if focused_debugging_info.include_build_target_patterns or focused_debugging_info.include_regular_expressions:
            is_included = _check_if_label_matches_patterns_or_expressions(info.label, focused_debugging_info.include_build_target_patterns, focused_debugging_info.include_regular_expressions)
        else:
            is_included = True

        if is_included and not _check_if_label_matches_patterns_or_expressions(info.label, focused_debugging_info.exclude_build_target_patterns, focused_debugging_info.exclude_regular_expressions):
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
