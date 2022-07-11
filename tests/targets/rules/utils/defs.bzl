load(
    "@fbcode//buck2/prelude/utils:build_target_pattern.bzl",
    "label_matches_build_target_pattern",
    "parse_build_target_pattern",
)
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def _test_parse_build_target_pattern_impl(ctx):
    for pattern, expected_result in ctx.attr.patterns.items():
        parsed_pattern = parse_build_target_pattern(pattern)
        expect(parsed_pattern.kind.value == expected_result["kind"], "Kind does not match for parsed pattern `{}`: {}".format(pattern, parsed_pattern.kind.value))
        expect(parsed_pattern.cell == expected_result["cell"], "Cell does not match for parsed pattern `{}`: {}".format(pattern, parsed_pattern.cell))
        expect(parsed_pattern.path == expected_result["path"], "Path does not match for parsed pattern `{}`: {}".format(pattern, parsed_pattern.path))
        expect(parsed_pattern.name == expected_result["name"], "Name does not match for parsed pattern `{}`: {}".format(pattern, parsed_pattern.name))
    return [DefaultInfo()]

test_parse_build_target_pattern = rule(
    impl = _test_parse_build_target_pattern_impl,
    attrs = {
        "patterns": attr.dict(key = attr.string(), value = attr.dict(key = attr.string(), value = attr.option(attr.string()))),
    },
)

def _test_label_matches_build_target_pattern(ctx):
    for pattern, expected_result in ctx.attr.patterns.items():
        parsed_pattern = parse_build_target_pattern(pattern)
        matches = label_matches_build_target_pattern(ctx.label, parsed_pattern)
        expect(matches == expected_result, "Unexpected matching result for build target pattern: `{}` returned {}".format(pattern, matches))
    return [DefaultInfo()]

test_label_matches_build_target_pattern = rule(
    impl = _test_label_matches_build_target_pattern,
    attrs = {
        "patterns": attr.dict(key = attr.string(), value = attr.bool()),
    },
)

def _test_parse_build_target_pattern_fail(ctx):
    parsed_pattern = parse_build_target_pattern(ctx.attr.pattern)
    fail("Successfully managed to parse bad build target pattern `{}`: {}".format(
        ctx.attr.pattern,
        parsed_pattern,
    ))

test_parse_build_target_pattern_fail = rule(
    impl = _test_parse_build_target_pattern_fail,
    attrs = {
        "pattern": attr.string(),
    },
)

def _test_expect(ctx):
    if ctx.attr.fail:
        if ctx.attr.arg_1 and ctx.attr.arg_2:
            expect(False, ctx.attr.message, ctx.attr.arg_1, ctx.attr.arg_2)
        elif ctx.attr.arg_1:
            expect(False, ctx.attr.message, ctx.attr.arg_1)
        else:
            expect(False, ctx.attr.message)
    return [DefaultInfo()]

test_expect = rule(
    impl = _test_expect,
    attrs = {
        "arg_1": attr.option(attr.string(), default = None),
        "arg_2": attr.option(attr.string(), default = None),
        "fail": attr.bool(),
        "message": attr.option(attr.string(), default = None),
    },
)
