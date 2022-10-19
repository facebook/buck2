load(
    "@prelude//utils:build_target_pattern.bzl",
    "label_matches_build_target_pattern",
    "parse_build_target_pattern",
)
load("@prelude//utils:set_record.bzl", "set", "set_type")
load("@prelude//utils:utils.bzl", "expect")

def _test_parse_build_target_pattern_impl(ctx):
    for pattern, expected_result in ctx.attrs.patterns.items():
        parsed_pattern = parse_build_target_pattern(pattern)
        expect(parsed_pattern.kind.value == expected_result["kind"], "Kind does not match for parsed pattern `{}`: {}".format(pattern, parsed_pattern.kind.value))
        expect(parsed_pattern.cell == expected_result["cell"], "Cell does not match for parsed pattern `{}`: {}".format(pattern, parsed_pattern.cell))
        expect(parsed_pattern.path == expected_result["path"], "Path does not match for parsed pattern `{}`: {}".format(pattern, parsed_pattern.path))
        expect(parsed_pattern.name == expected_result["name"], "Name does not match for parsed pattern `{}`: {}".format(pattern, parsed_pattern.name))
    return [DefaultInfo()]

test_parse_build_target_pattern = rule(
    impl = _test_parse_build_target_pattern_impl,
    attrs = {
        "patterns": attrs.dict(key = attrs.string(), value = attrs.dict(key = attrs.string(), value = attrs.option(attrs.string()))),
    },
)

def _test_label_matches_build_target_pattern(ctx):
    for pattern, expected_result in ctx.attrs.patterns.items():
        parsed_pattern = parse_build_target_pattern(pattern)
        matches = label_matches_build_target_pattern(ctx.label, parsed_pattern)
        expect(matches == expected_result, "Unexpected matching result for build target pattern: `{}` returned {}".format(pattern, matches))
    return [DefaultInfo()]

test_label_matches_build_target_pattern = rule(
    impl = _test_label_matches_build_target_pattern,
    attrs = {
        "patterns": attrs.dict(key = attrs.string(), value = attrs.bool()),
    },
)

def _test_parse_build_target_pattern_fail(ctx):
    parsed_pattern = parse_build_target_pattern(ctx.attrs.pattern)
    fail("Successfully managed to parse bad build target pattern `{}`: {}".format(
        ctx.attrs.pattern,
        parsed_pattern,
    ))

test_parse_build_target_pattern_fail = rule(
    impl = _test_parse_build_target_pattern_fail,
    attrs = {
        "pattern": attrs.string(),
    },
)

def _test_expect(ctx):
    if ctx.attrs.fail:
        if ctx.attrs.arg_1 and ctx.attrs.arg_2:
            expect(False, ctx.attrs.message, ctx.attrs.arg_1, ctx.attrs.arg_2)
        elif ctx.attrs.arg_1:
            expect(False, ctx.attrs.message, ctx.attrs.arg_1)
        else:
            expect(False, ctx.attrs.message)
    return [DefaultInfo()]

test_expect = rule(
    impl = _test_expect,
    attrs = {
        "arg_1": attrs.option(attrs.string(), default = None),
        "arg_2": attrs.option(attrs.string(), default = None),
        "fail": attrs.bool(),
        "message": attrs.option(attrs.string(), default = None),
    },
)

# Note, the type() will always return "record", but the typing system maps "set_type" to "set_record.type"
# and errors if a different type is used.
def _set_type_helper(test_set: set_type) -> None:
    expect(type(test_set) == "record", "Expected the type for the set to be record, got {} instead.", type(test_set))

def _test_set(_ctx):
    # @lint-ignore BUCKRESTRICTEDSYNTAX
    test_set = set()

    test_set.add(1)

    expect(test_set.size() == 1, "Wrong size for set, got {} when expecting 1", test_set.size())

    expect(test_set.add(1) == True, "Expected return value of True for add")

    expect(test_set.contains(2) == False, "Expected return value of False for contains")

    expect(test_set.remove(3) == False, "Expected return value of False for remove")

    test_set.add(4)
    expect(test_set.remove(4) == True, "Expected return value of True for remove")

    expect(test_set.list() == [1], "Expected set list to be [1], got {}", test_set.list())

    added_values = test_set.update([1, 5, 6])
    expect(added_values == [5, 6], "Expected update to return [5, 6], but got {}", added_values)
    expect(test_set.size() == 3, "Wrong size for set, got {} when expecting 3", test_set.size())
    expect(test_set.list() == [1, 5, 6], "Expected set list to be [1, 5 , 6], got {}", test_set.list())

    _set_type_helper(test_set)

    return [DefaultInfo()]

test_set = rule(
    impl = _test_set,
    attrs = {},
)
