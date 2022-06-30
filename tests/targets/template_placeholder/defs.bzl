def _args(cmd_args, v):
    cmd_args.add(v)

CombinerTset = transitive_set(
    args_projections = {
        "": _args,
    },
)

CombinerInfo = provider(fields = ["tset"])

def _combiner(ctx):
    output = ctx.actions.write("out", "")

    # the tset node's value will be our own output and the tset of ctx.attr.extra_value if present
    # note that the normal way of combining tsets is by putting them in children and we want to be
    # able to explicitly test when a tset appears within the node value
    value = cmd_args(output)
    if ctx.attr.extra_value != None:
        value.add(ctx.attr.extra_value[CombinerInfo].tset.project_as_args(""))
    children = [c[CombinerInfo].tset for c in ctx.attr.children]
    tset = ctx.actions.tset(CombinerTset, value = value, children = children)
    return [
        DefaultInfo(),
        CombinerInfo(
            tset = tset,
        ),
        TemplatePlaceholderInfo(
            keyed_variables = {
                "classpath_including_targets_with_no_output": tset.project_as_args(""),
            },
        ),
    ]

combiner = rule(
    implementation = _combiner,
    attrs = {
        "children": attr.list(attr.dep(), default = []),
        "extra_value": attr.option(attr.dep()),
    },
)

def test(query_deps, expected_deps):
    query_labels = {v.label: True for v in query_deps}
    expected_labels = {v.label: True for v in expected_deps}
    expected_len = len(expected_labels)
    query_len = len(query_labels)
    if expected_len != query_len:
        return "different length ({} != {})".format(expected_len, query_len)
    for x in expected_labels:
        if not x in query_labels:
            return "missing expected item `{}`".format(x)

    for x in query_labels:
        if not x in expected_labels:
            return "additional item `{}`".format(x)

    return None

def _tester(ctx):
    res = test(ctx.attr.query, ctx.attr.expected)
    if res:
        fail("failed due to {}. expected `{}`, got `{}`".format(res, ctx.attr.expected, ctx.attr.query))

    return [DefaultInfo()]

tester = rule(
    implementation = _tester,
    attrs = {
        "expected": attr.list(attr.dep()),
        "query": attr.query(),
    },
)
