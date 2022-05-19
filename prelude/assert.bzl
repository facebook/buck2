# A library of utilities for asserting properties about the outputs

def _assert_artifact_properties(ctx: "context") -> [DefaultInfo.type]:
    dep = ctx.attr.src[DefaultInfo].default_outputs[0]
    if ctx.attr.short_path != None:
        if ctx.attr.short_path != dep.short_path:
            fail("Wrong short_path, expected {}, got {}".format(ctx.attr.short_path, dep.short_path))
    if ctx.attr.basename != None:
        if ctx.attr.basename != dep.basename:
            fail("Wrong basename, expected {}, got {}".format(ctx.attr.basename, dep.basename))
    return [DefaultInfo()]

# Assert that a given `src` artifact have specific properties
assert_artifact_properties = rule(
    implementation = _assert_artifact_properties,
    attrs = {
        "basename": attr.option(attr.string(), default = None),
        "short_path": attr.option(attr.string(), default = None),
        "src": attr.dep(),
    },
)

def _assert_exists(ctx: "context") -> [DefaultInfo.type]:
    out = ctx.actions.declare_output("out")
    cp = cmd_args(["test -f \"", ctx.attr.path, "\" && echo 1 > ", out.as_output()], delimiter = "")
    ctx.actions.run(["sh", "-c", cp], category = "assert")
    return [DefaultInfo(default_outputs = [out])]

# Assert that a file exists
assert_exists = rule(
    implementation = _assert_exists,
    attrs = {
        "path": attr.arg(),
    },
)
