# A library of utilities for asserting properties about the outputs

def _assert_artifact_properties(ctx: "context") -> [DefaultInfo.type]:
    dep = ctx.attrs.src[DefaultInfo].default_outputs[0]
    if ctx.attrs.short_path != None:
        if ctx.attrs.short_path != dep.short_path:
            fail("Wrong short_path, expected {}, got {}".format(ctx.attrs.short_path, dep.short_path))
    if ctx.attrs.basename != None:
        if ctx.attrs.basename != dep.basename:
            fail("Wrong basename, expected {}, got {}".format(ctx.attrs.basename, dep.basename))
    return [DefaultInfo()]

# Assert that a given `src` artifact have specific properties
assert_artifact_properties = rule(
    impl = _assert_artifact_properties,
    attrs = {
        "basename": attrs.option(attrs.string(), default = None),
        "short_path": attrs.option(attrs.string(), default = None),
        "src": attrs.dep(),
    },
)

def _assert_exists(ctx: "context") -> [DefaultInfo.type]:
    out = ctx.actions.declare_output("out")
    cp = cmd_args(["test -f \"", ctx.attrs.path, "\" && echo 1 > ", out.as_output()], delimiter = "")
    ctx.actions.run(["sh", "-c", cp], category = "assert")
    return [DefaultInfo(default_outputs = [out])]

# Assert that a file exists
assert_exists = rule(
    impl = _assert_exists,
    attrs = {
        "path": attrs.arg(),
    },
)
