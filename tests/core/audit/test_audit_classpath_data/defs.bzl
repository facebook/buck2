def _classpath_with_content_based_path_impl(ctx):
    out = ctx.actions.declare_output("out.txt", uses_experimental_content_based_path_hashing = True)
    out = ctx.actions.write(out, "out")
    return [DefaultInfo(default_output = out), TemplatePlaceholderInfo(keyed_variables = {"classpath": cmd_args(out)})]

classpath_with_content_based_path = rule(
    impl = _classpath_with_content_based_path_impl,
    attrs = {},
)
