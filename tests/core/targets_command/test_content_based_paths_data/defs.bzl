def _write_with_content_based_path_impl(ctx):
    out = ctx.actions.declare_output("out.txt", uses_experimental_content_based_path_hashing = True)
    return [DefaultInfo(default_output = ctx.actions.write(out, "out"))]

write_with_content_based_path = rule(
    impl = _write_with_content_based_path_impl,
    attrs = {},
)
