def postprocess(ctx, input, postprocessor):
    output = ctx.actions.declare_output("postprocessed/{}".format(input.short_path))
    cmd = cmd_args()
    cmd.add(postprocessor)
    cmd.add(["--input", input])
    cmd.add(["--output", output.as_output()])
    ctx.actions.run(cmd, category = "link_postprocess", identifier = input.short_path)
    return output
