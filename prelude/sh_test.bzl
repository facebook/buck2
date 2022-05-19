def sh_test_impl(ctx):
    # This does not exist in v1 either, but v1 has those attributes presumably
    # to be compatible with this. We just fail if they're passed.
    if ctx.attr.list_args or ctx.attr.list_env or ctx.attr.run_args or ctx.attr.run_env:
        fail("An unsupported attribute was passed")

    args = cmd_args()

    if ctx.attr.test != None:
        if type(ctx.attr.test) == "artifact":
            args.add(ctx.attr.test)
        elif type(ctx.attr.test) == "dependency":
            run_info = ctx.attr.test[RunInfo]
            if run_info != None:
                args.add(run_info.args)
            else:
                info = ctx.attr.test[DefaultInfo]
                args.add(info.default_outputs).hidden(info.other_outputs)
        else:
            fail("Unexpected type for test attribute")

    args.hidden(ctx.attr.resources)

    command = [args] + ctx.attr.args

    # TODO support default info and runinfo properly by writing a sh script that invokes the command properly

    return [
        DefaultInfo(default_outputs = []),
        RunInfo(args = cmd_args(command)),
        ExternalRunnerTestInfo(
            type = ctx.attr.type or "custom",
            command = command,
            env = ctx.attr.env,
            labels = ctx.attr.labels,
            contacts = ctx.attr.contacts,
            use_templated_api = False,
        ),
    ]
