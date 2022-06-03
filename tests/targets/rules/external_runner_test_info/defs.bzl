def _test_impl(ctx):
    cli = cmd_args(ctx.attr.script, ctx.attr.artifact)

    # Relative paths AND running from cell root is a bit of an oddball
    # combination and we don't make it particularly easy to use. Rule authors
    # should probably not use this.
    if ctx.attr.use_project_relative_paths and not ctx.attr.run_from_project_root:
        cli.relative_to(ctx.label.cell_root)

    re_config = CommandExecutorConfig(
        local_enabled = False,
        remote_enabled = True,
        remote_execution_properties = {
            "platform": "linux-remote-execution",
        },
    )

    default_executor = re_config if ctx.attr.set_default_executor else None

    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            type = "custom",
            command = [cli],
            use_project_relative_paths = ctx.attr.use_project_relative_paths,
            run_from_project_root = ctx.attr.run_from_project_root,
            labels = ctx.attr.labels,
            default_executor = default_executor,
            executor_overrides = {
                "re-linux": re_config,
            },
        ),
    ]

test = rule(implementation = _test_impl, attrs = {
    "artifact": attr.source(),
    "labels": attr.list(attr.string(), default = []),
    "run_from_project_root": attr.option(attr.bool()),
    "script": attr.source(),
    "set_default_executor": attr.option(attr.bool()),
    "use_project_relative_paths": attr.option(attr.bool()),
})
