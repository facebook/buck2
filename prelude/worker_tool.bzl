load("@prelude//utils:utils.bzl", "expect")

WorkerToolInfo = provider(
    fields = [
        "command",  # "cmd_args"
    ],
)

def worker_tool(ctx: "context") -> ["provider"]:
    """
     worker_tool() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers (DefaultInfo with an empty output and TemplatePlaceholderInfo with $(worker) macro key)
    """

    executable = ctx.attrs.exe
    worker_tool_run_info = executable[RunInfo]
    expect(worker_tool_run_info != None, "Worker tool executable must have a RunInfo!")

    worker_tool_runner = ctx.attrs._worker_tool_runner[RunInfo]
    worker_tool_cmd = cmd_args(worker_tool_runner)
    worker_tool_cmd.add("--worker-tool")
    worker_tool_cmd.add(worker_tool_run_info)

    worker_args = ctx.attrs.args
    if worker_args:
        worker_args_file, worker_macro_file = ctx.actions.write(
            "worker_tool_args",
            worker_args,
            allow_args = True,
        )

        worker_tool_cmd.add("--worker-args-file")
        worker_tool_cmd.add(worker_args_file)
        worker_tool_cmd.hidden([worker_macro_file])

    worker_env = ctx.attrs.env
    if worker_env:
        env_args = []
        for key, value in worker_env.items():
            env_args.append(key)
            env_args.append(value)

        env_args_file, env_macro_file = ctx.actions.write(
            "worker_tool_envs",
            env_args,
            allow_args = True,
        )

        worker_tool_cmd.add("--worker-env-file")
        worker_tool_cmd.add(env_args_file)
        worker_tool_cmd.hidden([env_macro_file])

    return [
        DefaultInfo(default_outputs = []),
        TemplatePlaceholderInfo(keyed_variables = {
            "worker": worker_tool_cmd,
        }),
        WorkerToolInfo(
            command = worker_tool_cmd,
        ),
    ]
