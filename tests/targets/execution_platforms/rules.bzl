def _execution_platforms_impl(ctx):
    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = ctx.attr.configuration[ConfigurationInfo],
        local_enabled = True,
        remote_enabled = True,
        remote_execution_properties = {
            "platform": "linux-remote-execution",
        },
        remote_execution_max_input_files_mebibytes = 1,
        use_limited_hybrid = True,
    )

    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(
            platforms = [platform],
        ),
    ]

execution_platforms = rule(
    implementation = _execution_platforms_impl,
    attrs = {
        "configuration": attr.dep(providers = [ConfigurationInfo]),
    },
)

def _configuration_impl(_ctx):
    return [
        DefaultInfo(),
        ConfigurationInfo(constraints = {}, values = {}),
    ]

configuration = rule(
    implementation = _configuration_impl,
    attrs = {},
)

def _file_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        [
            "sh",
            "-c",
            'head -c "$1" /dev/urandom > $2',
            "--",
            str(ctx.attr.file_size),
            out.as_output(),
        ],
        category = "head",
        local_only = True,
    )
    return [DefaultInfo(default_outputs = [out])]

file = rule(implementation = _file_impl, attrs = {"file_size": attr.int()})

def _cp_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        [
            "cp",
            ctx.attr.file[DefaultInfo].default_outputs[0],
            out.as_output(),
        ],
        category = "cp",
    )
    return [DefaultInfo(default_outputs = [out])]

cp = rule(implementation = _cp_impl, attrs = {"file": attr.dep()})
