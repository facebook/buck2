ExecutorConfigInfo = provider(fields = ["config"])

def _platform(ctx):
    # We need to introduce a constraint to ensure our different execution
    # platforms are distinct. This is because exec_compatible_with selects a
    # ConfigurationInfo (which provides a config), not a ExecutionPlatformInfo
    # (instead it matches on it).
    configuration = ConfigurationInfo(
        constraints = {
            ctx.attrs.setting.label.raw_target(): ConstraintValueInfo(
                setting = ctx.attrs.setting[ConstraintSettingInfo],
                label = ctx.label.raw_target(),
            ),
        },
        values = {},
    )

    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = configuration,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = True,
            remote_execution_properties = {
                "platform": "linux-remote-execution",
            },
            remote_execution_max_input_files_mebibytes = 1,
            use_limited_hybrid = ctx.attrs.use_limited_hybrid,
            allow_limited_hybrid_fallbacks = ctx.attrs.allow_hybrid_fallbacks_on_failure,
            allow_hybrid_fallbacks_on_failure = ctx.attrs.allow_hybrid_fallbacks_on_failure,
            remote_execution_use_case = "buck2-default",
            allow_cache_uploads = ctx.attrs.allow_cache_uploads,
        ),
    )

    return [
        DefaultInfo(),
        platform,
        configuration,
    ]

platform = rule(
    impl = _platform,
    attrs = {
        "allow_cache_uploads": attrs.bool(default = False),
        "allow_hybrid_fallbacks_on_failure": attrs.bool(default = False),
        "setting": attrs.configuration_label(),
        "use_limited_hybrid": attrs.bool(default = True),
    },
)

def _platforms(ctx):
    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(
            platforms = [x[ExecutionPlatformInfo] for x in ctx.attrs.platforms],
        ),
    ]

platforms = rule(
    impl = _platforms,
    attrs = {
        "platforms": attrs.list(attrs.dep(providers = [ExecutionPlatformInfo])),
    },
)

def _target_platform(ctx):
    return [
        DefaultInfo(),
        PlatformInfo(
            label = str(ctx.label.raw_target()),
            configuration = ConfigurationInfo(constraints = {}, values = {}),
        ),
    ]

target_platform = rule(
    impl = _target_platform,
    attrs = {},
)

def _config_setting(ctx):
    return [DefaultInfo(), ConstraintSettingInfo(label = ctx.label.raw_target())]

config_setting = rule(
    impl = _config_setting,
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
            str(ctx.attrs.file_size),
            out.as_output(),
        ],
        category = "head",
        local_only = True,
    )
    return [DefaultInfo(default_outputs = [out])]

file = rule(impl = _file_impl, attrs = {"file_size": attrs.int()})

def _cp_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        [
            "cp",
            ctx.attrs.file[DefaultInfo].default_outputs[0],
            out.as_output(),
        ],
        category = "cp",
    )
    return [DefaultInfo(default_outputs = [out])]

cp = rule(impl = _cp_impl, attrs = {"file": attrs.dep()})

def _command_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        [
            ctx.attrs.command,
            out.as_output(),
        ],
        category = "command",
    )
    return [DefaultInfo(default_outputs = [out])]

command = rule(impl = _command_impl, attrs = {"command": attrs.source()})

def _write_impl(ctx):
    # NOTE: This uses an action so that we can exercise local uploads.
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        [
            "sh",
            "-c",
            'echo -n "$1" > "$2"',
            "--",
            ctx.attrs.text,
            out.as_output(),
        ],
        category = "write",
        local_only = ctx.attrs.local_only,
        allow_cache_upload = ctx.attrs.allow_cache_upload,
    )

    return [DefaultInfo(default_outputs = [out])]

write = rule(
    impl = _write_impl,
    attrs = {
        "allow_cache_upload": attrs.bool(default = False),
        "local_only": attrs.bool(default = False),
        "text": attrs.string(),
    },
)
