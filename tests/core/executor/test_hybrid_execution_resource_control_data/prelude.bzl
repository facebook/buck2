# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

prefer_local = read_root_config("test", "prefer_local", "false").lower() == "true"

def _impl(ctx) -> list[Provider]:
    outputs = []
    for i in range(1, 100):
        output = ctx.actions.declare_output("output{}.txt".format(i))
        outputs.append(output)
        ctx.actions.run(["sh", "-c", 'head -c 10 /dev/urandom > "$1"', "--", output.as_output()], category = "my_action{}".format(i), prefer_local = prefer_local)

    merged = ctx.actions.declare_output("output.txt")
    cmd = cmd_args("cat", outputs, ">", merged.as_output(), delimiter = " ")
    ctx.actions.run(["sh", "-c", cmd], category = "merge", prefer_local = prefer_local)

    return [
        DefaultInfo(merged),
    ]

china = rule(impl = _impl, attrs = {})

def _execution_platform(ctx):
    return [
        DefaultInfo(),
        ExecutionPlatformInfo(
            label = ctx.label.raw_target(),
            configuration = ConfigurationInfo(constraints = {}, values = {}),
            executor_config = CommandExecutorConfig(
                local_enabled = True,
                remote_enabled = True,
                remote_execution_properties = {
                    "platform": "linux-remote-execution",
                },
                remote_execution_use_case = "buck2-default",
            ),
        ),
    ]

execution_platform = rule(
    impl = _execution_platform,
    attrs = {},
)

def _execution_platforms(ctx):
    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(
            platforms = [p[ExecutionPlatformInfo] for p in ctx.attrs.platforms],
        ),
    ]

execution_platforms = rule(
    impl = _execution_platforms,
    attrs = {
        "platforms": attrs.list(attrs.dep(providers = [ExecutionPlatformInfo])),
    },
)
