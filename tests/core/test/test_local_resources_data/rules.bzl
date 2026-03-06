# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _broker_impl(ctx):
    json = ctx.actions.write_json("resources.json", {
        "resources": [{"my_alias": "42"}],
    })
    return [
        DefaultInfo(),
        LocalResourceInfo(
            setup = cmd_args(["cat", json]),
            resource_env_vars = {
                "MY_RESOURCE_ID": "my_alias",
            },
            setup_timeout_seconds = 5,
        ),
    ]

_broker_attrs = {}

broker = rule(impl = _broker_impl, attrs = _broker_attrs)

def _test_impl(ctx):
    return [DefaultInfo(), ExternalRunnerTestInfo(
        type = "custom",
        command = ["true"],
        local_resources = {
            "my_resource_type": ctx.attrs.broker.label,
        },
        required_local_resources = [
            RequiredTestLocalResource("my_resource_type"),
        ],
    )]

_test_attrs = {
    "broker": attrs.dep(providers = [LocalResourceInfo]),
}

test = rule(impl = _test_impl, attrs = _test_attrs)

def _daemon_broker_impl(_ctx):
    # Setup command that backgrounds a long-lived process (simulating a real
    # resource broker), prints JSON with its PID, and exits. Without the fix,
    # cgroup cleanup would kill the backgrounded process when setup exits.
    return [
        DefaultInfo(),
        LocalResourceInfo(
            setup = cmd_args([
                "sh",
                "-c",
                'sleep 300 & PID=$!; echo "{\\"pid\\": $PID, \\"resources\\": [{\\"broker_pid\\": \\"$PID\\"}]}"',
            ]),
            resource_env_vars = {
                "BROKER_PID": "broker_pid",
            },
            setup_timeout_seconds = 5,
        ),
    ]

daemon_broker = rule(impl = _daemon_broker_impl, attrs = {})

def _daemon_test_impl(ctx):
    # Test that checks the broker process is still alive via kill -0.
    # Fails if cgroup cleanup killed the broker.
    return [DefaultInfo(), ExternalRunnerTestInfo(
        type = "custom",
        command = ["sh", "-c", "kill -0 $BROKER_PID"],
        local_resources = {
            "daemon_resource": ctx.attrs.broker.label,
        },
        required_local_resources = [
            RequiredTestLocalResource("daemon_resource"),
        ],
    )]

daemon_test = rule(impl = _daemon_test_impl, attrs = {
    "broker": attrs.dep(providers = [LocalResourceInfo]),
})
