# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

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
