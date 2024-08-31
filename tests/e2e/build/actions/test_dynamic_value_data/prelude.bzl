# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

MyInfo = provider(fields = {"hgfd": int})

def _produce_dynamic_value_impl(actions, artifacts, dynamic_values, outputs, arg):
    _ignore = (actions, artifacts, dynamic_values, outputs, arg)  # buildifier: disable=unused-variable
    return [
        MyInfo(hgfd = 123),
    ]

_produce_dynamic_value = dynamic_actions(impl = _produce_dynamic_value_impl)

def _consume_dynamic_value_impl(actions, artifacts, dynamic_values, outputs, arg):
    _ignore = (actions, artifacts)  # buildifier: disable=unused-variable
    (out, v) = arg
    value = dynamic_values[v].providers[MyInfo].hgfd
    actions.write(outputs[out], "<<<{}>>>".format(value))
    return []

_consume_dynamic_value = dynamic_actions(impl = _consume_dynamic_value_impl)

def _test_rule(ctx):
    v = ctx.actions.dynamic_output_new(_produce_dynamic_value(
        dynamic = [],
        outputs = [],
        arg = None,
    ))

    out = ctx.actions.declare_output("poiuy")
    ctx.actions.dynamic_output_new(_consume_dynamic_value(
        dynamic = [],
        outputs = [out.as_output()],
        dynamic_values = [v],
        arg = (out, v),
    ))

    return [DefaultInfo(default_output = out)]

test_rule = rule(
    impl = _test_rule,
    attrs = {},
)
