# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

cache_buster = read_config("test", "cache_buster", "")

def _impl(ctx) -> list[Provider]:
    fast = ctx.actions.declare_output("validation.json")
    ctx.actions.run(["python3", ctx.attrs.fast, fast.as_output()], env = {
        "cache_buster": cache_buster,
    }, category = "fast")
    slow = ctx.actions.declare_output("out")
    ctx.actions.run(["python3", ctx.attrs.slow, slow.as_output()], env = {
        "cache_buster": cache_buster,
    }, category = "slow")
    return [
        DefaultInfo(slow),
        ValidationInfo(
            validations = [
                ValidationSpec(
                    name = "validation",
                    validation_result = fast,
                ),
            ],
        ),
    ]

china = rule(impl = _impl, attrs = {
    "fast": attrs.source(),
    "slow": attrs.source(),
})
