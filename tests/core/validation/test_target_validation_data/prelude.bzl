# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx) -> list[Provider]:
    flute = ctx.actions.write_json("flute.json", {
        "data": {
            "message": "Here I am describing the failure reason" if ctx.attrs.fail else None,
            "status": "failure" if ctx.attrs.fail else "success",
        },
        "version": 1,
    }, pretty = True)
    return [
        DefaultInfo(),
        RunInfo(args = ["echo", "hello"]),
        ExternalRunnerTestInfo(type = "dummy", command = ["true"]),
        InstallInfo(installer = ctx.attrs.installer, files = {"random_file": flute}),
        ValidationInfo(
            validations = [
                ValidationSpec(
                    name = "whistle",
                    validation_result = flute,
                    optional = ctx.attrs.optional,
                ),
            ],
        ),
    ]

china = rule(impl = _impl, attrs = {
    "dep": attrs.option(attrs.dep(), default = None),
    "fail": attrs.bool(default = False),
    "installer": attrs.default_only(attrs.label(default = "//:my_installer")),
    "optional": attrs.bool(default = False),
})

def _installer_impl(ctx) -> list[Provider]:
    return [
        DefaultInfo(),
        RunInfo(args = ["python3", ctx.attrs.main]),
    ]

installer = rule(impl = _installer_impl, attrs = {
    "main": attrs.source(),
})
