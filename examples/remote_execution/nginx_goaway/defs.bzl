# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _cached_write_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        cmd_args(
            "sh",
            "-c",
            'head -c "$1" /dev/urandom > "$2"',
            "--",
            str(ctx.attrs.size),
            out.as_output(),
        ),
        category = "cached_write",
        allow_cache_upload = True,
    )
    return [DefaultInfo(default_output = out)]

# Writes `size` random bytes locally and uploads the result to the remote
# cache, so that the next build after `buck2 clean` downloads it.
cached_write = rule(impl = _cached_write_impl, attrs = {"size": attrs.int()})

def _platforms_impl(ctx: AnalysisContext) -> list[Provider]:
    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = ConfigurationInfo(constraints = {}, values = {}),
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = False,
            remote_cache_enabled = True,
            allow_cache_uploads = True,
        ),
    )
    return [DefaultInfo(), ExecutionPlatformRegistrationInfo(platforms = [platform])]

# Local execution with a remote action cache and CAS.
platforms = rule(impl = _platforms_impl, attrs = {})
