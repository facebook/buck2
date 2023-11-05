# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _tests(ctx):
    # Create a large stdout stream locally, and upload it to CAS.
    # The limit for inline stdout is 50KiB. So this will force calling client.upload_blob.
    stage0 = ctx.actions.declare_output("stage0")
    ctx.actions.run(
        ["sh", "-c", 'yes abcdefghijklmnopqrstuvwxyz | head -c 65536 && echo done > "$1"', "--", stage0.as_output()],
        category = "stage0",
        local_only = True,
        allow_cache_upload = True,
    )

    return [DefaultInfo(stage0)]

tests = rule(attrs = {}, impl = _tests)
