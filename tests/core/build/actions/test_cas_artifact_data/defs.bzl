# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _cas_artifact_out_of_range_expiration_impl(ctx):
    out = ctx.actions.declare_output("out", has_content_based_path = False)
    ctx.actions.cas_artifact(
        out.as_output(),
        # sha1 of the empty file; the digest is parsed before the timestamp so it must be valid
        "da39a3ee5e6b4b0d3255bfef95601890afd80709:0",
        "buck2-testing",
        expires_after_timestamp = 1 << 62,
    )
    return [DefaultInfo(default_output = out)]

cas_artifact_out_of_range_expiration = rule(
    impl = _cas_artifact_out_of_range_expiration_impl,
    attrs = {},
)
