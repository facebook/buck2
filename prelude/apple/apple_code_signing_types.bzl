# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Provider which exposes a field from `apple_binary` to `apple_bundle` as it might be used during code signing.
AppleEntitlementsInfo = provider(fields = {
    "entitlements_file": provider_field(Artifact | None, default = None),
})

CodeSignType = enum(
    "skip",
    "adhoc",
    "distribution",
)

CodeSignConfiguration = enum(
    "dry-run",
    "fast-adhoc",
    "none",
)

def get_code_signing_configuration_attr_value(ctx: AnalysisContext) -> [CodeSignConfiguration, None]:
    configuration_value = ctx.attrs.code_signing_configuration or ctx.attrs._code_signing_configuration
    return CodeSignConfiguration(configuration_value)
