# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//platforms/apple:constants.bzl", "APPLE_PLATFORMS_KEY")

def add_apple_platforms_attr(attributes):
    # Add _apple_platforms to all rules so that we may query the target platform to use until we support configuration
    # modifiers and can use them to set the configuration to use for operations.
    # Map of string identifer to platform.
    attributes[APPLE_PLATFORMS_KEY] = attrs.dict(key = attrs.string(), value = attrs.dep(), sorted = False, default = {})
    return attributes
