# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":apple_package_config.bzl", "IpaCompressionLevel")

def apple_package_impl(ctx: "context") -> ["provider"]:
    bundle = ctx.attrs.bundle
    ipa_name = "{}.ipa".format(bundle.label.name)
    app = bundle[DefaultInfo].default_outputs[0]

    payload_dir_name = "Payload"

    payload = ctx.actions.copied_dir(
        payload_dir_name,
        {
            app.basename: app,
        },
    )

    compression_level = _compression_level_arg(IpaCompressionLevel(ctx.attrs._ipa_compression_level))

    package = ctx.actions.declare_output(ipa_name)

    # TODO(T96496412): Add support for SwiftSupport
    # TODO(T110378117): Pull this into a shared zip utility function

    zip = cmd_args(["(cd \"", cmd_args(payload).parent(), "\" && zip -X -r {} - {}) > ".format(compression_level, payload_dir_name), package.as_output()], delimiter = "")
    ctx.actions.run(["sh", "-c", zip], category = "apple_package_zip")

    return [DefaultInfo(default_output = package)]

def _compression_level_arg(compression_level: IpaCompressionLevel.type) -> str.type:
    if compression_level.value == "none":
        return "-0"
    elif compression_level.value == "default":
        return "-6"
    elif compression_level.value == "min":
        return "-1"
    elif compression_level.value == "max":
        return "-9"
    else:
        fail("Unknown .ipa compression level: " + compression_level)
