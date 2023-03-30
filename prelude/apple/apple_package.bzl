# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(":apple_package_config.bzl", "IpaCompressionLevel")

def apple_package_impl(ctx: "context") -> ["provider"]:
    ipa_contents = _get_ipa_contents(ctx)
    compression_level = _compression_level_arg(IpaCompressionLevel(ctx.attrs._ipa_compression_level))

    package = ctx.actions.declare_output("{}.ipa".format(ctx.attrs.bundle.label.name))

    # TODO(T110378117): Pull this into a shared zip utility function
    zip = cmd_args(["(cd \"", cmd_args(ipa_contents), "\" && zip -X -r {} - .) > ".format(compression_level), package.as_output()], delimiter = "")
    ctx.actions.run(["sh", "-c", zip], category = "apple_package_zip")

    return [DefaultInfo(default_output = package)]

def _get_ipa_contents(ctx) -> "artifact":
    # TODO(T96496412): Add support for SwiftSupport
    app = ctx.attrs.bundle[DefaultInfo].default_outputs[0]
    return ctx.actions.copied_dir(
        "__unzipped_ipa_contents__",
        {
            paths.join("Payload", app.basename): app,
        },
    )

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
        fail("Unknown .ipa compression level: " + str(compression_level))
