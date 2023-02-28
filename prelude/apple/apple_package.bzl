# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

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

    package = ctx.actions.declare_output(ipa_name)

    # TODO(T96496412): Add support for compression levels and SwiftSupport
    # TODO(T110378117): Pull this into a shared zip utility function

    zip = cmd_args(["(cd \"", cmd_args(payload).parent(), "\" && zip -X -r - {}) > ".format(payload_dir_name), package.as_output()], delimiter = "")
    ctx.actions.run(["sh", "-c", zip], category = "apple_package_zip")

    return [DefaultInfo(default_output = package)]
