def apple_package_impl(ctx: "context") -> ["provider"]:
    bundle = ctx.attr.bundle
    ipa_name = "{}.ipa".format(bundle.label.name)
    app = bundle[DefaultInfo].default_outputs[0]

    payload = ctx.actions.copy_file("Payload", app)

    package = ctx.actions.declare_output(ipa_name)

    # TODO(T96496412): Add support for compression levels and SwiftSupport
    # TODO(T110378117): Pull this into a shared zip utility function
    cmd = cmd_args(["zip", "-X", "-r", package.as_output(), payload])
    ctx.actions.run(cmd, category = "apple_package_zip")

    return [DefaultInfo(default_outputs = [package])]
