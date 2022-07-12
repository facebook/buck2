load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load("@fbcode//buck2/prelude/user:rule_spec.bzl", "RuleRegistrationSpec")

def _impl(ctx: "context") -> ["provider"]:
    return [
        DefaultInfo(),
        AppleToolsInfo(
            assemble_bundle = ctx.attr.assemble_bundle[RunInfo],
            info_plist_processor = ctx.attr.info_plist_processor[RunInfo],
            make_modulemap = ctx.attr.make_modulemap[RunInfo],
            make_vfsoverlay = ctx.attr.make_vfsoverlay[RunInfo],
            swift_objc_header_postprocess = ctx.attr.swift_objc_header_postprocess[RunInfo],
        ),
    ]

# The `apple_tools` rule exposes a set of supplementary tools
# required by the Apple rules _internally_. Such tools are not
# toolchain/SDK specific, they're just internal helper tools.
registration_spec = RuleRegistrationSpec(
    name = "apple_tools",
    impl = _impl,
    attrs = {
        "assemble_bundle": attr.dep(providers = [RunInfo]),
        "info_plist_processor": attr.dep(providers = [RunInfo]),
        "make_modulemap": attr.dep(providers = [RunInfo]),
        "make_vfsoverlay": attr.dep(providers = [RunInfo]),
        "swift_objc_header_postprocess": attr.dep(providers = [RunInfo]),
    },
)
