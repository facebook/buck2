load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _impl(ctx: "context") -> ["provider"]:
    return [
        DefaultInfo(),
        AppleToolsInfo(
            assemble_bundle = ctx.attrs.assemble_bundle[RunInfo],
            info_plist_processor = ctx.attrs.info_plist_processor[RunInfo],
            make_modulemap = ctx.attrs.make_modulemap[RunInfo],
            make_vfsoverlay = ctx.attrs.make_vfsoverlay[RunInfo],
            swift_objc_header_postprocess = ctx.attrs.swift_objc_header_postprocess[RunInfo],
        ),
    ]

# The `apple_tools` rule exposes a set of supplementary tools
# required by the Apple rules _internally_. Such tools are not
# toolchain/SDK specific, they're just internal helper tools.
registration_spec = RuleRegistrationSpec(
    name = "apple_tools",
    impl = _impl,
    attrs = {
        "assemble_bundle": attrs.dep(providers = [RunInfo]),
        "info_plist_processor": attrs.dep(providers = [RunInfo]),
        "make_modulemap": attrs.dep(providers = [RunInfo]),
        "make_vfsoverlay": attrs.dep(providers = [RunInfo]),
        "swift_objc_header_postprocess": attrs.dep(providers = [RunInfo]),
    },
)
