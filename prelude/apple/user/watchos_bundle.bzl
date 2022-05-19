load("@fbcode//buck2/prelude/apple:apple_bundle_types.bzl", "AppleBundleInfo")
load("@fbcode//buck2/prelude/user:rule_spec.bzl", "RuleRegistrationSpec")
load(":watch_transition.bzl", "watch_transition")

def _impl(ctx: "context") -> ["provider"]:
    return ctx.attr.actual.providers

registration_spec = RuleRegistrationSpec(
    name = "watchos_bundle",
    implementation = _impl,
    attributes = {
        "actual": attr.dep(providers = [DefaultInfo, AppleBundleInfo]),
    },
    cfg = watch_transition,
)
