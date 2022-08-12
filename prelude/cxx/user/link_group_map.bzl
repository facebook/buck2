load("@fbcode//buck2/prelude:attributes.bzl", "Linkage", "Traversal")
load("@fbcode//buck2/prelude/user:rule_spec.bzl", "RuleRegistrationSpec")

def _impl(_ctx: "context") -> ["provider"]:
    return [
        DefaultInfo(),
    ]

registration_spec = RuleRegistrationSpec(
    name = "link_group_map",
    impl = _impl,
    attrs = {
        "map": attrs.list(attrs.tuple(attrs.string(), attrs.list(attrs.tuple(attrs.dep(), attrs.enum(Traversal), attrs.option(attrs.string()), attrs.option(attrs.enum(Linkage)))))),
    },
)
