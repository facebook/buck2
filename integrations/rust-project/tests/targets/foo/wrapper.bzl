load("@prelude//decls:rust_rules.bzl", _prelude_rust_test = "rust_test")

def _rust_test_impl(ctx: AnalysisContext) -> list[Provider]:
    return _prelude_rust_test.impl(ctx)

rust_test = rule(
    impl = _rust_test_impl,
    attrs = _prelude_rust_test.attrs,
    uses_plugins = _prelude_rust_test.uses_plugins,
    supports_incoming_transition = _prelude_rust_test.supports_incoming_transition,
)
