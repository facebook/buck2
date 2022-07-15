load(":tr.bzl", "tr")

def _my_java_library(ctx):
    _ = ctx
    fail("we don't build it in test")

my_java_library = rule(
    impl = _my_java_library,
    cfg = tr,
    attrs = {
        "java_version": attrs.int(),
    },
)
