def _tr(platform, refs, attrs):
    if attrs.java_version != 14:
        fail("java_version must be 14 in this test")
    _ = refs
    return platform

tr = transition(
    _tr,
    refs = {},
    attrs = [
        "java_version",
    ],
)
