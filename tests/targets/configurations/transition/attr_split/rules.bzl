load(":tr.bzl", "cpu_split_transition")

MyArtifact = provider(fields = ["test_data"])

def _assert(cond):
    if not cond:
        fail("assertion failed")

def _binary_impl(ctx):
    # Check deps come as dict from split transition label to providers.
    [lib_deps] = ctx.attrs.native_deps
    _assert(type(lib_deps) == type({}))
    _assert("a64" == lib_deps["arm64"][MyArtifact].test_data)
    _assert("a32" == lib_deps["arm32"][MyArtifact].test_data)
    return [
        DefaultInfo(),
    ]

def _library_impl(ctx):
    # This is not how configurations are meant to be used, we are testing internals here.
    if "/attr_split:code (arm32-" in str(ctx.label):
        test_data = "a32"
    elif "/attr_split:code (arm64-" in str(ctx.label):
        test_data = "a64"
    else:
        fail("unknown configuration")
    return [
        DefaultInfo(),
        MyArtifact(test_data = test_data),
    ]

my_android_binary = rule(impl = _binary_impl, attrs = {
    "native_deps": attr.list(attr.split_transition_dep(cfg = cpu_split_transition)),
})

my_cxx_library = rule(impl = _library_impl, attrs = {})
