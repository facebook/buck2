load("@fbcode//buck2/prelude/java:java_providers.bzl", "JavaLibraryInfo", "JavaPackagingInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def _extract_expected_artifacts(ctx, use_abi: "bool"):
    expected_deps = []
    for dep in ctx.attrs.expected_deps:
        java_library_info = dep[JavaLibraryInfo]
        if java_library_info:
            library_output = java_library_info.library_output
            if library_output:
                expected_deps.append(library_output.abi if use_abi else library_output.full_library)
        else:
            expected_deps.append(dep[DefaultInfo].default_outputs[0])
    return expected_deps

def _compare_deps(deps_type, actual_deps, expected_deps):
    expect(len(actual_deps) == len(expected_deps), "Fail when comparing {} deps. Expected {}, got {}", deps_type, expected_deps, actual_deps)
    for dep in actual_deps:
        expect(dep in expected_deps, "Fail when comparing {} deps. Expected {}, got {}", deps_type, expected_deps, actual_deps)

def _assert_packaging_deps_impl(ctx):
    packaging_deps = ctx.attrs.actual_target[JavaPackagingInfo].packaging_deps
    actual_deps = [packaging_dep.jar for packaging_dep in (list(packaging_deps.traverse()) if packaging_deps else []) if packaging_dep.jar]

    expected_deps = _extract_expected_artifacts(ctx, use_abi = False)
    _compare_deps("packaging", actual_deps, expected_deps)

    return [DefaultInfo()]

assert_packaging_deps = rule(
    impl = _assert_packaging_deps_impl,
    attrs = {
        "actual_target": attrs.dep(),
        "expected_deps": attrs.list(attrs.dep(), default = []),
    },
)

def _assert_compiling_deps_impl(ctx):
    actual_deps = []
    expected_suffix = ctx.attrs.expected_suffix
    compiling_deps = ctx.attrs.actual_target[JavaLibraryInfo].compiling_deps
    for dep in list(compiling_deps.traverse()) if compiling_deps else []:
        actual_dep = dep.abi
        if expected_suffix:
            basename = actual_dep.basename
            expect(basename.endswith(expected_suffix), "Expected base name: {} should end with {}", basename, expected_suffix)

        actual_deps.append(actual_dep)

    expected_deps = _extract_expected_artifacts(ctx, use_abi = True)
    _compare_deps("compiling", actual_deps, expected_deps)

    return [DefaultInfo()]

assert_compiling_deps = rule(
    impl = _assert_compiling_deps_impl,
    attrs = {
        "actual_target": attrs.dep(),
        "expected_deps": attrs.list(attrs.dep(), default = []),
        "expected_suffix": attrs.string(default = ""),
    },
)
