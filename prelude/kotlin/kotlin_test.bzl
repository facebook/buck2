load("@fbcode//buck2/prelude/java:java_test.bzl", "build_junit_test")
load("@fbcode//buck2/prelude/kotlin:kotlin_library.bzl", "build_kotlin_library")

def kotlin_test_impl(ctx: "context") -> ["provider"]:
    (
        tests_java_library_info,
        tests_java_packaging_info,
        _shared_library_info,
        _cxx_resources_info,
        template_placeholder_info,
        default_info,
    ) = build_kotlin_library(ctx, ctx.attr.srcs)
    external_runner_test_info, run_info = build_junit_test(ctx, tests_java_library_info, tests_java_packaging_info)

    return [
        tests_java_library_info,
        template_placeholder_info,
        default_info,
        external_runner_test_info,
        run_info,
    ]
