load("@fbcode//buck2/prelude/java:java_test.bzl", "build_junit_test")
load("@fbcode//buck2/prelude/kotlin:kotlin_library.bzl", "build_kotlin_library")

def kotlin_test_impl(ctx: "context") -> ["provider"]:
    java_providers = build_kotlin_library(ctx, ctx.attrs.srcs)
    external_runner_test_info, run_info = build_junit_test(ctx, java_providers.java_library_info, java_providers.java_packaging_info)

    return [
        java_providers.java_library_info,
        java_providers.java_packaging_info,
        java_providers.template_placeholder_info,
        java_providers.default_info,
        external_runner_test_info,
        run_info,
    ]
