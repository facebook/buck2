load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidResourceInfo", "merge_android_packageable_info", "merge_exported_android_resource_info")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/android:r_dot_java.bzl", "get_dummy_r_dot_java")
load("@fbcode//buck2/prelude/java:java_library.bzl", "build_java_library")
load("@fbcode//buck2/prelude/java:java_providers.bzl", "to_list")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/kotlin:kotlin_library.bzl", "build_kotlin_library")
load("@fbcode//buck2/prelude/utils:utils.bzl", "filter_and_map_idx")

def android_library_impl(ctx: "context") -> ["provider"]:
    java_providers = build_android_library(ctx)

    return to_list(java_providers) + [
        merge_android_packageable_info(
            ctx.label,
            ctx.actions,
            ctx.attrs.deps + (ctx.attrs.deps_query or []) + ctx.attrs.exported_deps + ctx.attrs.runtime_deps,
            manifest = ctx.attrs.manifest,
        ),
        merge_exported_android_resource_info(ctx.attrs.exported_deps),
    ]

def build_android_library(
        ctx: "context",
        r_dot_java: ["artifact", None] = None) -> "JavaProviders":
    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    bootclasspath_entries = [] + ctx.attrs._android_toolchain[AndroidToolchainInfo].android_bootclasspath
    additional_classpath_entries = []

    # If we were given an R.java to compile against, use that. Otherwise, just create a "dummy" R.java.
    if r_dot_java:
        additional_classpath_entries.append(r_dot_java)
    else:
        dummy_r_dot_java = _get_dummy_r_dot_java(ctx, java_toolchain)
        if dummy_r_dot_java:
            additional_classpath_entries.append(dummy_r_dot_java)

    if ctx.attrs.language != None and ctx.attrs.language.lower() == "kotlin":
        return build_kotlin_library(
            ctx,
            additional_classpath_entries = additional_classpath_entries,
            bootclasspath_entries = bootclasspath_entries,
        )
    else:
        return build_java_library(
            ctx,
            ctx.attrs.srcs,
            additional_classpath_entries = additional_classpath_entries,
            bootclasspath_entries = bootclasspath_entries,
        )

def _get_dummy_r_dot_java(
        ctx: "context",
        java_toolchain: "JavaToolchainInfo") -> ["artifact", None]:
    android_resources = [resource for resource in filter_and_map_idx(
        AndroidResourceInfo,
        ctx.attrs.deps + (ctx.attrs.deps_query or []) + ctx.attrs.provided_deps + (getattr(ctx.attrs, "provided_deps_query", []) or []),
    ) if resource.res != None]
    if len(android_resources) == 0:
        return None

    dummy_r_dot_java_library_info = get_dummy_r_dot_java(
        ctx,
        ctx.attrs._android_toolchain[AndroidToolchainInfo].merge_android_resources[RunInfo],
        java_toolchain,
        android_resources,
        ctx.attrs.resource_union_package,
    )

    return dummy_r_dot_java_library_info.library_output.abi
