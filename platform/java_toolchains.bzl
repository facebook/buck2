load("@fbcode//buck2/platform:utils.bzl", "optional_binary_or_source_attr", "source_list_attr", "string_attr", "string_list_attr")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JUnitToolchainInfo", "JavaPlatformInfo", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/java/utils:java_utils.bzl", "derive_javac")

_buckconfig_java_toolchain_attrs = {
    "bootclasspath_7": (source_list_attr, []),
    "bootclasspath_8": (source_list_attr, []),
    "javac": (optional_binary_or_source_attr, "fbsource//third-party/toolchains/jdk:javac"),
    "source_level": (string_attr, ""),
    "src_roots": (string_list_attr, ""),
    "target_level": (string_attr, ""),
}

def config_backed_java_toolchain(
        name,
        class_abi_generator = "buck//src/com/facebook/buck/jvm/java/abi:api-stubber",
        fat_jar_main_class_lib = "buck//src/com/facebook/buck/jvm/java:fat-jar-main",
        **kwargs):
    kwargs["class_abi_generator"] = class_abi_generator
    kwargs["compile_and_package"] = "fbsource//xplat/buck2/tools/java:compile_and_package"
    kwargs["src_dir_helper"] = "fbsource//xplat/buck2/tools/java:src_dir_helper"
    kwargs["fat_jar"] = "fbsource//xplat/buck2/tools/java:fat_jar"
    kwargs["fat_jar_main_class_lib"] = fat_jar_main_class_lib
    kwargs["jar"] = "fbsource//third-party/toolchains/jdk:jar"
    kwargs["java"] = "fbsource//third-party/toolchains/jdk:java"

    sections = ["java", "tools"]
    for (key, (info, default)) in _buckconfig_java_toolchain_attrs.items():
        val = None
        for section in sections:
            val = info.reader(section, key)
            if val != None:
                break
        if val == None:
            val = default
        if val != None:
            kwargs[key] = val

    _config_backed_java_toolchain_rule(
        name = name,
        **kwargs
    )

def _config_backed_java_toolchain_rule_impl(ctx):
    src_root_elements, src_root_prefixes = _parse_src_roots(ctx.attr.src_roots)
    return [
        DefaultInfo(),
        JavaPlatformInfo(
            name = ctx.attr.name,
        ),
        JavaToolchainInfo(
            bootclasspath_7 = ctx.attr.bootclasspath_7,
            bootclasspath_8 = ctx.attr.bootclasspath_8,
            class_abi_generator = ctx.attr.class_abi_generator,
            compile_and_package = ctx.attr.compile_and_package,
            src_dir_helper = ctx.attr.src_dir_helper,
            fat_jar = ctx.attr.fat_jar,
            fat_jar_main_class_lib = ctx.attr.fat_jar_main_class_lib,
            jar = ctx.attr.jar[RunInfo],
            java = ctx.attr.java[RunInfo],
            javac = derive_javac(ctx.attr.javac),
            source_level = ctx.attr.source_level,
            src_root_elements = src_root_elements,
            src_root_prefixes = src_root_prefixes,
            target_level = ctx.attr.target_level,
        ),
    ]

_config_backed_java_toolchain_rule = rule(
    attrs = {
        "bootclasspath_7": attr.list(attr.source()),
        "bootclasspath_8": attr.list(attr.source()),
        "class_abi_generator": attr.option(attr.dep(providers = [RunInfo]), default = None),
        "compile_and_package": attr.dep(),
        "fat_jar": attr.dep(),
        "fat_jar_main_class_lib": attr.option(attr.source(), default = None),
        "jar": attr.dep(providers = [RunInfo]),
        "java": attr.dep(providers = [RunInfo]),
        "javac": attr.option(attr.one_of(attr.dep(), attr.source(), attr.string()), default = None),
        "source_level": attr.string(),
        "src_dir_helper": attr.dep(),
        "src_roots": attr.list(attr.string()),
        "target_level": attr.string(),
    },
    implementation = _config_backed_java_toolchain_rule_impl,
)

def _parse_src_roots(src_roots: [str.type]) -> ([str.type], [str.type]):
    prefixes = []
    elements = []
    for src_root in src_roots:
        if src_root.startswith("/"):
            if not src_root.endswith("/"):
                fail("Elements in java.src_roots config that begin with a / must end in one too, but {} does not".format(src_root))
            prefixes.append(src_root[1:])
        elif "/" in src_root:
            fail("No / is permitted in java.src_roots config elements, but {} has one".format(src_root))
        else:
            elements.append(src_root)

    return elements, prefixes

def junit_toolchain(name, **kwargs):
    kwargs["junit_test_runner_library_jar"] = "buck//src/com/facebook/buck/testrunner:testrunner-bin-fixed"
    kwargs["junit_test_runner_main_class"] = "com.facebook.buck.testrunner.JUnitMain"
    kwargs["list_class_names"] = "fbsource//xplat/buck2/tools/java:list_class_names"

    _junit_toolchain_rule(name = name, **kwargs)

def _junit_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        JUnitToolchainInfo(
            junit_test_runner_library_jar = ctx.attr.junit_test_runner_library_jar[DefaultInfo].default_outputs[0],
            junit_test_runner_main_class = ctx.attr.junit_test_runner_main_class,
            list_class_names = ctx.attr.list_class_names,
        ),
    ]

_junit_toolchain_rule = rule(
    attrs = {
        "junit_test_runner_library_jar": attr.dep(),
        "junit_test_runner_main_class": attr.string(),
        "list_class_names": attr.dep(providers = [RunInfo]),
    },
    implementation = _junit_toolchain_rule_impl,
)
