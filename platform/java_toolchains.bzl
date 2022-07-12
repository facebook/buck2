load("@fbcode//buck2/platform:utils.bzl", "optional_binary_or_source_attr", "source_list_attr", "string_attr", "string_list_attr")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "AbiGenerationMode", "JUnitToolchainInfo", "JavaPlatformInfo", "JavaToolchainInfo", "JavacProtocol", "PrebuiltJarToolchainInfo")
load("@fbcode//buck2/prelude/java/utils:java_utils.bzl", "derive_javac")

_buckconfig_java_toolchain_attrs = {
    "abi_generation_mode": (string_attr, "class"),
    "bootclasspath_7": (source_list_attr, []),
    "bootclasspath_8": (source_list_attr, []),
    # There's special default handling for `javac` below.
    "javac": (optional_binary_or_source_attr, None),
    "source_level": (string_attr, ""),
    "src_roots": (string_list_attr, ""),
    "target_level": (string_attr, ""),
}

DEFAULT_ABI_GENERATOR = "buck//src/com/facebook/buck/jvm/java/abi:api-stubber"
AST_DUMPER = "fbsource//xplat/buck2/tools/java/dump_ast:dump_ast"
FAT_JAR_MAIN_CLASS_LIB = "buck//src/com/facebook/buck/jvm/java:fat-jar-main"

def config_backed_java_toolchain(
        name,
        is_bootstrap_toolchain = False,
        javac = None,
        java = None,
        java_for_tests = None,
        visibility = None):
    # Set params which we don't read from configs.
    kwargs = {}
    kwargs["compile_and_package"] = "fbsource//xplat/buck2/tools/java:compile_and_package"
    kwargs["merge_to_jar"] = "fbsource//xplat/buck2/tools/java:merge_to_jar"
    kwargs["src_dir_helper"] = "fbsource//xplat/buck2/tools/java:src_dir_helper"
    kwargs["fat_jar"] = "fbsource//xplat/buck2/tools/java:fat_jar"
    kwargs["jar"] = "fbsource//third-party/toolchains/jdk:jar"
    kwargs["java"] = "fbsource//third-party/toolchains/jdk:java"
    kwargs["fallback_javac"] = "fbsource//third-party/toolchains/jdk:javac"

    # Now pull in values from config (overriding defaults).
    sections = ["java", "tools"]
    for (key, (info, default)) in _buckconfig_java_toolchain_attrs.items():
        if key in kwargs:
            continue

        val = None
        for section in sections:
            val = info.reader(section, key)
            if val != None:
                break
        if val == None:
            val = default
        if val != None:
            kwargs[key] = val

    # Add in rule-level params last, which override everything.
    if java != None:
        kwargs["java"] = java
    if java_for_tests != None:
        kwargs["java_for_tests"] = java_for_tests
    if javac != None:
        kwargs["javac"] = javac

    kwargs["is_bootstrap_toolchain"] = is_bootstrap_toolchain
    if is_bootstrap_toolchain:
        # set none for tools that don't used for bootstrap toolchain
        kwargs["ast_dumper"] = None
        kwargs["class_abi_generator"] = None
        kwargs["fat_jar_main_class_lib"] = None
    else:
        kwargs["ast_dumper"] = AST_DUMPER
        kwargs["class_abi_generator"] = DEFAULT_ABI_GENERATOR
        kwargs["fat_jar_main_class_lib"] = FAT_JAR_MAIN_CLASS_LIB

    if "javac" in kwargs:
        kwargs["javac_protocol"] = "classic"
    else:
        # if javac isn't set, we use the buck1 "internal" javac
        kwargs["javac"] = "buck//src/com/facebook/buck/jvm/java/stepsbuilder/javacd/main:javacd_tool"
        kwargs["javac_protocol"] = "javacd"

    _config_backed_java_toolchain_rule(
        name = name,
        visibility = visibility,
        **kwargs
    )

def _config_backed_java_toolchain_rule_impl(ctx):
    src_root_elements, src_root_prefixes = _parse_src_roots(ctx.attrs.src_roots)
    abi_generation_mode = ctx.attrs.abi_generation_mode.lower()

    return [
        DefaultInfo(),
        JavaPlatformInfo(
            name = ctx.attrs.name,
        ),
        JavaToolchainInfo(
            abi_generation_mode = AbiGenerationMode(abi_generation_mode),
            ast_dumper = ctx.attrs.ast_dumper,
            bootclasspath_7 = ctx.attrs.bootclasspath_7,
            bootclasspath_8 = ctx.attrs.bootclasspath_8,
            class_abi_generator = ctx.attrs.class_abi_generator,
            compile_and_package = ctx.attrs.compile_and_package,
            fallback_javac = derive_javac(ctx.attrs.fallback_javac),
            merge_to_jar = ctx.attrs.merge_to_jar,
            src_dir_helper = ctx.attrs.src_dir_helper,
            fat_jar = ctx.attrs.fat_jar,
            fat_jar_main_class_lib = ctx.attrs.fat_jar_main_class_lib,
            jar = ctx.attrs.jar[RunInfo],
            java = ctx.attrs.java,
            java_for_tests = ctx.attrs.java_for_tests[RunInfo] if ctx.attrs.java_for_tests else ctx.attrs.java[RunInfo],
            javac = derive_javac(ctx.attrs.javac),
            javac_protocol = ctx.attrs.javac_protocol,
            source_level = ctx.attrs.source_level,
            src_root_elements = src_root_elements,
            src_root_prefixes = src_root_prefixes,
            target_level = ctx.attrs.target_level,
            is_bootstrap_toolchain = ctx.attr.is_bootstrap_toolchain,
        ),
    ]

_config_backed_java_toolchain_rule = rule(
    attrs = {
        "abi_generation_mode": attr.enum(["class", "source", "source_only"], default = "class"),
        "ast_dumper": attr.option(attr.dep(), default = None),
        "bootclasspath_7": attr.list(attr.source()),
        "bootclasspath_8": attr.list(attr.source()),
        "class_abi_generator": attr.option(attr.dep(providers = [RunInfo]), default = None),
        "compile_and_package": attr.dep(),
        "fallback_javac": attr.option(attr.one_of(attr.dep(), attr.source(), attr.string()), default = None),
        "fat_jar": attr.dep(),
        "fat_jar_main_class_lib": attr.option(attr.source(), default = None),
        "is_bootstrap_toolchain": attr.bool(default = False),
        "jar": attr.dep(providers = [RunInfo]),
        "java": attr.dep(),
        "java_for_tests": attr.option(attr.dep(providers = [RunInfo])),
        "javac": attr.option(attr.one_of(attr.dep(), attr.source(), attr.string()), default = None),
        "javac_protocol": attr.enum(JavacProtocol.values(), default = "classic"),
        "merge_to_jar": attr.dep(),
        "source_level": attr.string(),
        "src_dir_helper": attr.dep(),
        "src_roots": attr.list(attr.string()),
        "target_level": attr.string(),
    },
    impl = _config_backed_java_toolchain_rule_impl,
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
            junit_test_runner_library_jar = ctx.attrs.junit_test_runner_library_jar[DefaultInfo].default_outputs[0],
            junit_test_runner_main_class = ctx.attrs.junit_test_runner_main_class,
            list_class_names = ctx.attrs.list_class_names,
        ),
    ]

_junit_toolchain_rule = rule(
    attrs = {
        "junit_test_runner_library_jar": attr.dep(),
        "junit_test_runner_main_class": attr.string(),
        "list_class_names": attr.dep(providers = [RunInfo]),
    },
    impl = _junit_toolchain_rule_impl,
)

def prebuilt_jar_toolchain(name, is_bootstrap_toolchain = False, visibility = None):
    kwargs = {}
    kwargs["is_bootstrap_toolchain"] = is_bootstrap_toolchain
    kwargs["class_abi_generator"] = None if is_bootstrap_toolchain else DEFAULT_ABI_GENERATOR
    _prebuilt_jar_toolchain_rule(name = name, visibility = visibility, **kwargs)

def _prebuilt_jar_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        PrebuiltJarToolchainInfo(
            class_abi_generator = ctx.attrs.class_abi_generator,
            is_bootstrap_toolchain = ctx.attrs.is_bootstrap_toolchain,
        ),
    ]

_prebuilt_jar_toolchain_rule = rule(
    attrs = {
        "class_abi_generator": attr.option(attr.dep(providers = [RunInfo]), default = None),
        "is_bootstrap_toolchain": attr.bool(default = False),
    },
    impl = _prebuilt_jar_toolchain_rule_impl,
)
