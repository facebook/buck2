load("@fbcode//buck2/platform:utils.bzl", "optional_binary_or_source_attr", "source_list_attr", "string_attr", "string_list_attr")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "AbiGenerationMode", "JUnitToolchainInfo", "JavaPlatformInfo", "JavaToolchainInfo", "JavacProtocol", "PrebuiltJarToolchainInfo")
load("@fbcode//buck2/prelude/java/utils:java_utils.bzl", "derive_javac")
load("@fbsource//tools/build_defs:buckconfig.bzl", "read_bool")

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
    kwargs["zip_scrubber"] = "buck//src/com/facebook/buck/util/zip:zip_scrubber_main_jar"

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
            zip_scrubber = RunInfo(cmd_args([ctx.attrs.java[RunInfo], "-jar", ctx.attrs.zip_scrubber])),
            is_bootstrap_toolchain = ctx.attrs.is_bootstrap_toolchain,
        ),
    ]

_config_backed_java_toolchain_rule = rule(
    attrs = {
        "abi_generation_mode": attrs.enum(["class", "source", "source_only"], default = "class"),
        "ast_dumper": attrs.option(attrs.dep(), default = None),
        "bootclasspath_7": attrs.list(attrs.source()),
        "bootclasspath_8": attrs.list(attrs.source()),
        "class_abi_generator": attrs.option(attrs.dep(providers = [RunInfo]), default = None),
        "compile_and_package": attrs.dep(),
        "fallback_javac": attrs.option(attrs.one_of(attrs.dep(), attrs.source(), attrs.string()), default = None),
        "fat_jar": attrs.dep(),
        "fat_jar_main_class_lib": attrs.option(attrs.source(), default = None),
        "is_bootstrap_toolchain": attrs.bool(default = False),
        "jar": attrs.dep(providers = [RunInfo]),
        "java": attrs.dep(),
        "java_for_tests": attrs.option(attrs.dep(providers = [RunInfo])),
        "javac": attrs.option(attrs.one_of(attrs.dep(), attrs.source(), attrs.string()), default = None),
        "javac_protocol": attrs.enum(JavacProtocol.values(), default = "classic"),
        "merge_to_jar": attrs.dep(),
        "source_level": attrs.string(),
        "src_dir_helper": attrs.dep(),
        "src_roots": attrs.list(attrs.string()),
        "target_level": attrs.string(),
        "zip_scrubber": attrs.source(),
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
    use_custom_class_loader_for_junit_test = read_bool("test", "use_custom_class_loader_for_junit_test", False)

    kwargs["java_custom_class_loader_class"] = "com.facebook.IndexClassLoader" if use_custom_class_loader_for_junit_test else None
    kwargs["java_custom_class_loader_library_jar"] = "fbsource//xplat/buck2/tools/android/index_classloader:index_classloader" if use_custom_class_loader_for_junit_test else None
    kwargs["java_custom_class_loader_vm_args"] = ["--add-exports", "java.base/jdk.internal.loader=ALL-UNNAMED"] if use_custom_class_loader_for_junit_test else None
    kwargs["junit_test_runner_library_jar"] = "buck//src/com/facebook/buck/testrunner:testrunner-bin-fixed"
    kwargs["junit_test_runner_main_class_args"] = ["com.facebook.buck.jvm.java.runner.FileClassPathRunner", "com.facebook.buck.testrunner.JUnitMain"]
    kwargs["list_class_names"] = "fbsource//xplat/buck2/tools/java:list_class_names"
    kwargs["use_java_custom_class_loader"] = use_custom_class_loader_for_junit_test

    _junit_toolchain_rule(name = name, **kwargs)

def _junit_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        JUnitToolchainInfo(
            java_custom_class_loader_class = ctx.attrs.java_custom_class_loader_class,
            java_custom_class_loader_library_jar = ctx.attrs.java_custom_class_loader_library_jar,
            java_custom_class_loader_vm_args = ctx.attrs.java_custom_class_loader_vm_args,
            junit_test_runner_library_jar = ctx.attrs.junit_test_runner_library_jar,
            junit_test_runner_main_class_args = ctx.attrs.junit_test_runner_main_class_args,
            list_class_names = ctx.attrs.list_class_names,
            use_java_custom_class_loader = ctx.attrs.use_java_custom_class_loader,
        ),
    ]

_junit_toolchain_rule = rule(
    attrs = {
        "java_custom_class_loader_class": attrs.option(attrs.string(), default = None),
        "java_custom_class_loader_library_jar": attrs.option(attrs.source(), default = None),
        "java_custom_class_loader_vm_args": attrs.option(attrs.list(attrs.string()), default = None),
        "junit_test_runner_library_jar": attrs.source(),
        "junit_test_runner_main_class_args": attrs.list(attrs.string()),
        "list_class_names": attrs.dep(providers = [RunInfo]),
        "use_java_custom_class_loader": attrs.bool(),
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
        "class_abi_generator": attrs.option(attrs.dep(providers = [RunInfo]), default = None),
        "is_bootstrap_toolchain": attrs.bool(default = False),
    },
    impl = _prebuilt_jar_toolchain_rule_impl,
)
