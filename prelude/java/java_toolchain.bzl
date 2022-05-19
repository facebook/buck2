JavaPlatformInfo = provider(
    "Java platform info",
    fields = [
        "name",
    ],
)

JavaToolchainInfo = provider(
    "Java toolchain info",
    fields = [
        "bootclasspath_7",
        "bootclasspath_8",
        "class_abi_generator",
        "compile_and_package",
        "fat_jar",
        "fat_jar_main_class_lib",
        "jar",
        "java",
        "java_for_tests",
        "javac",
        "src_dir_helper",
        "source_level",
        "src_root_elements",
        "src_root_prefixes",
        "target_level",
    ],
)

JUnitToolchainInfo = provider(
    "Java test toolchain info",
    fields = [
        "junit_test_runner_library_jar",
        "junit_test_runner_main_class",
        "list_class_names",
    ],
)
