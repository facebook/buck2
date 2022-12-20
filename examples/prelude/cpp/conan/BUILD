load("//test_utils.bzl", "assert_output")

cxx_binary(
    name = "main",
    link_style = "static",
    srcs = ["main.cpp"],
) if not host_info().os.is_windows else None

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "Hello Conan",
) if not host_info().os.is_windows else None

