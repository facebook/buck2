load("//test_utils.bzl", "assert_output")

cxx_binary(
    name = "main",
    srcs = ["main.cpp"],
    link_style = "static",
    deps = ["//cpp/library:print"],
) if not host_info().os.is_windows else None

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "hello world from cpp toolchain",
) if not host_info().os.is_windows else None
