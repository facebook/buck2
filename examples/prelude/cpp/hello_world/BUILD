load("@prelude//rules.bzl", "cxx_binary")
load("//test_utils.bzl", "assert_output")

cxx_binary(
    name = "main",
    link_style = "static",
    srcs = ["main.cpp"],
    deps = ["//cpp/library:print"]
)

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "hello world from cpp toolchain",
)
