load("@prelude//rules.bzl", "python_binary")
load("//test_utils.bzl", "assert_output")

python_binary(
    name = "main",
    deps = ["//python/library:printlib"],
    main = "main/main.py"
)

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "hello world from python toolchain",
)
