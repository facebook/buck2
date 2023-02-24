load("//test_utils.bzl", "assert_output")

python_binary(
    name = "main",
    main = "main/main.py",
    deps = ["//python/library:printlib"],
)

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "hello world from python toolchain",
)
