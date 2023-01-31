load("//test_utils.bzl", "assert_output")

rust_binary(
    name = "main",
    srcs =  ["src/main.rs"],
    crate_root = "src/main.rs",
    deps = [":print_hello"],
) if not host_info().os.is_windows else None

rust_library(
    name = "print_hello",
    srcs = ["src/print_hello.rs"],
    crate_root = "src/print_hello.rs",
)

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "hello world from rust toolchain",
) if not host_info().os.is_windows else None
