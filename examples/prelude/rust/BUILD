load("//test_utils.bzl", "assert_output")

rust_binary(
    name = "main",
    srcs = glob(
        ["src/**/*.rs"],
    ),
    crate_root = "src/main.rs",
) if not host_info().os.is_windows else None

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "hello world from rust toolchain",
) if not host_info().os.is_windows else None
