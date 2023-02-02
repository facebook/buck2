load("//test_utils.bzl", "assert_output")

ocaml_object(
    name = "fib-ml",
    srcs = ["fib.ml"],
) if host_info().os.is_macos else None

cxx_binary(
    name = "fib-cpp",
    srcs = ["fib.cpp"],
    deps = [
        ":fib-ml",
        "//third-party/ocaml:ocaml-dev"
    ],
) if host_info().os.is_macos else None

rust_binary(
    name = "fib-rs",
    srcs = ["fib.rs"],
    deps = [":fib-ml"],
    crate_root = "fib.rs",
) if host_info().os.is_macos else None

assert_output(
    name = "check-fib-cpp",
    command = "$(exe_target :fib-cpp)",
    output = "fib(10) = Result is: 89",
) if host_info().os.is_macos else None

assert_output(
    name = "check-fib-rs",
    command = "$(exe_target :fib-rs)",
    output = "fib(10) = Result is: 89",
) if host_info().os.is_macos else None
