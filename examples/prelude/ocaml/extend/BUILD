load("//test_utils.bzl", "assert_output")

ocaml_binary(
    name = "hello-c",
    srcs = [
        "hello.ml",
    ],
    deps = [":hello-stubs-c"],
) if not host_info().os.is_windows else None

cxx_library(
    name = "hello-stubs-c",
    srcs = [
        "hello_stubs.c",
    ],
    deps = ["//third-party/ocaml:ocaml-dev"],
) if not host_info().os.is_windows else None

ocaml_binary(
    name = "hello-rs",
    srcs = [
        "hello.ml",
    ],
    deps = [":hello-stubs-rs"],
) if not host_info().os.is_windows else None

rust_library(
    name = "hello-stubs-rs",
    srcs = [
        "hello_stubs.rs",
    ],
    crate_root = "hello_stubs.rs",
) if not host_info().os.is_windows else None

assert_output(
    name = "check-hello-c",
    command = "$(exe_target :hello-c)",
    output = "Hello C",
) if not host_info().os.is_windows else None

assert_output(
    name = "check-hello-rs",
    command = "$(exe_target :hello-rs)",
    output = "Hello Rust",
) if not host_info().os.is_windows else None
