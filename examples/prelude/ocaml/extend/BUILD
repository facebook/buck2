load("//test_utils.bzl", "assert_output")

ocaml_binary(
    name = "hello",
    srcs = [
        "hello.ml",
    ],
    deps = [":hello-stubs"],
) if not host_info().os.is_windows else None

cxx_library(
    name = "hello-stubs",
    srcs = [
        "hello_stubs.c",
    ],
    deps = ["//third-party/ocaml:ocaml-dev"],
) if not host_info().os.is_windows else None

assert_output(
    name = "check-hello",
    command = "$(exe_target :hello)",
    output = "Hello",
) if not host_info().os.is_windows else None
