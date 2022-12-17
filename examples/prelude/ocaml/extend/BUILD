load("//test_utils.bzl", "assert_output")

ocaml_binary(
    name = "hello",
    srcs = [
        "hello.ml",
    ],
    deps = [":hello-stubs"],
) if host_info().os.is_linux else None

cxx_library(
    name = "hello-stubs",
    srcs = [
        "hello_stubs.c",
    ],
    deps = ["root//ocaml/opam:ocaml-dev"],
) if host_info().os.is_linux else None

assert_output(
    name = "check-hello",
    command = "$(exe_target :hello)",
    output = "Hello",
) if host_info().os.is_linux else None
