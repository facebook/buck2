load("//test_utils.bzl", "assert_output")

ocaml_binary(
    name = "hello",
    srcs = [
        "hello.ml",
        "hello_stubs.c",
    ],
) if not host_info().os.is_windows else None

assert_output(
    name = "hello-check",
    command = "$(exe_target :hello)",
    output = "Hello",
) if not host_info().os.is_windows else None
