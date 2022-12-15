load("//test_utils.bzl", "assert_output")

ocaml_binary(
    name = "main",
    srcs = ["main.ml"],
    deps = ["//ocaml/library:print"]
) if not host_info().os.is_windows else None

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "hello world from ocaml toolchain",
)if not host_info().os.is_windows else None
