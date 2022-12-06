load("@prelude//rules.bzl", "ocaml_binary")
load("//test_utils.bzl", "assert_output")

ocaml_binary(
    name = "main",
    srcs = ["main.ml"],
    deps = ["//ocaml/library:print"]
)

assert_output(
    name = "check_main",
    command = "$(exe_target :main)",
    output = "hello world from ocaml toolchain",
)
