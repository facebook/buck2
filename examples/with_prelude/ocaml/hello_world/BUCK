load("//test_utils.bzl", "assert_output")

# buildifier: disable=no-effect
ocaml_binary(
    name = "hello-world",
    srcs = ["hello_world.ml"],
    deps = [":hello-world-lib"],
) if not host_info().os.is_windows else None

# buildifier: disable=no-effect
ocaml_library(
    name = "hello-world-lib",
    srcs = ["hello_world_lib.ml"],
    visibility = ["PUBLIC"],
) if not host_info().os.is_windows else None

# buildifier: disable=no-effect
assert_output(
    name = "hello-world-check",
    command = "$(exe_target :hello-world)",
    output = "Hello world!",
) if not host_info().os.is_windows else None
