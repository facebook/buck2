load("@prelude//toolchains/cxx/zig:defs.bzl", "download_zig_distribution", "cxx_zig_toolchain")
# TODO Replace by prelude toolchain once available
load("@toolchains//python:defs.bzl", "system_python_bootstrap_toolchain")

download_zig_distribution(
    name = "zig",
    version = "0.9.1",
)

cxx_zig_toolchain(
    name = "cxx",
    distribution = ":zig",
    visibility = ["PUBLIC"],
)

system_python_bootstrap_toolchain(
    name = "python_bootstrap",
    visibility = ["PUBLIC"],
)
