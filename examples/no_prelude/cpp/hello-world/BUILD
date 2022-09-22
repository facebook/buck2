load("//:rules.bzl", "cpp_binary")

cpp_binary(
    name = "main",
    srcs = glob(["src/**/*.cpp"]),
    headers = glob(["src/**/*.hpp"]),
    toolchain = "//:clang",
    deps = [],
)
