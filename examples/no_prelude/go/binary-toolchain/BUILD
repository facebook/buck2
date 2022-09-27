load("//:rules.bzl", "go_binary")
load("//:toolchain.bzl", "go_toolchain")

go_toolchain(
    name = "go_linux",
    platform = "linux-amd64",
    sha1 = "3511fcb34e0162abdcdeea0ab532f0264943e3d8",
    version = "1.18.3",
)

go_binary(
    name = "main",
    srcs = glob(["*.go"]),
    toolchain = ":go_linux",
    deps = [],
)
