load(":toolchain.bzl", "cxx_toolchain", "ocaml_toolchain", "python_toolchain")
load("@prelude//toolchains:python.bzl", "system_python_bootstrap_toolchain")

cxx_toolchain(
    name="cxx",
    visibility=["PUBLIC"],
)

python_toolchain(
    name="python",
    visibility=["PUBLIC"],
)

ocaml_toolchain(
    name="ocaml",
    visibility=["PUBLIC"],
)

system_python_bootstrap_toolchain(
    name = "python_bootstrap",
    visibility = ["PUBLIC"],
)
