load(":toolchain.bzl", "cxx_toolchain", "ocaml_toolchain", "python_toolchain")
load("@prelude//toolchains/python/bootstrap:defs.bzl", "python_bootstrap_toolchain")

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

# the bootstrap toolchain is used
# to poentially initialize other
# toolchains
python_bootstrap_toolchain(
    name="python_bootstrap",
    visibility=["PUBLIC"],
)
