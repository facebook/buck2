load("@prelude//rules.bzl", "ocaml_library")

ocaml_library(
    name = "print",
    srcs = ["library.ml"],
    visibility = ["PUBLIC"],
) if not host_info().os.is_windows else None
