load("@prelude//rules.bzl", "cxx_library")

cxx_library(
    name = "print",
    srcs = glob(["**/*.cpp"]),
    exported_headers = glob(["**/*.hpp"]),
    visibility = ["PUBLIC"],
) if not host_info().os.is_windows else None
