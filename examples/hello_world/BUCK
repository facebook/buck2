cxx_binary(
    name = "main",
    srcs = ["main.cpp"],
    link_style = "static",
    deps = [":print"],
)

cxx_library(
    name = "print",
    srcs = glob(["**/*.cpp"]),
    exported_headers = glob(["**/*.hpp"]),
    visibility = ["PUBLIC"],
)
