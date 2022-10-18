load("@fbcode//buck2:buck_rust_binary.bzl", "buck_rust_binary")

def rust_protobuf_library(name, srcs, build_script, spec, build_env = None, deps = None):
    build_name = name + "-build"
    proto_name = name + "-proto"

    buck_rust_binary(
        name = build_name,
        srcs = [build_script],
        crate_root = build_script,
        deps = [
            "fbsource//third-party/rust:tonic-build",
            "//buck2/app/buck2_protoc_dev:buck2_protoc_dev",
        ],
    )
