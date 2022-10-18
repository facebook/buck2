load("@fbcode//buck2:buck_rust_binary.bzl", "buck_rust_binary")
load("@fbcode_macros//build_defs:rust_library.bzl", "rust_library")

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

    build_env = build_env or {}
    build_env.update(
        {
            "OUT_DIR": "$OUT",
            "PROTOC": "$(exe buck//third-party/proto:protoc)",
            "PROTOC_INCLUDE": "$(location buck//third-party/proto:google_protobuf)",
        },
    )
    set_env = "\n".join(['export {}="{}"'.format(k, v) for k, v in build_env.items()])

    native.genrule(
        name = proto_name,
        srcs = [
            spec,
            "buck//third-party/proto:google_protobuf",
        ],
        cmd = "\n".join([
            set_env,
            "$(exe :" + build_name + ")",
        ]),
        out = ".",
    )

    rust_library(
        name = name,
        srcs = srcs,
        env = {
            # This is where tonic looks for generated .rs files
            "OUT_DIR": "$(location :{})".format(proto_name),
        },
        deps = [
            "fbsource//third-party/rust:prost",
            "fbsource//third-party/rust:prost-types",
            "fbsource//third-party/rust:tonic",
        ] + (deps or []),
    )
