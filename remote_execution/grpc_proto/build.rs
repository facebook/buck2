use std::io;

fn main() -> io::Result<()> {
    let proto_files = &[
        "proto/build/bazel/remote/execution/v2/remote_execution.proto",
        "proto/google/rpc/code.proto",
    ];

    buck2_protoc_dev::configure()
        .setup_protoc("../../../..")
        .compile(proto_files, &["./proto/"])?;

    Ok(())
}
