use std::io;

fn main() -> io::Result<()> {
    let proto_files = &["forkserver.proto"];

    // Tonic build uses PROTOC to determine the protoc path.
    println!("cargo:rerun-if-env-changed=PROTOC");
    tonic_build::configure().compile(proto_files, &["."])?;

    // Tell Cargo that if the given file changes, to rerun this build script.
    for proto_file in proto_files {
        println!("cargo:rerun-if-changed={}", proto_file);
    }

    Ok(())
}
