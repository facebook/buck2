use std::io;

fn main() -> io::Result<()> {
    let proto_files = &["forkserver.proto"];

    // Tonic build uses PROTOC to determine the protoc path.
    println!("cargo:rerun-if-env-changed=PROTOC");
    buck2_protoc_dev::maybe_setup_protoc("../../../..");
    tonic_build::configure()
        .protoc_arg("--experimental_allow_proto3_optional")
        .type_attribute(
            "buck.forkserver.RequestEvent.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName, ::gazebo::variants::UnpackVariants)]",
        )
        .type_attribute(
            "buck.forkserver.EnvDirective.data",
            "#[derive(::derive_more::From, ::gazebo::variants::VariantName, ::gazebo::variants::UnpackVariants)]",
        )
        .compile(proto_files, &["."])?;

    // Tell Cargo that if the given file changes, to rerun this build script.
    for proto_file in proto_files {
        println!("cargo:rerun-if-changed={}", proto_file);
    }

    Ok(())
}
