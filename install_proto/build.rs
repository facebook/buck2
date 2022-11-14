use std::io;

fn main() -> io::Result<()> {
    let proto_files = &["install.proto"];

    buck2_protoc_dev::configure()
        .setup_protoc("../../..")
        .compile(proto_files, &["."])
}
