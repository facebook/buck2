extern crate cc;

use std::env;

fn main() {
    let target = env::var("TARGET").unwrap();
    let target_os = target.split('-').nth(2).unwrap();

    let mut builder = cc::Build::new();
    match target_os {
        "linux" | "android" | "androideabi" => builder.file("c/linux.c"),
        "illumos" | "solaris" => {
            println!("cargo:rustc-link-lib=kstat");
            return;
        }
        "darwin" | "ios" => builder.file("c/macos.c"),
        "windows" => {
            println!("cargo:rustc-flags=-l psapi");
            println!("cargo:rustc-flags=-l powrprof");
            builder.file("c/windows.c")
        },
        "freebsd" => {
            println!("cargo:rustc-flags=-l pthread");
            builder.file("c/freebsd.c")
        },
        "openbsd" => {
            println!("cargo:rustc-flags=-l pthread");
            builder.file("c/openbsd.c")
        },
        _ => panic!("unsupported system: {}", target_os)
    };
    builder.compile("info");
}
