use std::env;
use std::process::Command;
use std::str;

fn main() {
    let compiler = match rustc_minor_version() {
        Some(compiler) => compiler,
        None => return,
    };

    if compiler < 36 {
        // https://doc.rust-lang.org/std/mem/union.MaybeUninit.html
        println!("cargo:rustc-cfg=no_maybe_uninit");
    }

    if compiler < 52 {
        // #![deny(unsafe_op_in_unsafe_fn)].
        // https://github.com/rust-lang/rust/issues/71668
        println!("cargo:rustc-cfg=no_unsafe_op_in_unsafe_fn_lint");
    }
}

fn rustc_minor_version() -> Option<u32> {
    let rustc = env::var_os("RUSTC")?;
    let output = Command::new(rustc).arg("--version").output().ok()?;
    let version = str::from_utf8(&output.stdout).ok()?;
    let mut pieces = version.split('.');
    if pieces.next() != Some("rustc 1") {
        return None;
    }
    pieces.next()?.parse().ok()
}
