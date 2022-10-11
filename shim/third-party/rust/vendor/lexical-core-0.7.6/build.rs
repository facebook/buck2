fn main() {
    // TARGET
    // ------

    // We need to optimize limb size for performance.
    // Only have optimized 64-bit instructions on certain architectures.
    // See `lexical-core/src/atof/algorithm/math.rs` for detailed
    // instructions of architecture instruction support for 64-bit
    // mathematical operations.

    // https://github.com/rust-lang/cargo/issues/4302#issuecomment-316482399
    let limb_64_archs = ["aarch64", "mips64", "powerpc64", "x86_64"];
    let limb_width_64 = match std::env::var("CARGO_CFG_TARGET_ARCH") {
        Ok(arch) => limb_64_archs.contains(&&*arch),
        _ => false,
    };
    if limb_width_64 {
        println!("cargo:rustc-cfg=limb_width_64");
    } else {
        println!("cargo:rustc-cfg=limb_width_32");
    }
}
