use std::process::exit;

const MIN_SUPPORTED_VERSION: &str = "1.34.0";

fn main() {
    match version_check::is_min_version(MIN_SUPPORTED_VERSION) {
        Some(true) => {}
        _ => {
            // rustc version too small or can't figure it out
            eprintln!(
                "rustc >= {} is required for test-case to compile",
                MIN_SUPPORTED_VERSION
            );
            exit(1);
        }
    }
}
