#[no_mangle]
pub extern "C" fn call_back() {
    // No-op
}

fn main() {
    let mut args = std::env::args();
    let _ = args.next();
    let lib = args.next().unwrap();

    unsafe {
        let lib = libloading::Library::new(lib).unwrap();
        let func: libloading::Symbol<unsafe extern "C" fn()> = lib.get(b"call_lib").unwrap();
        func();
    }
}
