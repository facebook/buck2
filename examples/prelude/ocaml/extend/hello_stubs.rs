#[no_mangle]
pub unsafe extern "C" fn caml_print_hello(_: usize) -> usize {
    println!("Hello Rust!\n");
    return 0;
}
