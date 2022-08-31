#[no_mangle]
pub extern "C" fn magic(x: usize) -> usize {
    x + x
}
