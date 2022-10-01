#[no_mangle]
extern "C" fn get_base_value(_unit: usize) -> usize {
    (1usize << 1) | 1
}
