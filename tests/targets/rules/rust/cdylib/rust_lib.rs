// (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

extern "C" {
    fn from_lib();
}

#[no_mangle]
pub extern "C" fn via_lib() {
    unsafe {
        from_lib();
    }
}
