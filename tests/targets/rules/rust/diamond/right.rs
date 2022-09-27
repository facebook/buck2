extern "C" {
    fn native();
}

pub fn right() {
    unsafe {
        native();
    }
}
