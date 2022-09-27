extern "C" {
    fn native();
}

pub fn left() {
    unsafe {
        native();
    }
}
