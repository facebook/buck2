extern "C" {
    fn hello(x: i32);
}

fn main() {
    unsafe {
        hello(42);
    }
}
