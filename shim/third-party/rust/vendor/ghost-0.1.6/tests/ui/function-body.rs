use ghost::phantom;

fn main() {
    // Not supported. https://github.com/dtolnay/ghost/issues/1

    #[phantom]
    struct MyPhantom<T: ?Sized>;
}
