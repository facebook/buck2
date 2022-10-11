use ghost::phantom;

#[phantom]
struct MyPhantom<T: ?Sized>;

fn main() {
    let phantom = MyPhantom::<u8>;

    match phantom {
        MyPhantom => {}
    }

    match phantom {
        MyPhantom::<u8> => {}
    }
}
