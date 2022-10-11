use ghost::phantom;

#[phantom]
struct MyPhantom<T: ?Sized>;

fn require_send<T: Send>() {}
fn require_sync<T: Sync>() {}

fn main() {
    // ok
    require_send::<MyPhantom<u8>>();
    require_sync::<MyPhantom<u8>>();

    // not ok
    require_send::<MyPhantom<*const u8>>();
    require_sync::<MyPhantom<*const u8>>();
}
