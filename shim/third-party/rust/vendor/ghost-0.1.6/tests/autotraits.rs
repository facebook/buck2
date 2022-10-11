use ghost::phantom;

#[phantom]
pub struct MyPhantom<T>;

// Test that #[phantom] doesn't contain its own explicit autotrait impls, which
// would conflict with the following.
unsafe impl<T> Send for MyPhantom<T> {}
unsafe impl<T> Sync for MyPhantom<T> {}
