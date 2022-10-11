//! `jemalloc`'s build-time configuration.

option! {
    malloc_conf[ str: b"config.malloc_conf\0", str: 2 ] => &'static str |
    ops: r |
    docs:
    /// Default run-time options specified during `jemalloc`'s build configuration.
    ///
    /// The string will be empty unless `--with-malloc-conf` was specified
    /// during build configuration.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate jemallocator;
    /// # extern crate jemalloc_ctl;
    /// #
    /// # #[global_allocator]
    /// # static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;
    /// #
    /// # fn main() {
    /// use jemalloc_ctl::config;
    /// let malloc_conf = config::malloc_conf::mib().unwrap();
    /// println!("default malloc conf: {}", malloc_conf.read().unwrap());
    /// # }
    /// ```
    mib_docs: /// See [`malloc_conf`].
}
