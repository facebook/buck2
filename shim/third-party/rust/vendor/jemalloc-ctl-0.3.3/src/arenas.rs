//! Arena operations.

option! {
    narenas[ str: b"arenas.narenas\0", non_str: 2 ] => libc::c_uint |
    ops: r |
    docs:
    /// Current limit on the number of arenas.
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
    /// use jemalloc_ctl::arenas;
    /// println!("number of arenas: {}", arenas::narenas::read().unwrap());
    ///
    /// let arenas_mib = arenas::narenas::mib().unwrap();
    /// println!("number of arenas: {}", arenas_mib.read().unwrap());
    /// # }
    /// ```
    mib_docs: /// See [`narenas`].
}
