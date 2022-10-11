use std::fmt;

use thread_local::ThreadLocal;
use storage::Storage;

pub struct LocalValue<T> {
    tls: ThreadLocal<T>,
    init_fn: Box<dyn Fn() -> T>,
}

impl<T: Send + 'static> LocalValue<T> {
    pub fn new<F: Fn() -> T + 'static>(init_fn: F) -> LocalValue<T> {
        LocalValue {
            tls: ThreadLocal::new(),
            init_fn: Box::new(init_fn),
        }
    }

    pub fn get(&self) -> &T {
        self.tls.get_or(|| (self.init_fn)())
    }
}

unsafe impl<T: Send + 'static> Sync for LocalValue<T> {}

unsafe impl<T: Send + 'static> Send for LocalValue<T> {}

/// A single storage location for global access to thread-local values.
///
/// A `LocalStorage` instance allows global access to a `n` per-thread values,
/// all of which are initialized in the same manner when the value is first
/// retrieved from a thread.
//
/// The initialization function for values in `LocalStorage` is specified via
/// the [set](#method.set) method. The initialization function must be set
/// before a value is attempted to be retrieved via the [get](#method.get)
/// method. The [try_get](#method.try_get) can be used to determine whether the
/// `LocalStorage` has been initialized before attempting to retrieve a value.
///
/// For safety reasons, values stored in `Storage` must be `Send + 'static`.
///
/// # Comparison with `Storage`
///
/// When the use-case allows, there are two primary advantages to using a
/// `LocalStorage` instance over a `Storage` instance:
///
///   1. Values stored in `LocalStorage` do not need to be thread-safe. In
///      other words, their type does not need to implement `Sync`.
///   2. There is no synchronization overhead when modifying values.
///
/// Keep in mind that values stored in `LocalStorage` are _not_ the same across
/// different threads. Modifications made to the stored value in one thread are
/// _not_ visible in another. Furthermore, because Rust reuses thread IDs, a new
/// thread is _not_ guaranteed to receive a newly initialized value on its first
/// call to `get`.
///
/// # Usage
///
/// **This type is only available when the `"tls"` feature is enabled.** To
/// enable the feature, include the `state` dependency in your `Cargo.toml` as
/// follows:
///
/// ```toml
/// [dependencies]
/// state = { version = "0.4", features = ["tls"] }
/// ```
///
/// # Example
///
/// The following example uses `LocalStorage` to store a per-thread count:
///
/// ```rust
/// # extern crate state;
/// # use std::cell::Cell;
/// # use std::thread;
/// # use state::LocalStorage;
/// static COUNT: LocalStorage<Cell<usize>> = LocalStorage::new();
///
/// fn check_count() {
///     let count = COUNT.get();
///
///     // initialize the state, in case we reuse thread IDs
///     count.set(0);
///
///     // increment it, non-atomically
///     count.set(count.get() + 1);
///
///     // The count should always be 1 since the state is thread-local.
///     assert_eq!(count.get(), 1);
/// }
///
/// fn main() {
///     // setup the initializer for thread-local state
///     COUNT.set(|| Cell::new(0));
///
///     // spin up many threads that call `check_count`.
///     let mut threads = vec![];
///     for i in 0..10 {
///         threads.push(thread::spawn(|| check_count()));
///     }
///
///     // Wait for all of the thread to finish.
///     for thread in threads {
///         thread.join().expect("correct count");
///     }
/// }
/// ```
pub struct LocalStorage<T> {
    storage: Storage<LocalValue<T>>
}

impl<T> LocalStorage<T> {
    /// Create a new, uninitialized storage location.
    ///
    /// # Example
    ///
    /// ```rust
    /// use state::LocalStorage;
    ///
    /// static MY_GLOBAL: LocalStorage<String> = LocalStorage::new();
    /// ```
    pub const fn new() -> LocalStorage<T> {
        LocalStorage { storage: Storage::new() }
    }
}

impl<T: Send + 'static> LocalStorage<T> {
    /// Sets the initialization function for this local storage unit to
    /// `state_init` if it has not already been set before. The function will be
    /// used to initialize values on the first access from a thread with a new
    /// thread ID.
    ///
    /// If a value has previously been set, `self` is unchanged and `false` is
    /// returned. Otherwise `true` is returned.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use state::LocalStorage;
    /// static MY_GLOBAL: LocalStorage<&'static str> = LocalStorage::new();
    ///
    /// assert_eq!(MY_GLOBAL.set(|| "Hello, world!"), true);
    /// assert_eq!(MY_GLOBAL.set(|| "Goodbye, world!"), false);
    /// ```
    #[inline]
    pub fn set<F: Fn() -> T + 'static>(&self, state_init: F) -> bool {
        self.storage.set(LocalValue::new(state_init))
    }

    /// Attempts to borrow the value in this storage location. If this is the
    /// first time a thread with the current thread ID has called `get` or
    /// `try_get` for `self`, the value will be initialized using the
    /// initialization function.
    ///
    /// Returns `Some` if the state has previously been [set](#method.set).
    /// Otherwise returns `None`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use state::LocalStorage;
    /// static MY_GLOBAL: LocalStorage<&'static str> = LocalStorage::new();
    ///
    /// assert_eq!(MY_GLOBAL.try_get(), None);
    ///
    /// MY_GLOBAL.set(|| "Hello, world!");
    ///
    /// assert_eq!(MY_GLOBAL.try_get(), Some(&"Hello, world!"));
    /// ```
    #[inline]
    pub fn try_get(&self) -> Option<&T> {
        self.storage.try_get().map(|v| v.get())
    }

    /// If this is the first time a thread with the current thread ID has called
    /// `get` or `try_get` for `self`, the value will be initialized using the
    /// initialization function.
    ///
    /// # Panics
    ///
    /// Panics if an initialization function has not previously been
    /// [set](#method.set). Use [try_get](#method.try_get) for a non-panicking
    /// version.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use state::LocalStorage;
    /// static MY_GLOBAL: LocalStorage<&'static str> = LocalStorage::new();
    ///
    /// MY_GLOBAL.set(|| "Hello, world!");
    /// assert_eq!(*MY_GLOBAL.get(), "Hello, world!");
    /// ```
    #[inline]
    pub fn get(&self) -> &T {
        self.try_get().expect("localstorage::get(): called get() before set()")
    }
}

unsafe impl<T: Send + 'static> Send for LocalStorage<T> {  }

unsafe impl<T: Send + 'static> Sync for LocalStorage<T> {  }

impl<T: fmt::Debug + Send + 'static> fmt::Debug for LocalStorage<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.try_get() {
            Some(object) => object.fmt(f),
            None => write!(f, "[uninitialized local storage]")
        }
    }
}
