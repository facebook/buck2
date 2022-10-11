use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicUsize, AtomicBool, Ordering};
use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use std::any::{Any, TypeId};

use init::Init;
use ident_hash::IdentHash;

#[cfg(feature = "tls")]
use tls::LocalValue;

/// A container for global type-based state.
///
/// A container stores at most _one_ global instance of given type as well as
/// _n_ thread-local instances of a given type.
///
/// ## Global State
///
/// Global state is set via the [set](#method.set) method and retrieved via the
/// [get](#method.get) method. The type of the value being set must be
/// thread-safe and transferable across thread boundaries. In other words, it
/// must satisfy `Sync + Send + 'static`.
///
/// ### Example
///
/// Set and later retrieve a value of type T:
///
/// ```rust
/// # struct T;
/// # impl T { fn new() -> T { T } }
/// static CONTAINER: state::Container = state::Container::new();
///
/// CONTAINER.set(T::new());
/// CONTAINER.get::<T>();
/// ```
///
/// ## Freezing
///
/// By default, all `get`, `set`, `get_local`, and `set_local` calls result in
/// synchronization overhead for safety. However, if calling `set` or
/// `set_local` is no longer required, the overhead can be eliminated by
/// _freezing_ the `Container`. A frozen container can only be read and never
/// written to. Attempts to write to a frozen container will fail.
///
/// To freeze a `Container`, call [`freeze()`](Container::freeze()). A frozen
/// container can never be thawed. To check if a container is frozen, call
/// [`is_frozen()`](Container::is_frozen()).
///
/// ## Thread-Local State
///
/// Thread-local state is set via the [set_local](#method.set_local) method and
/// retrieved via the [get_local](#method.get_local) method. The type of the
/// value being set must be transferable across thread boundaries but need not
/// be thread-safe. In other words, it must satisfy `Send + 'static` but not
/// necessarily `Sync`. Values retrieved from thread-local state are exactly
/// that: local to the current thread. As such, you cannot use thread-local
/// state to synchronize across multiple threads.
///
/// Thread-local state is initialized on an as-needed basis. The function used
/// to initialize the thread-local state is passed in as an argument to
/// `set_local`. When the state is retrieved from a given thread for the first
/// time, the function is executed to generate the initial value. The function
/// is executed at most once per thread. The same function is used for
/// initialization across all threads.
///
/// **Note:** Rust reuses thread IDs across multiple threads. This means that is
/// possible to set thread-local state in thread A, have that thread die, start
/// a new thread B, and access the state set in A in B.
///
/// ### Example
///
/// Set and later retrieve a value of type T:
///
/// ```rust
/// # struct T;
/// # impl T { fn new() -> T { T } }
/// # #[cfg(not(feature = "tls"))] fn test() { }
/// # #[cfg(feature = "tls")] fn test() {
/// static CONTAINER: state::Container = state::Container::new();
///
/// CONTAINER.set_local(|| T::new());
/// CONTAINER.get_local::<T>();
/// # }
/// # fn main() { test() }
/// ```
pub struct Container {
    init: Init,
    map: UnsafeCell<Option<TypeMap>>,
    mutex: AtomicUsize,
    frozen: AtomicBool
}

type TypeMap = HashMap<TypeId, AnyObject, BuildHasherDefault<IdentHash>>;

/// FIXME: This is a hack so we can create a *mut dyn Any with a `const`. It
/// depends on trait objects being represented in this way.
#[repr(C)]
struct AnyObject {
    data: *mut (),
    vtable: *mut (),
}

impl AnyObject {
    fn anonymize<T: 'static>(value: T) -> AnyObject {
        let any: Box<dyn Any> = Box::new(value) as Box<dyn Any>;
        let any: *mut dyn Any = Box::into_raw(any);
        unsafe { std::mem::transmute(any) }
    }

    fn deanonymize<T: 'static>(&self) -> &T {
        unsafe {
            let any: *const *const dyn Any = std::mem::transmute(self);
            &*(*any as *const dyn Any as *const T)
        }
    }
}

impl Drop for AnyObject {
    fn drop(&mut self) {
        unsafe {
            let any: *mut *mut dyn Any = std::mem::transmute(self);
            let any: *mut dyn Any = *any;
            let any: Box<dyn Any> = Box::from_raw(any);
            drop(any)
        }
    }
}

impl Container {
    /// Creates a new container with no stored values.
    ///
    /// ## Example
    ///
    /// Create a globally available state container:
    ///
    /// ```rust
    /// static CONTAINER: state::Container = state::Container::new();
    /// ```
    pub const fn new() -> Container {
        Container {
            init: Init::new(),
            map: UnsafeCell::new(None),
            mutex: AtomicUsize::new(0),
            frozen: AtomicBool::new(false),
        }
    }

    #[inline(always)]
    fn lock(&self) {
        while self.mutex.compare_and_swap(0, 1, Ordering::AcqRel) != 0 {}
    }

    #[inline(always)]
    fn unlock(&self) {
        assert!(self.mutex.compare_and_swap(1, 0, Ordering::AcqRel) == 1);
    }

    /// Freezes the container. A frozen container disallows writes allowing for
    /// synchronization-free reads.
    ///
    /// # Example
    ///
    /// ```rust
    /// use state::Container;
    ///
    /// // A new container starts unfrozen and can be written to.
    /// let mut container = Container::new();
    /// assert_eq!(container.set(1usize), true);
    ///
    /// // While unfrozen, `get`s require synchronization.
    /// assert_eq!(container.get::<usize>(), &1);
    ///
    /// // After freezing, calls to `set` or `set_local `will fail.
    /// container.freeze();
    /// assert_eq!(container.set(1u8), false);
    /// assert_eq!(container.set("hello"), false);
    ///
    /// // Calls to `get` or `get_local` are synchronization-free when frozen.
    /// assert_eq!(container.try_get::<u8>(), None);
    /// assert_eq!(container.get::<usize>(), &1);
    /// ```
    #[inline(always)]
    pub fn freeze(&mut self) {
        self.frozen.store(true, Ordering::SeqCst);
    }

    /// Returns `true` if the container is frozen and `false` otherwise.
    ///
    /// # Example
    ///
    /// ```rust
    /// use state::Container;
    ///
    /// // A new container starts unfrozen and is frozen using `freeze`.
    /// let mut container = Container::new();
    /// assert_eq!(container.is_frozen(), false);
    ///
    /// container.freeze();
    /// assert_eq!(container.is_frozen(), true);
    /// ```
    #[inline(always)]
    pub fn is_frozen(&self) -> bool {
        self.frozen.load(Ordering::Relaxed)
    }

    // Initializes the `map` if needed.
    unsafe fn init_map_if_needed(&self) {
        if self.init.needed() {
            *self.map.get() = Some(HashMap::<_, _, _>::default());
            self.init.mark_complete();
        }
    }

    // Initializes the `map` if needed and returns it.
    #[inline(always)]
    unsafe fn map_mut(&self) -> &mut TypeMap {
        self.init_map_if_needed();
        (*self.map.get()).as_mut().unwrap()
    }

    // Initializes the `map` if needed and returns it.
    #[inline(always)]
    unsafe fn map_ref(&self) -> &TypeMap {
        self.init_map_if_needed();
        (*self.map.get()).as_ref().unwrap()
    }

    /// Sets the global state for type `T` if it has not been set before and
    /// `self` is not frozen.
    ///
    /// If the state for `T` has previously been set or `self` is frozen, the
    /// state is unchanged and `false` is returned. Otherwise `true` is
    /// returned.
    ///
    /// # Example
    ///
    /// Set the state for `AtomicUsize`. The first `set` is succesful while the
    /// second fails.
    ///
    /// ```rust
    /// # use std::sync::atomic::AtomicUsize;
    /// static CONTAINER: state::Container = state::Container::new();
    ///
    /// assert_eq!(CONTAINER.set(AtomicUsize::new(0)), true);
    /// assert_eq!(CONTAINER.set(AtomicUsize::new(1)), false);
    /// ```
    #[inline]
    pub fn set<T: Send + Sync + 'static>(&self, state: T) -> bool {
        if self.is_frozen() {
            return false;
        }

        unsafe {
            self.lock();
            let map = self.map_mut();
            let type_id = TypeId::of::<T>();
            let already_set = map.contains_key(&type_id);
            if !already_set {
                map.insert(type_id, AnyObject::anonymize(state));
            }

            self.unlock();
            !already_set
        }
    }

    /// Attempts to retrieve the global state for type `T`.
    ///
    /// Returns `Some` if the state has previously been [set](#method.set).
    /// Otherwise returns `None`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::sync::atomic::{AtomicUsize, Ordering};
    /// struct MyState(AtomicUsize);
    ///
    /// static CONTAINER: state::Container = state::Container::new();
    ///
    /// // State for `T` is initially unset.
    /// assert!(CONTAINER.try_get::<MyState>().is_none());
    ///
    /// CONTAINER.set(MyState(AtomicUsize::new(0)));
    ///
    /// let my_state = CONTAINER.try_get::<MyState>().expect("MyState");
    /// assert_eq!(my_state.0.load(Ordering::Relaxed), 0);
    /// ```
    #[inline]
    pub fn try_get<T: Send + Sync + 'static>(&self) -> Option<&T> {
        unsafe {
            // If we're frozen, there can't be any concurrent writers, so we're
            // free to read this safely without taking a lock.
            let map = self.map_ref();
            let type_id = TypeId::of::<T>();
            let item = if self.is_frozen() {
                map.get(&type_id)
            } else {
                self.lock();
                let item = map.get(&type_id);
                self.unlock();
                item
            };

            item.map(|ptr| ptr.deanonymize())
        }
    }

    /// Retrieves the global state for type `T`.
    ///
    /// # Panics
    ///
    /// Panics if the state for type `T` has not previously been
    /// [set](#method.set). Use [try_get](#method.try_get) for a non-panicking
    /// version.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::sync::atomic::{AtomicUsize, Ordering};
    /// struct MyState(AtomicUsize);
    ///
    /// static CONTAINER: state::Container = state::Container::new();
    ///
    /// CONTAINER.set(MyState(AtomicUsize::new(0)));
    ///
    /// let my_state = CONTAINER.get::<MyState>();
    /// assert_eq!(my_state.0.load(Ordering::Relaxed), 0);
    /// ```
    #[inline]
    pub fn get<T: Send + Sync + 'static>(&self) -> &T {
        self.try_get()
            .expect("container::get(): get() called before set() for given type")
    }

    /// Sets the thread-local state for type `T` if it has not been set before.
    ///
    /// The state for type `T` will be initialized via the `state_init` function as
    /// needed. If the state for `T` has previously been set, the state is unchanged
    /// and `false` is returned. Returns `true` if the thread-local state is
    /// successfully set to be initialized with `state_init`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::cell::Cell;
    /// struct MyState(Cell<usize>);
    ///
    /// static CONTAINER: state::Container = state::Container::new();
    ///
    /// assert_eq!(CONTAINER.set_local(|| MyState(Cell::new(1))), true);
    /// assert_eq!(CONTAINER.set_local(|| MyState(Cell::new(2))), false);
    /// ```
    #[cfg(feature = "tls")]
    #[inline]
    pub fn set_local<T, F>(&self, state_init: F) -> bool
        where T: Send + 'static, F: Fn() -> T + 'static
    {
        self.set::<LocalValue<T>>(LocalValue::new(state_init))
    }

    /// Attempts to retrieve the thread-local state for type `T`.
    ///
    /// Returns `Some` if the state has previously been set via
    /// [set_local](#method.set_local). Otherwise returns `None`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::cell::Cell;
    /// struct MyState(Cell<usize>);
    ///
    /// static CONTAINER: state::Container = state::Container::new();
    ///
    /// CONTAINER.set_local(|| MyState(Cell::new(10)));
    ///
    /// let my_state = CONTAINER.try_get_local::<MyState>().expect("MyState");
    /// assert_eq!(my_state.0.get(), 10);
    /// ```
    #[cfg(feature = "tls")]
    #[inline]
    pub fn try_get_local<T: Send + 'static>(&self) -> Option<&T> {
        // TODO: This will take a lock on the HashMap unnecessarily. Ideally
        // we'd have a `HashMap` per thread mapping from TypeId to (T, F).
        self.try_get::<LocalValue<T>>().map(|value| value.get())
    }

    /// Retrieves the thread-local state for type `T`.
    ///
    /// # Panics
    ///
    /// Panics if the thread-local state for type `T` has not previously been set
    /// via [set_local](#method.set_local). Use
    /// [try_get_local](#method.try_get_local) for a non-panicking version.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::cell::Cell;
    /// struct MyState(Cell<usize>);
    ///
    /// static CONTAINER: state::Container = state::Container::new();
    ///
    /// CONTAINER.set_local(|| MyState(Cell::new(10)));
    ///
    /// let my_state = CONTAINER.get_local::<MyState>();
    /// assert_eq!(my_state.0.get(), 10);
    /// ```
    #[cfg(feature = "tls")]
    #[inline]
    pub fn get_local<T: Send + 'static>(&self) -> &T {
        self.try_get_local::<T>()
            .expect("container::get_local(): get_local() called before set_local()")
    }
}

unsafe impl Sync for Container {  }
unsafe impl Send for Container {  }
