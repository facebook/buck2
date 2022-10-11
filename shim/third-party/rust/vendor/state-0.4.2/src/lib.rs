#![doc(html_root_url = "https://docs.rs/state/0.4.2")]

//! # state - safe and effortless state management
//!
//! This crate allows you to safely and effortlessly manage global and/or
//! thread-local state. Three primitives are provided for state management:
//!
//!  * **[Container](struct.Container.html):** Type-based global and
//!  thread-local storage for many values.
//!  * **[Storage](struct.Storage.html):** Global storage for a single instance.
//!  * **[LocalStorage](struct.LocalStorage.html):** Thread-local storage for a
//!  single instance.
//!
//! ## Usage
//!
//! Include `state` in your `Cargo.toml` `[dependencies]`:
//!
//! ```toml
//! [dependencies]
//! state = "0.4"
//! ```
//!
//! Thread-local state management is not enabled by default. You can enable it
//! via the `tls` feature:
//!
//! ```toml
//! [dependencies]
//! state = { version = "0.4", features = ["tls"] }
//! ```
//!
//! ## Use Cases
//!
//! ### Read-Only Singleton
//!
//! Suppose you have the following structure which is initialized in `main`
//! after receiving input from the user:
//!
//! ```rust
//! struct Configuration {
//!     name: String,
//!     number: isize,
//!     verbose: bool
//! }
//!
//! fn main() {
//!     let config = Configuration {
//!         /* fill in structure at run-time from user input */
//! #        name: "Sergio".to_string(),
//! #        number: 1,
//! #        verbose: true
//!     };
//! }
//! ```
//!
//! You'd like to access this structure later, at any point in the program,
//! without any synchronization overhead. Prior to `state`, assuming you needed
//! to setup the structure after program start, your options were:
//!
//!   1. Use `static mut` and `unsafe` to set an `Option<Configuration>` to
//!      `Some`. Retrieve by checking for `Some`.
//!   2. Use `lazy_static` with a `RwLock` to set an
//!      `RwLock<Option<Configuration>>` to `Some`. Retrieve by `lock`ing and
//!      checking for `Some`, paying the cost of synchronization.
//!
//! With `state`, you can use [LocalStorage](struct.LocalStorage.html) and call
//! `set` and `get`, as follows:
//!
//! ```rust
//! # extern crate state;
//! # #[cfg(feature = "tls")]
//! # fn main() {
//! # use state::LocalStorage;
//! # struct Configuration { name: String, number: isize, verbose: bool }
//! static CONFIG: LocalStorage<Configuration> = LocalStorage::new();
//!
//! fn main() {
//!     CONFIG.set(|| Configuration {
//!         /* fill in structure at run-time from user input */
//! #        name: "Sergio".to_string(),
//! #        number: 1,
//! #        verbose: true
//!     });
//!
//!     /* at any point later in the program, in any thread */
//!     let config = CONFIG.get();
//! }
//! # }
//! # #[cfg(not(feature = "tls"))]
//! # fn main() {  }
//! ```
//!
//! ### Read/Write Singleton
//!
//! Following from the previous example, let's now say that we want to be able
//! to modify our singleton `Configuration` structure as the program evolves. We
//! have two options:
//!
//!   1. If we want to maintain the _same_ state in any thread, we can use a
//!      `Storage` structure and wrap our `Configuration` structure in a
//!      synchronization primitive.
//!   2. If we want to maintain _different_ state in any thread, we can continue
//!      to use a `LocalStorage` structure and wrap our `LocalStorage` type in a
//!      `Cell` structure for internal mutability.
//!
//! In this example, we'll choose **1**. The next example illustrates an
//! instance of **2**.
//!
//! The following implements **1** by using a `Storage` structure and wrapping
//! the `Configuration` type with a `RwLock`:
//!
//! ```rust
//! # struct Configuration { name: String, number: isize, verbose: bool }
//! # use state::Storage;
//! # use std::sync::RwLock;
//! static CONFIG: Storage<RwLock<Configuration>> = Storage::new();
//!
//! fn main() {
//!     let config = Configuration {
//!         /* fill in structure at run-time from user input */
//! #        name: "Sergio".to_string(),
//! #        number: 1,
//! #        verbose: true
//!     };
//!
//!     // Make the config avaiable globally.
//!     CONFIG.set(RwLock::new(config));
//!
//!     /* at any point later in the program, in any thread */
//!     let mut_config = CONFIG.get().write();
//! }
//! ```
//!
//! ### Mutable, thread-local data
//!
//! Imagine you want to count the number of invocations to a function per
//! thread. You'd like to store the count in a `Cell<usize>` and use
//! `count.set(count.get() + 1)` to increment the count. Prior to `state`, your
//! only option was to use the `thread_local!` macro. `state` provides a more
//! flexible, and arguably simpler solution via `LocalStorage`. This scanario
//! is implemented in the folloiwng:
//!
//! ```rust
//! # extern crate state;
//! # use std::cell::Cell;
//! # use std::thread;
//! # #[cfg(feature = "tls")]
//! # use state::LocalStorage;
//! # #[cfg(feature = "tls")]
//! static COUNT: LocalStorage<Cell<usize>> = LocalStorage::new();
//!
//! # #[cfg(not(feature = "tls"))] fn function_to_measure() { }
//! # #[cfg(feature = "tls")]
//! fn function_to_measure() {
//!     let count = COUNT.get();
//!     count.set(count.get() + 1);
//! }
//!
//! # #[cfg(not(feature = "tls"))] fn main() { }
//! # #[cfg(feature = "tls")]
//! fn main() {
//!     // setup the initializer for thread-local state
//!     COUNT.set(|| Cell::new(0));
//!
//!     // spin up many threads that call `function_to_measure`.
//!     let mut threads = vec![];
//!     for i in 0..10 {
//!         threads.push(thread::spawn(|| {
//!             // Thread IDs may be reusued, so we reset the state.
//!             COUNT.get().set(0);
//!             function_to_measure();
//!             COUNT.get().get()
//!         }));
//!     }
//!
//!     // retrieve the total
//!     let total: usize = threads.into_iter()
//!         .map(|t| t.join().unwrap())
//!         .sum();
//!
//!     assert_eq!(total, 10);
//! }
//! ```
//!
//! ## Performance
//!
//! `state` is heavily tuned to perform optimally. `get{_local}` and
//! `set{_local}` calls to a `Container` incur overhead due to type lookup.
//! `Storage`, on the other hand, is optimal for global storage retrieval; it is
//! _slightly faster_ than accessing global state initialized through
//! `lazy_static!`, more so across many threads. `LocalStorage` incurs slight
//! overhead due to thread lookup. However, `LocalStorage` has no
//! synchronization overhead, so retrieval from `LocalStorage` is faster than
//! through `Storage` across many threads.
//!
//! Keep in mind that `state` allows global initialization at _any_ point in the
//! program. Other solutions, such as `lazy_static!` and `thread_local!` allow
//! initialization _only_ a priori. In other words, `state`'s abilities are a
//! superset of those provided by `lazy_static!` and `thread_local!`.
//!
//! ## When To Use
//!
//! You should avoid using `state` as much as possible. Instead, thread state
//! manually throughout your program when feasible.
//!

#[macro_use]
#[cfg(feature = "tls")]
extern crate lazy_static;

mod ident_hash;
mod container;
mod storage;
mod init;
#[cfg(feature = "tls")] mod thread_local;
#[cfg(feature = "tls")] mod tls;

pub use container::Container;
pub use storage::Storage;
#[cfg(feature = "tls")] pub use tls::LocalStorage;
