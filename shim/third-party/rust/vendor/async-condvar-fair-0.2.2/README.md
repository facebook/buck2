async-condvar-fair
==================

Condition variables for async Rust.  Features:

 * Fair.  [`notify_one`] always wakes up the longest-waiting waiter.

 * Novel [`Baton`] facility to help avoid dropped notifications
   which can otherwise arise through async task cancellation.

 * Works with any async runtime.

 * Can work with any kind of mutex (sync or async).

 * 100% safe code.  (Uses `parking_lot` internally.)

 * Can handle multiple different mutexes interacting with multiple
   different condvars, if you like that kind of thing.

Main entrypoint
---------------

The main entrypoint to this library is [`Condvar`].
See its documentation for details of the constructors and 
methods available.

Example
-------

```
// async-condvar-fair = { version = "0.2", features = "parking_lot_0_12" }
// parking_lot = { version = "0.12", features = ["send_guard"] }
# #[cfg(feature = "parking_lot_0_12")] mod demo {
use std::collections::VecDeque;
use async_condvar_fair::{Condvar, BatonExt};
use parking_lot::Mutex;

struct Shared {
    cv: Condvar,
    queue: Mutex<VecDeque<String>>,
}

impl Shared {
    pub async fn procssor(&self) {
        let mut guard = self.queue.lock();
        let mut baton = None;
        loop {
            while let Some(entry) = guard.pop_front() {
                println!("processing {:?}", &entry);
            }
            baton.dispose();

            let got = self.cv.wait_baton(guard).await;
            guard = got.0;
            baton = got.1;
        }
    }
}
# }

```

Mutexes - choosing a mutex, sync vs async
-----------------------------------------

Note that it can make perfect sense to have an async condvar, but
protect the shared state with a sync mutex.  If the operations which
occur while holding the mutex are fast (and, in particular, do not
perform IO), this is usually the best choice.

[`parking_lot::Mutex`] is a good choice for such a sync mutex, and is
conveniently supported by `async-condvar-fair`.  You will probably
want to enable `parking_lot`'s `send_guard` feature.

If you use a sync mutex, you probably don't intend to be awaiting with
the mutex held, so you can probably use plain [`wait`] (rather than
[`wait_baton`]).

If the operations you do while holding the mutex are slow, you should
use the async mutex supplied with your async runtime.  In this case,
if you are using [`notify_one`] you should consider using
[`wait_baton`] to avoid task cancellation causing lost notifications:
see [`Baton`].

### Mutex guard, `Send`ing between threads

In a multithreaded async runtime, tasks can move between threads.
This means the mutex guard needs to be `Send`.  Async mutex guards
supplied with multithreaded async runtimes are all `Send`.  Sync mutex
guards often aren't.

For example, [`std::sync::MutexGuard`], which can only be used with
`Condvar` with a non-multithreaded runtime, because futures which have
a local variable with such a guard will not be `Send`.  Use
[`parking_lot`] with `send_guard` enabled, instead.

Mutexes - how to pass the mutex to `wait_baton` et al
-----------------------------------------------------

[`Condvar::wait_baton`] and [`wait`] can in principle work with any mutex.
But they need to know how to relock the mutex.

For the most convenient mutexes, like [`parking_lot::Mutex`], you can
just pass the guard to `wait`.  `Condvar` will use the guard to unlock
the mutex, and then to relock it again during wakeup.

But for many mutexes, this is not possible, because the guard type
does not provide a way to get back to the unlocked mutex reference.
This is the case, for example, for [`std::sync::Mutex`].

For these inconvenient mutexes, you can pass a tuple to [`wait_baton`]
 or [`wait`], or use [`wait_no_relock`] and relock the mutex yourself.

### Builtin mutex support, and `async-condvar-fair` crate features

| feature        | mutex type      | pass to [`wait`] / [`wait_baton`]      |
| -------------- | ---------------------- | ------------------------------- |
| always enabled | [`std::sync::Mutex`]   | `(MutexGuard, &Mutex)` \[2\]   |
| `parking_lot_N` | [`parking_lot::Mutex`] | `MutexGuard` \[1\]             |
| `parking_lot_N` | [`parking_lot::FairMutex`] | `FairMutexGuard` \[1\]     |
| `tokio`        | [`tokio::sync::Mutex`] | `(MutexGuard, &Mutex)`          |
| `tokio` | [`Arc`]`<`[`tokio::sync::Mutex`]`>` | `(OwnedMutexGuard, Arc<Mutex>)` |
| `tokio` | [`tokio::sync::RwLock`]       | `(RwLockReadGuard, &Mutex)`     |
| `tokio` | [`tokio::sync::RwLock`]       | `(RwLockWriteGuard, &Mutex)`    |
| `tokio` | [`Arc`]`<`[`tokio::sync::RwLock`]`>` | `(OwnedRwLockReadGuard, Arc<RwLock>)` |
| `tokio` | [`Arc`]`<`[`tokio::sync::RwLock`]`>` | `(OwnedRwLockWriteGuard, Arc<RwLock>)` |
| `smol` | [`smol::lock::Mutex`]           | `MutexGuard`                   |
| `smol` | [`smol::lock::RwLock`]          | `(RwLockReadGuard, &RwLock)`   |
| `smol` | [`smol::lock::RwLock`]          | `(RwLockWriteGuard, &RwLock)`  |
| `smol` | [`Arc`]`<`[`smol::lock::Mutex`]`>` | `MutexGuardArc`             |

Notes:
 0. None of these are not enabled by default,
    since each one implies a dependency on the corresponding crate.
 1. Enable the `parking_lot_N` feature corresponding to the `parking_lot`
    version you are using (as specified in your `Cargo.toml`).
    Enable `parking_lot`'s `send_guard` feature for use with a
    multithreaded runtime.
 2. Single-threaded runtimes only.

### Use with other mutexes

The support listed above is all achieved through provided
implementations of the [`RelockMutexGuard`] trait.

If you want to use a mutex type without builtin support in
`async-condvar-wait`, use the [`RelockMutexGuard!`] macro to define a
suitable impl, define that impl by hand, or use [`wait_no_relock`] and
relock the mutex yourself each time.

### Example of passing a tuple of guard and mutex

```
# fn main() {
# #[cfg(feature = "parking_lot_0_12")] {
# use std::collections::VecDeque;
# use async_condvar_fair::{Condvar, BatonExt};
use std::sync::Mutex;
struct Shared {
    cv: Condvar,
    queue: Mutex<VecDeque<String>>,
}
# 
# impl Shared {
#    pub async fn procssor(&self) {
#        let mut guard = self.queue.lock().unwrap();
#        let mut baton = None;
loop {
    while let Some(entry) = guard.pop_front() {
        println!("processing {:?}", &entry);
        if entry.is_empty() { break }
    }
    baton.dispose();

    let got = self.cv.wait_baton((guard, &self.queue)).await;
    guard = got.0;
    baton = got.1;
}
#    }
# } } }
```

Change log - breaking changes and significant bug fixes
=======================================================

 * 0.2.2 2022-03-27:
   - Bump `dlv-list` dependency.  No API or functional changes.

 * 0.2.1 2022-03-27:
   - **Breaking change:**
     New `parking_lot_N` features for providing `RelockMutexGuard`
     impls for specific `parking_lot` versions.
     When upgrading from 0.1.0, specify the new feature `parking_lot_0_11`.
   - Support for `parking_lot` 0.9, 0.10, 0.12.

 * 0.2.0: version number not used.

 * 0.1.0, 2021-07-05
   - Initial release.
