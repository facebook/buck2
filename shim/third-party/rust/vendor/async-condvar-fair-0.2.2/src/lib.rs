// Copyright Ian Jackson and contributors
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

#![forbid(unsafe_code)]
#![allow(clippy::needless_lifetimes)] // these sometimes make things clearer
#![allow(clippy::option_map_unit_fn)] // suggestion is in poor taste

//! [`wait`]: Condvar::wait
//! [`wait_baton`]: Condvar::wait_baton
//! [`wait_no_relock`]: Condvar::wait_no_relock
//! [`notify_one`]: Condvar::notify_one
//! [`RelockMutexGuard`]: trait@RelockMutexGuard
//! [`Arc`]: std::sync::Arc
#![doc = include_str!("../README.md")]

use std::fmt::Debug;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Waker, Poll, Poll::*};

use pin_project_lite::pin_project;

#[cfg(test)]
mod test;

// ---------- public structs ----------

/// **Condition variable (for async)**
///
/// For background information about of the semantics of a condition
/// variable, see [Wikipedia](https://en.wikipedia.org/wiki/Condition_variable#Condition_variables_2) and/or [`std::sync::Condvar`].
///
/// Unlike `std`'s condition variables, `async-condvar-fair`'s do not
/// block the current thread, only the current async task.  Also,
/// multiple condition variables may be freely used with multiple
/// different mutexes.
///
/// Like all condition variables, `async-condvar-wait`'s may generate
/// spurious wakeups.  After completing a wait, you must re-check the
/// actual condition in your data structure, not simply assume that it
/// must be ready for you.
#[derive(Debug,Default)]
pub struct Condvar(parking_lot::Mutex<CV>);

pin_project!{
/// Future for waiting, from [`wait_baton`]
///
/// See [`wait_baton`] for information about the results of awaiting this.
///
/// This type definition could be useful if a named type is wanted.
///
/// [`wait_baton`]: Condvar::wait_baton
pub struct Waiter<'c,G> where G: RelockMutexGuard {
  #[pin] waitstate: WaitState<'c,G>,
}}

/// Obligation to do work, or notify someone else to do it
///
/// # Task cancellation in async Rust
///
/// In async Rust, futures can be cancelled at any `await` point, and
/// simply discarded, if whatever was waiting for them loses interest.
/// From the point of view of a reader of the `async` block, this is
/// as if the code simply stopped running at some `await` point, for
/// reasons outside of its own control, and discarded all of its state.
///
/// When `notify_one` is being used, there is therefore a risk that
/// the waiting task that `notify_one` chooses to wake up gets
/// cancelled before it is able to do the work that the notifier
/// intended.
///
/// (This risk only arises if the process of responding to the
/// notification might `await`.  In that case you will also want to be
/// using an async mutex, since it is generally forbidden to
/// `await` with a sync mutex held.)
///
/// `Baton` helps with this risk.  `Option<Baton>` is returned by
/// `wait_baton`, and should be kept until the work is completed, and
/// then [`dispose`]d.
///
/// If the `Baton` is simply dropped (for example, due to task
/// cancellation), the condvar will be re-notified.
///
/// # How to handle a `Baton`
///
/// Use [`wait_baton`] rather than plain [`wait`].  When `wait_baton`
/// completes, keep the baton while you do whatever work there is to
/// be done.
///
/// After having done the necessary work, as the caller of
/// `notify_one` was expecting, call `Baton::dispose`.
///
/// # Infinite loop, or even livelock, risk
///
/// It is important to `dispose` of the baton even if your processing
/// suffers a (possibly persistent) error.  If you accidentally drop the
/// baton (eg on an error path), another task will be woken up and
/// perhaps perform the same failing actions, leading to the program
/// looping uselessly, eating cpu.
///
/// Depending on your runtime's scheduler, that might even be a
/// livelock.
///
/// [`dispose`]: Baton::dispose
/// [`wait_baton`]: Condvar::wait_baton
/// [`wait`]: Condvar::wait
#[derive(Debug)]
pub struct Baton<'c> {
  condvar: Option<&'c Condvar>, // always Some other than in discard()
}
// In the implementation there are not just real `Baton` but
// also other notional battons:
//
//   * An `Entry` which is `Signaled`.
//   * A `Waiter` which is `Locking` and has `baton=true`.
//
// We arrange `Drop` impls, and the code handling these situations, to
// always pass on the baton.

impl Condvar {
  pub fn new() -> Self { Default::default() }
}

// ---------- RelockMutexRef trait ----------

/// Lock guards that can be unlocked and relocked
///
/// # Purpose
///
/// [`Condvar::wait_baton`] and [`wait`] need to unlock and then
/// relock the mutex.  So, they need to be able to recover `&Mutex`
/// from the guard.
///
/// # Regaining the mutex ref
///
/// If the lock in use doesn't support this (e.g., at the time of
/// writing, `std::sync::Mutex`, it is usually possible to implement
/// this trait on a tuple struct of guard and lock reference (and,
/// therefore, pass that tuple struct to `Condvar::wait`.
///
/// # Provided implementations and support
///
/// Implementations are provided for a handful of common `MutexGuard`
/// types.  Contributions of more (with the appropriate optional
/// dpendencies in `Cargo.toml` are welcome.
///
/// The [`RelockMutexGuard!`] macro can save much boilerplate in the
/// common cases.
///
/// # Semantics
///
/// `async-condvar-fair` assumes that `RelockMutexGuard` impl's are
/// sensible.  If they aren't, malfunctions including deadlocks or
/// livelocks or panics are possible.  But memory safety won't be
/// compromised.
///
/// [`wait`]: Condvar::wait
pub trait RelockMutexGuard {
  /// The reference to the mutex, recovered after unlocking.
  type MutexRef: Clone + Send;

  /// The actual guard type.
  /// 
  /// Where `Self` (the argument to `wait_baton` or `wait` contains
  /// both a guard and a separate mutex reference, it is typically most
  /// convvenient for the `wait` futures to produce just the mutex guard.
  ///
  /// That is what this type is.
  type JustGuard;

  /// The type of the relock future.
  type LockFuture: Future<Output=Self::JustGuard> + Send;

  /// Unlock the mutex and return a reference tt.
  fn unlock_for_relock(self) -> Self::MutexRef;

  /// Relock the mutex, given a reference.
  ///
  /// Poisoning (as found in `std::sync::Mutedx`) will have to
  /// propagate poisoning as panics, or or ignore it, since
  /// `notify_one` insists that someone must go and acquire the mutex
  /// which they can't if that always fails due to poisoning.
  fn lock(r: Self::MutexRef) -> Self::LockFuture;
}

/// Implements [`trait@RelockMutexGuard`] (needed for any mutexes used
/// with a condvar)
///
/// # Summary
///
/// Helper macro to `impl` [`RelockMutexGuard`], for various mutex
/// types.  `RelockMutexGuard!` has five forms, for five use cases:
///
///  * Third-party impl, convenient mutex: mutex ref recoverable from guard
///  * Third-party impl, inconveneint mutex ref passed separately
///  * First or second party impl, convenient mutex
///  * Within `async_condvar_fair`, inconvenient mutex
///  * Explicitly specify type and lifetime parameters.
///
/// An alternative to implementing [`RelockMutexGuard`] (via this
/// macro or otherwisse) is to use [`wait_no_relock`] everywhere.
///
/// # Trait coherence, first/second party vs third party
///
/// The firt or second party forms can only be used in the crate
/// defining the mutex guard type (or within `async_condvar_fair`),
/// because of Rust's trait coherence rules.
/// 
/// The third party forms define a helper struct, for pasing to
/// `wait_baton` or `wait`.
///
/// # Convenient vs inconvenient mutexes
///
/// Ideally, mutex guards implement a method for recovering a
/// reference to the original mutex.  This saves users who need to
/// unlock and relock a mutex (like anyone uusing a condition
/// variable) from having to carry a separate copy of a reference to
/// the mutex.
///
/// For convenient mutexes, you can just pass the guard to
/// [`wait_baton`] or [`wait`].  For inconvenient mutexes, you must
/// also pass a reference to the uneerlying mutex - typically, as
/// element `.1` of a tuple or tuple struct.
///
/// # Examples
///
/// ## Third party impl, convenient mutex
///
/// ```
/// # use async_condvar_fair::{Condvar, RelockMutexGuard};
/// # use parking_lot as nice;
/// RelockMutexGuard!{
///     NiceGuardForCondvar(nice::MutexGuard) [nice::Mutex],
///     l => async move { l.lock() },
///     g => nice::MutexGuard::mutex(&g),
/// }
/// # let condvar = Condvar::new();
/// # let mutex = nice::Mutex::new(());
/// # let guard = mutex.lock();
/// condvar.wait_baton(NiceGuardForCondvar(guard));
/// ```
/// Macro expansion:
/// ```ignore
/// struct NiceGuardForCondvar<'l,T>(nice::MutexGuard<'l,T>);
/// impl<'l,T> RelockMutexGuard for NiceGuardForCondvar<'l,T> {/*...*/}
/// async fn wait(&self, NiceGuardForCondvar<'l,T>) -> nice::MutexGuard<'l,T>; //roughly
/// ```
///
/// The expression after `l =>` must be a a future, such as an async
/// block or an un-`await`ed call to an async function.  `l` will have
/// type `&'l MutexMT>`.  When awaited, the lock expression must yield
/// `Guard<'l, T>`.
///
/// The expression after `g =>` must recover the mutex reference.  `g`
/// will have type `Guard<'l, T>`, and the expression must have type
/// `&'l Mutex<T>`.
///
/// The mutex recovery expression is given ownership of the guard, but
/// it should discard it.  (The expression forms the core of the
/// generated implementation of `unlock_for_relock`.)
///
/// The optional `$xbound`s are additional bounds on `T` (each with
/// their own `where`, contrary to normal Rust syntax).  (This is
/// needed, for example, for `RwLockReadGuard`, which needs `T:
/// Send`.)
///
/// ## Third party impl, inconvenient mutex
///
/// ```
/// # use async_condvar_fair::{Condvar, RelockMutexGuard};
/// # use parking_lot as awkward;
/// RelockMutexGuard!{
///     AwkwardGuardForCondvar(awkward::MutexGuard, awkward::Mutex),
///     l => async move { l.lock() },
/// }
/// # let condvar = Condvar::new();
/// # let mutex = awkward::Mutex::new(());
/// # let guard = mutex.lock();
/// condvar.wait_baton(AwkwardGuardForCondvar(guard, &mutex));
/// ```
/// Macro expansion:
/// ```ignore
/// struct AwkwardGuardForCondvar<'o,'i,T>(
///     awkward::MutexGuard<'i,T>,
///     &'o awkward::Mutex<T>,
/// );
/// impl<'o,'i,T> RelockMutexGuard for AwkwardGuardForCondvar<'o,'i,T> {/*...*/}
/// async fn wait(&self, AwkwardGuardForCondvar<'o,'i,T>) -> awkward::MutexGuard<'o,T>; //roughly
/// ```
///
/// ## First and second-party impl's
///
/// If you are invoking `RelockMutexGuard!` in the same crate as
/// defines a convenient mutex, or within `async_condvar_fair`, omit
/// the struct name.
///
/// This will implement the trait directly for the guard type, or the
/// pair consisting of the guard and the mutex reference:
///
/// ### First or second party impl, convenient mutex
/// ```
/// # use async_condvar_fair::RelockMutexGuard;
/// # struct Mutex<T>(parking_lot::Mutex<T>);
/// # impl<T> Mutex<T> { fn lock(&self) -> MutexGuard<T> { todo!() } }
/// # struct MutexGuard<'l,T>(parking_lot::MutexGuard<'l,T>);
/// impl<'l,T> MutexGuard<'l,T> {
///     fn mutex(self_: &Self) -> &'l Mutex<T> { todo!() }
/// }
/// RelockMutexGuard!{
///     (MutexGuard) [Mutex],
///     l => async move { l.lock() },
///     g => MutexGuard::mutex(&g),
/// }
/// ```
/// Generates:
/// ```ignore
/// impl<'l,T> RelockMutexGuard for MutexGuard<'l,T> {/*...*/}
/// async fn wait(&self, MutexGuard<'l,T>) -> MutexGuard<'l,T>; //roughly
/// ```
/// ### Within `async_condvar_fair`, inconvenient mutex
/// ```ignore
/// RelockMutexGuard!{
///     (MutexGuard, Mutex),
///     l => async move { l.lock() },
/// }
/// ```
/// Generates:
/// ```ignore
/// impl<'o,'i,T> RelockMutexGuard for (MutexGuard<'i,T>, &'o Mutex<T>) {/*...*/}
/// async fn wait(&self, (MutexGuard<'i,T>, &'o Mutex<T>)) -> MutexGuard<'o,T>; //roughly
/// ```
///
/// ### Explicit type and lifetime parameters
///
/// The other four forms all assume that the unlocked mutex is `&...`
/// and that the mutex is a wrapper type directly around `T`.  For other
/// situations, for example `Arc`-based owned guards, the explicit form
/// is needed.
///
/// The other four forms are convenience wrappers that all expand to
/// the explicit form.
///
/// ```ignore
/// RelockMutexGuard!{
///      <T>
///      (smol::lock::MutexGuardArc<T>)
///      [std::sync::Arc<smol::lock::Mutex<T>>, smol::lock::MutexGuardArc<T>],
///      l => async move { l.lock_arc().await },
///      g => smol::lock::MutexGuardArc::source(&g).clone(),
///      where T: 'static
/// }
/// ```
/// `$guard_in` is the argument to [`wait`].  `$guard_out` is the output
/// of the future.  `$mutexref` is the intermediate, unlocked, form.
/// 
/// The first lifetime in the generic arguments (if there are any
/// lifetimes) becomes a bound on `LockFuture`.
/// 
/// `$t: $bounds` is expanded as `$t : std::marker::Send + $bounds`.
///
/// [`wait`]: Condvar::wait
/// [`wait_baton`]: Condvar::wait_baton
/// [`wait_no_relock`]: Condvar::wait_no_relock
/// [`notify_one`]: Condvar::notify_one
/// [`RelockMutexGuard`]: trait@RelockMutexGuard
#[macro_export]
macro_rules! RelockMutexGuard {
  { $struct:ident ($($guard:tt)+) $(,)? [$($mutex:tt)+] $(,)?
    $l:pat => $lock:expr,
    $g:pat => $get_mutex:expr
    $( , where $xbound:path )* $(,)?
  } => {
    pub struct $struct<'l,T>(pub $($guard)+<'l,T>);
    $crate::RelockMutexGuard! {
      <'l,T>
        ( $struct<'l,T> )
        [ &'l $($mutex)+ <T>, $($guard)+ <'l,T> ],
      $l => $lock,
      $struct($g) => $get_mutex,
      where T: $( $xbound + )*
    }
  };

  { $struct:ident ($guard:ident $(:: $guardx:ident)*, $($mutex:tt)+) $(,)?
    $l:pat => $lock:expr
    $( , where $xbound:path )* $(,)?
  } => {
    pub struct $struct<'i,'o,T>(pub $guard $(:: $guardx)*<'i,T>,
                                pub &'o $($mutex)+<T>);
    
    $crate::RelockMutexGuard! {
      <'o,'i,T>
        ( $struct<'i,'o,T> )
        [ &'o $($mutex)+ <T>, $guard $(:: $guardx)* <'o,T> ],
      $l => $lock,
      g => g.1,
      where T: $( $xbound + )*
    }

  };

  { ($($guard:tt)+) $(,)? [$($mutex:tt)+] $(,)?
    $l:ident => $lock:expr,
    $g:ident => $get_mutex:expr
    $( , where $xbound:path )* $(,)?
  } => {
    $crate::RelockMutexGuard! {
      <'l,T>
        ( $($guard)* <'l,T> )
        [ &'l $($mutex) +<T>, $($guard)* <'l,T> ],
      $l => $lock,
      $g => $get_mutex,
      where T: $( $xbound + )*
    }
  };

  { ($guard:ident $(:: $guardx:ident)*, $($mutex:tt)+) $(,)?
    $l:ident => $lock:expr
    $( , where $xbound:path )* $(,)?
  } => {
    $crate::RelockMutexGuard! {
      <'o,'i,T>
        ( ($guard $(:: $guardx)* <'i,T>, &'o $($mutex)+ <T>) )
        [ &'o $($mutex)+ <T>, $guard $(:: $guardx)* <'o,T> ],
      $l => $lock,
      g => g.1,
      where T: $( $xbound + )*
    }
  };

  { < $($gen_lf0:lifetime, $($gen_lf1:lifetime,)*)? $($gen_ty:ident),* > $(,)?
      ( $guard_in:ty )                             $(,)?
      [ $mutexref:ty, $guard_out:path ]            $(,)?
    $l:pat => $lock:expr                             ,
    $g:pat => $get_mutex:expr                        ,
    where $t:ident : $($bound:tt)*
  } => {
    impl < $($gen_lf0, $($gen_lf1,)*)? $($gen_ty),* >
    $crate::RelockMutexGuard for $guard_in
    where $t : std::marker::Send + $($bound)*
    {
      type MutexRef = $mutexref;
      type JustGuard = $guard_out;
      type LockFuture = std::pin::Pin<std::boxed::Box<
        dyn std::future::Future<Output=Self::JustGuard>
            + std::marker::Send $(+ $gen_lf0)?
      >>;
      fn unlock_for_relock(self) -> Self::MutexRef {
        let $g = self;
        $get_mutex
      }
      fn lock($l: Self::MutexRef) -> Self::LockFuture {
        std::boxed::Box::pin($lock)
      }
    }
  }
}

RelockMutexGuard!{
  (std::sync::MutexGuard, std::sync::Mutex),
  l => async move { l.lock().unwrap() },
}

impl<G> RelockMutexGuard for NotRelockable<G> {
  type MutexRef = ();
  type JustGuard = ();
  type LockFuture = std::future::Ready<()>;
  fn unlock_for_relock(self) -> Self::MutexRef { }
  fn lock(_l: ()) -> Self::LockFuture { std::future::ready(()) }
}

macro_rules! impl_parking_lot { {
  $feat:literal, $parking_lot:ident,
  $( $FairMutex:ident, )?
} => {
  #[cfg(feature=$feat)]
  RelockMutexGuard!{
    ($parking_lot::MutexGuard) [$parking_lot::Mutex],
    l => async move { l.lock() },
    g => $parking_lot::lock_api::MutexGuard::mutex(&g),
  }
  $(
  #[cfg(feature=$feat)]
  RelockMutexGuard!{
    ($parking_lot::FairMutexGuard) [$parking_lot::$FairMutex],
    l => async move { l.lock() },
    g => $parking_lot::lock_api::MutexGuard::mutex(&g),
  }
  )?
} }
impl_parking_lot!{ "parking_lot_0_12", parking_lot     , FairMutex, }
impl_parking_lot!{ "parking_lot_0_11", parking_lot_0_11, FairMutex, }
impl_parking_lot!{ "parking_lot_0_10", parking_lot_0_10, FairMutex, }
impl_parking_lot!{ "parking_lot_0_9",  parking_lot_0_9, }

#[cfg(feature="tokio")]
RelockMutexGuard!{
  (tokio::sync::MutexGuard, tokio::sync::Mutex),
  l => l.lock(),
}
#[cfg(feature="tokio")]
RelockMutexGuard!{
  (tokio::sync::RwLockReadGuard, tokio::sync::RwLock)
  l => l.read(),
  where Sync
}
#[cfg(feature="tokio")]
RelockMutexGuard!{
  (tokio::sync::RwLockWriteGuard, tokio::sync::RwLock),
  l => l.write(),
  where Sync
}
#[cfg(feature="tokio")]
RelockMutexGuard!{
  <T>
    ( (tokio::sync::OwnedMutexGuard<T>,
       std::sync::Arc<tokio::sync::Mutex<T>>) )
    [ std::sync::Arc<tokio::sync::Mutex<T>>,
      tokio::sync::OwnedMutexGuard<T> ],
  l => async move { l.lock_owned().await },
  g => g.1,
  where T: 'static
}
#[cfg(feature="tokio")]
RelockMutexGuard!{
  <T>
    ( (tokio::sync::OwnedRwLockReadGuard<T>,
       std::sync::Arc<tokio::sync::RwLock<T>>) )
    [ std::sync::Arc<tokio::sync::RwLock<T>>,
      tokio::sync::OwnedRwLockReadGuard<T> ],
  l => async move { l.read_owned().await },
  g => g.1,
  where T: Sync + 'static
}
#[cfg(feature="tokio")]
RelockMutexGuard!{
  <T>
    ( (tokio::sync::OwnedRwLockWriteGuard<T>,
       std::sync::Arc<tokio::sync::RwLock<T>>) )
    [ std::sync::Arc<tokio::sync::RwLock<T>>,
      tokio::sync::OwnedRwLockWriteGuard<T> ],
  l => async move { l.write_owned().await },
  g => g.1,
  where T: Sync + 'static
}

#[cfg(feature="smol")]
RelockMutexGuard!{
  (smol::lock::MutexGuard) [smol::lock::Mutex],
  l => l.lock(),
  g => smol::lock::MutexGuard::source(&g),
}
#[cfg(feature="smol")]
RelockMutexGuard!{
  (smol::lock::RwLockReadGuard, smol::lock::RwLock)
  l => l.read(),
  where Sync
}
#[cfg(feature="smol")]
RelockMutexGuard!{
  <T>
    (smol::lock::MutexGuardArc<T>)
    [std::sync::Arc<smol::lock::Mutex<T>>, smol::lock::MutexGuardArc<T>],
  l => async move { l.lock_arc().await },
  g => smol::lock::MutexGuardArc::source(&g).clone(),
  where T: 'static
}

// ---------- internal structs and types ----------

// States a Waiter/WaitState can be in:
//
// WS::Waiting, Entry::Waiting
//
//   A task is waiting on the mutex.  (The future has been created and
//   may be being polled.)
//
//   If the `Waiter` is dropped, the `Entry` is removed from the list.
//   See below (esp., `Entry::Signaled`) about the handling of any
//   notifications this might affect.
//
// WS::Waiting, Entry::Broadcasted
//
//   The task was waiting and has been woken (waker.wake()).  (Or
//   Perhaps the fututre has not been polled yet, in which case there
//   is no Waker, but then when the future is polled, we will see the
//   broadcast.
//
// WS::Waiting, Entry::Signaled
//
//   As for Broadcasted, but, additionally: `notify_one` was called
//   and this WS/Entry represents the baton.  If the `Waiter` is
//   dropped, the condvar must be re-signaled to notify another
//   waiter.
//
// WS::Locking
//
//   The task is trying to reacquire the lock.  We have a suitable
//   future working on that.  `baton` tells us whether, if we drop
//   this `Waiter`, we need to pass on the baton by signalling another
//   task.
//
// WS::Ended
//
//   The Waker is a future.  We have already returned `Poll::Ready`.
//   We don't expect to be called again; if we are, we panic.

#[derive(Debug,Default)]
struct CV {
  list: dlv_list::VecList<Entry>,
}
type I = dlv_list::Index<Entry>;

#[derive(Debug)]
enum Entry {
  Waiting(Option<Waker>),
  Signaled, // implied baton
  Broadcasted,
}
use Entry::*;


// This is a bit clunky because of a number of interlocking constraints:
//  * The public struct must be a struct, not an enum.  Hence the
//    public wrapper `Waiter` vs the internal `WaitState`.
//
//  * We want to impl Drop, somehow.  pin_project struts cannot impl
//    Drop, so we impl Drop on sub-structs that are not structurally
//    pinned.  So we have both pinned and non-pinned sub-fields.
//
//  * The Drop impls must be on parts of the struct which contain
//    `&Condvar`.  This plus the above means `&Condvar` must be
//    entangled with the enum, even though all the variants have it.
//
//  * pin_project_lite supports only struct variants, not tuple variants.

pin_project! {
#[project=WSProj]
enum WaitState<'c,G> where G: RelockMutexGuard {
  Waiting {
    ns: WS_Waiting<'c,G>
  },
  Locking {
    ns: WS_Locking_NS<'c>, // "non-structural", ie not #[pin], but Drop
    #[pin] locking: G::LockFuture,
  },
  Ended,
}}
type WS<'c,G> = WaitState<'c,G>;

#[derive(Debug)]
#[allow(non_camel_case_types)]
struct WS_Waiting<'c,G> where G: RelockMutexGuard {
  condvar: &'c Condvar,
  ent: Option<I>, // always Some, except when dropping or overwriting
  lock: G::MutexRef,
}
#[derive(Debug)]
#[allow(non_camel_case_types)]
struct WS_Locking_NS<'c> {
  condvar: &'c Condvar,
  baton: bool,
}

// ---------- waiting ----------

impl Condvar {
  /// Wait for someone to call [`notify_one`] or [`notify_all`]
  ///
  /// Atomically unlocks the mutex corresponding to `guard` and starts
  /// waiting for notify.
  ///
  /// This is a future producing `(G, Option<` [`Baton`] `<'c>>)`.
  ///
  /// `G` is a fresh guard for the mutex, which has been unlocked and
  /// relocked.
  ///
  /// `baton` is a token representing an possible obligation to either
  /// perform the actions that the caller of `notify_one` is
  /// expecting, or re-notify the condvar.  See [`Baton`].  `baton`
  /// will be `None` if the wakeup was the result of `notify_all`.
  ///
  /// [`notify_one`]: Condvar::notify_one
  /// [`notify_all`]: Condvar::notify_all
  pub fn wait_baton<'c,G>(&'c self, guard: G) -> Waiter<'c,G>
  where G: RelockMutexGuard
  {
    let mut cv = self.0.lock();
    let ent = cv.list.push_back(Waiting(None));
    let lock = RelockMutexGuard::unlock_for_relock(guard);
    Waiter { waitstate: WS::Waiting { ns: WS_Waiting {
      condvar: self,
      ent: Some(ent),
      lock,
    } } }
  }

  /// Wait for a notification; caller must worry about cancellation
  ///
  /// Like `wait_baton` but [`disposes`](Baton::dispose) of the baton
  /// right away on return.
  ///
  /// # Beware task cancellation when using with `notify_one` and an async mutex
  ///
  /// When `wait`, `notify_one`, and an async mutex, are combined,
  /// notifications can easily be lost: perhaps a task calling `wait`
  /// could be cancelled after being woken up by `notify_one`, but
  /// before doing the actual work.  If that happens, probably no
  /// other task will be signaled and the work would go undone.
  ///
  /// So when using `notify_one`, with an async mutex, it is probably
  /// best to use `wait_baton`.
  ///
  /// `async-condvar-fair` does guarantee that `notify_one` will
  /// ensure that at least one `wait` call returns to its caller, (In
  /// particular, if the `wait` task is cancelled after being selected
  /// by `notify_one`, but before it manages to acquire the mutex, the
  /// condvar will be re-notified.)
  /// 
  /// But, in async code it is difficult and error-prone to try to
  /// avoid waiting.  Any `await` might result in task cancellaton,
  /// and then if you're not using `wait_baton`, the notification will
  /// be lost.
  ///
  /// [`wait_baton`] avoids this problem.  `notify_all` doesn't suffer
  /// from it because everyone is woken up anyway.  If you are using a
  /// sync mutex, there is no problem either, because you won't be
  /// `await`ing while processing (ie, while holding the mutex) anyway.
  ///
  /// [`notify_one`]: Condvar::notify_one
  /// [`notify_all`]: Condvar::notify_all
  /// [`wait_baton`]: Condvar::wait_baton
  pub async fn wait<'c,G>(&'c self, guard: G) -> G::JustGuard
  where G: RelockMutexGuard
  {
    let (guard, baton) = self.wait_baton(guard).await;
    baton.dispose();
    guard
  }

  /// Wait for notification; caller will have to relock the mutex
  ///
  /// Like [`wait_baton`] but does not relock the mutex.
  ///
  /// This can be used with any mutex guard type, even one for
  /// which no impl of [`trait@RelockMutexGuard`] is available.
  ///
  /// `wait_no_relock` will first start waiting for notifications, and
  /// then drop `guard` (which, with a mutex guard, will unlock the
  /// mutex).  When `wait_no_relock` completes, you will very probably
  /// want to acquire the mutex again: with `wait_no_relock` this must
  /// be done separately.
  ///
  /// Be sure to `dispose` of the `Option<`[`Baton`]`>` exactly iff
  /// that is appropriate.
  ///
  /// # Deadlock hazard
  ///
  /// There is no type restricton on `guard`.  It is important that
  /// you pass the ownership of the actual mutex guard.
  /// There is no way for the compiler to spot if you don't.
  /// If (for example) you pass `&mut MutexGuard`,
  /// you will fail to unlock the mutex, usually resulting in deadlock.
  ///
  /// [`wait_baton`]: Condvar::wait_baton
  pub async fn wait_no_relock<'c,G>(&'c self, guard: G) -> Option<Baton<'c>> {
    let (_guard, baton) = self.wait_baton(NotRelockable(guard)).await;
    baton
  }
}
struct NotRelockable<G>(G);

impl<'c,G> Future for Waiter<'c,G> where G: RelockMutexGuard {
  type Output = (G::JustGuard, Option<Baton<'c>>);
  fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>)
          -> Poll<Self::Output>
  {
    loop { // to let us rerun the match if we cange the state

      match self.as_mut().project().waitstate.project() {

        WSProj::Ended => panic!(),

        WSProj::Waiting { ns: WS_Waiting { condvar, ent, lock } } => {
          let condvar = *condvar;
          let mut cv = condvar.0.lock();
          let entry = &mut cv.list.get_mut(ent.unwrap()).unwrap();
          let baton = match entry {
            Signaled => true,
            Broadcasted => false,
            Waiting(waker) => {
              *waker = Some(cx.waker().clone());
              return Pending;
            }
          };
          // OK, condvar has been signaled, we need to start to
          // reaquire the lock.  We have converted the baton from
          // Entry::Signaled to a a boolean.
          cv.list.remove(ent.take().unwrap());
          let locking = <G as RelockMutexGuard>::lock(lock.clone());
          self.as_mut().set(Waiter { waitstate: { WS::Locking {
            ns: WS_Locking_NS { condvar, baton },
            locking,
          } } } );
        },

        WSProj::Locking { ns: WS_Locking_NS { condvar, baton }, locking } => {
          let guard = match locking.poll(cx) {
            Pending => return Pending,
            Ready(guard) => guard,
          };
          let rbaton = condvar.baton_from_bool(*baton);
          *baton = false;
          self.as_mut().set(Waiter { waitstate: WS::Ended });
          return Ready((guard, rbaton))
        }
      }

    }
  }
}     

// ---------- notification ----------

impl Condvar {
  /// Notify a waiting task (aka "signal")
  ///
  /// If there are any tasks in [`wait_baton`] (or
  /// [`wait_no_relock`]), at least one of them will wake up and get
  /// `Some(`[`Baton`]`)` from wait_baton.
  ///
  /// Likewise, if there are any tasks in [`wait`], at least one of
  /// them will wake up and return.  But if that task is cancelled
  /// after `wait` completss, the notification may be lost.
  /// See [`wait`] and [`Baton`] for a discussion of the interaction
  /// between task cancellation and `notify_one`.
  ///
  /// Notifications do not "stack" or "count".  Calling `notify_one`
  /// several times might still wake up only one task.
  ///
  /// [`wait`]: Condvar::wait
  /// [`wait_baton`]: Condvar::wait_baton
  /// [`wait_no_relock`]: Condvar::wait_no_relock
  pub fn notify_one(&self) {
    self.0.lock().notify_one()
  }
}

impl CV {
  fn notify_one(&mut self) {
    //dbg!(&self.list);
    if let Some(entry) = self.list.front_mut() {
      match entry {
        Signaled | Broadcasted => { }, // notify_one is idempotent
        Waiting(waker) => {
          if let Some(waker) = waker.take() { waker.wake() }
          *entry = Signaled;
        },
      };
    }
  }
}

impl Condvar {
  /// Notify all waiting tasks (aka "broadcast")
  ///
  /// Wake up all tasks currently in [`wait`], [`wait_baton`],
  /// and [`wait_no_relock`],
  /// 
  /// Each the tasks in [`wait`] and [`wait_baton`] will start to try
  /// to reacquire the mutex; they will then (in general) take turns
  /// to return from `wait`/`wait_baton` with the mutex held.
  /// 
  /// All tasks will get `None` rather than `Some(` [`Baton`] `)`,
  /// from `wait_baton` or `wait_no_relock` - even possibly tasks
  /// which are in the process of waking up because of a previous call
  /// to `notify_one`.
  ///
  /// [`wait`]: Condvar::wait
  /// [`wait_baton`]: Condvar::wait_baton
  /// [`wait_no_relock`]: Condvar::wait_no_relock
  pub fn notify_all(&self) {
    let mut cv = self.0.lock();
    for entry in cv.list.iter_mut() {
      match entry {
        Signaled | Broadcasted => {
          *entry = Broadcasted; // no baton needed any more
        },
        Waiting(waker) => {
          if let Some(waker) = waker.take() { waker.wake() }
          *entry = Broadcasted;
        },
      };
    }
  }
}

impl Condvar {
  fn baton_from_bool<'c>(&'c self, yes: bool) -> Option<Baton<'c>> {
    if yes {
      Some(self.make_baton())
    } else {
      None
    }
  }

  /// Make a baton directly, without waiting
  ///
  /// This may be useful in unusual situations.
  ///
  /// If the returned `Baton` is simply dropped, this is the same as
  /// [`notify_one`].
  ///
  /// [`notify_one`]: Condvar::notify_one
  pub fn make_baton<'c>(&'c self) -> Baton<'c> {
    Baton { condvar: Some(self) }
  }
}

impl Baton<'_> {
  /// Declare that responsibility has been discharged
  ///
  /// The baton will be consumed, without generating any notifications.
  pub fn dispose(mut self) { let _ = self.condvar.take(); }

  /// Pass on the baton to someone else, if anyone else is waiting
  ///
  /// This is equivalent to `mem::drop`.
  pub fn pass(self) { /* drop impl will do the actual passing */ }
}

/// Extension trait for `Option<Baton>` to provide `dispose` and `pass`
pub trait BatonExt: Sized {
  /// Declare any responsibility has been discharged
  fn dispose(self);
  /// Pass on any responsibility to someone else
  fn pass(self) { /* drop impl will do the actual passing */ }
}
impl BatonExt for Option<Baton<'_>> {
  fn dispose(self) { self.map(Baton::dispose); }
}

impl<G> Drop for WS_Waiting<'_,G> where G: RelockMutexGuard {
  fn drop(&mut self) {
    if let Some(ent) = self.ent.take() {
      let mut cv = self.condvar.0.lock();
      cv.list.remove(ent);
    }
  }
}
impl Drop for WS_Locking_NS<'_> {
  fn drop(&mut self) {
    let _baton = self.condvar.baton_from_bool(self.baton);
    // we pass the baton to Baton::dorp */
  }
}

impl Drop for Baton<'_> {
  fn drop(&mut self) {
    if let Some(condvar) = self.condvar.take() {
      condvar.notify_one();
    }
  }
}
