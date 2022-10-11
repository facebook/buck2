
use super::*;

use std::ops::{Deref, DerefMut};

use parking_lot::Mutex as fMutex;
use parking_lot::MutexGuard as fMutexGuard;

#[derive(Debug)]
pub struct Mutex<T> {
  st: fMutex<State>,
  data: fMutex<T>,
}
impl<T> Default for Mutex<T> where T: Default {
  fn default() -> Self { Mutex::new(default()) }
}

#[derive(Clone)] // only allowed in Drop
#[derive(Debug)]
struct Token;

#[derive(Debug,Default)]
struct State {
  free: Option<Token>,
  waiting: Vec<Waker>,
}

pub struct Guard<'l,T> {
  token: Token,
  mx: &'l Mutex<T>,
  data: fMutexGuard<'l,T>,
}
impl<'l,T> Deref for Guard<'l,T> {
  type Target = T;
  fn deref(&self) -> &T { &*self.data }
}
impl<'l,T> DerefMut for Guard<'l,T> {
  fn deref_mut(&mut self) -> &mut T { &mut *self.data }
}

impl<'l,T> Drop for Guard<'l,T> {
  fn drop(&mut self) {
    let mut st = self.mx.st.lock();
    //edbg!(&st);
    assert!(st.free.is_none());
    st.free = Some(self.token.clone());
    for waker in st.waiting.drain(..) {
      waker.wake()
    }
  }
}

impl<T> Mutex<T> {
  pub fn new(t: T) -> Self { Mutex {
    st: fMutex::new(State {
      free: Some(Token),
      waiting: vec![],
    }),
    data: fMutex::new(t)
  } }
  pub fn lock<'l>(&'l self) -> GuardFuture<'l,T> { self }
  pub fn try_lock<'l>(&'l self) -> Option<Guard<'l,T>> {
    let mut st = self.st.lock();
    self.try_lock_(&mut *st)
  }
  fn try_lock_<'l,'x>(&'l self, st: &'x mut State) -> Option<Guard<'l,T>> {
    //;edbg!(&st);
    let got = if let Some(token) = st.free.take() {
      Some(Guard {
        token,
        mx: self,
        data: self.data.try_lock().unwrap(),
      })
    } else {
      None
    };
    //edbg!(&st);
    got
  }
}

pub type GuardFuture<'l,T> = &'l Mutex<T>;

impl<'l,T> Guard<'l,T> {
  pub fn mutex(self_: &Self) -> &'l Mutex<T> { self_.mx }
}

impl<'l,T> Future for GuardFuture<'l,T> {
  type Output = Guard<'l,T>;
  fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>)
          -> Poll<Self::Output>
  {
    let self_ = Pin::into_inner(self);
    let mut st = self_.st.lock();

    if let Some(guard) = self_.try_lock_(&mut *st) {
      Ready(guard)
    } else {
      st.waiting.push(cx.waker().clone());
      Pending
    }
  }
}

impl<'l,T:Send> RelockMutexGuard for Guard<'l,T> {
  type MutexRef = &'l Mutex<T>;
  type JustGuard = Self;
  type LockFuture = GuardFuture<'l,T>;
  fn unlock_for_relock(self) -> Self::MutexRef { Guard::mutex(&self) }
  fn lock(r: Self::MutexRef) -> Self::LockFuture { r.lock() }
}
