// Copyright Ian Jackson and contributors
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use std::env;
use std::fmt::{self, Display, Formatter, Write};
use std::io::{self, LineWriter, Write as _};
use std::sync::Arc;
use std::os::unix::ffi::OsStrExt;
use std::str;
use std::task::Waker;

use futures_util::select;
use futures_util::FutureExt;
use futures_test::future::FutureTestExt;

use paste::paste;

use super::*;

mod predmutex;
mod exhaustive;

pub type SendTestFuture = Pin<Box<dyn Future<Output=()> + Send>>;
pub type NoSendTestFuture = Pin<Box<dyn Future<Output=()>>>;

#[cfg(feature="tokio")]
#[path="test/with-tokio.rs"]
mod with_tokio;

#[cfg(feature="smol")]
#[path="test/with-smol.rs"]
mod with_smol;

pub use parking_lot;
#[cfg(feature="parking_lot_0_12")]
#[path="test/with-parking.rs"]
mod with_parking;

macro_rules! test_parking_N { {
  $feat:literal, $with:ident, $parking_lot_N:ident
} => {
  #[cfg(feature=$feat)]
  #[path="test"]
  mod $with {
    pub use $parking_lot_N as parking_lot;
    pub use super::*;
    #[path="with-parking.rs"]
    mod with_parking;
  }
} }
test_parking_N!{ "parking_lot_0_11", with_parking_0_11, parking_lot_0_11 }
// We don't test 0.10 and earlier becuase its guards are not Send

#[path="test/with-std.rs"]
mod with_std;


fn default<T>() -> T where T:Default { Default::default() }

#[macro_export]
macro_rules! lock_async { { $($x:tt)* } => {
  pub async fn lock_async<T>(l: &TestMutex<T>) -> TestMutexGuard<'_,T> {
    l $($x)*
  }
}}
use crate::lock_async;

#[macro_export]
macro_rules! define_test_with_parking {
  { $case:ident, $is_short:expr, $tasks:expr } => { paste! {
    #[test]
    fn [< $case _smol >](){
      use [< $case _parking_generic >] as call;
      select_debug!(true, call,)
    }

    fn [< $case _parking_generic >]<D:DebugWrite>(mut d: D) {
      let tasks: Vec<TasksMaker<_>> = $tasks;
      for tasks in tasks.into_iter() {
        let _: Vec<()> = smol::block_on(
          futures_util::future::join_all(tasks.1(&mut d))
        );
      }
    }
  } };
}

//---------- debug selection ----------

pub trait DebugWrite: Default + io::Write + Send + Sync + 'static {
  const ENABLED: bool;
  fn enabled(&self) -> bool { Self::ENABLED }
}
#[derive(Debug,Default)] struct DebugDisabled;
#[derive(Debug        )] struct DebugEnabled(LineWriter<Eprintln>);

impl DebugWrite for DebugDisabled { const ENABLED: bool = false; }
impl DebugWrite for DebugEnabled  { const ENABLED: bool = true; }

impl io::Write for DebugDisabled {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> { Ok(buf.len()) }
  fn flush(&mut self)             -> io::Result<()  >  { Ok(()       ) }
}
impl io::Write for DebugEnabled {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize>  { self.0.write(buf) }
  fn flush(&mut self)             -> io::Result<()   >  { self.0.flush(   ) }
}
impl Default for DebugEnabled {
  fn default() -> Self { Self(LineWriter::new(default())) }
}
#[derive(Debug,Default)]
struct Eprintln; // this is daft, but proper API is unstable
impl io::Write for Eprintln {
  fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
    let s = str::from_utf8(buf).unwrap();
    eprint!("{}", s);
    Ok(buf.len())
  }
  fn flush(&mut self)             -> io::Result<()  >  { Ok(()       ) }
}

#[macro_export]
macro_rules! select_debug {
  { $def:expr, $call:expr, $($after:tt)* } => {
    if want_debug($def) {
      $call (DebugEnabled::default()) $($after)*
    } else {
      $call (DebugDisabled)           $($after)*
    }
  }
}

pub fn want_debug(def: bool) -> bool {
  let var = "ASYNC_CONDVAR_DEBUG";
  match env::var_os(var).map(|s| s.as_bytes().to_owned()).as_deref() {
    None | Some([]) => def,
    Some([ b'0',.. ]) => false,
    Some([ b'n',.. ]) => false,
    Some([ b'f',.. ]) => false,
    Some([ b'1',.. ]) => true,
    Some([ b'y',.. ]) => true,
    Some([ b't',.. ]) => true,
    _ => panic!("bad value for {}", var),
  }
}

#[macro_export]
macro_rules! edbg {
  ($($val:expr),*) => {
    if want_debug(false) { ddbg!(DebugEnabled::default(); $($val),*); }
  }
}

#[macro_export]
macro_rules! ddbg {
  ($d:expr; $val:expr $(,)?) => {
    match $val {
      tmp => {
        if $d.enabled() {
          writeln!($d, "[{}:{}] {} = {:?}",
                   file!(), line!(), stringify!($val), &tmp)
            .unwrap();
        }
        tmp
      }
    }
  };
  ($d:expr; $($val:expr),+ $(,)?) => {
    ($(ddbg!($d; $val)),+,)
  };
}
