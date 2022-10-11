// Copyright% Ian Jackson and contributors
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

pub use predmutex::Guard as TestMutexGuard;
pub use crate::define_test_exhaustive as define_test;
pub use super::SendTestFuture as TestFuture;
pub use cases::pair_for_wait::for_wait;
lock_async!{ .lock().await }
#[path="cases.rs"] pub mod cases;
use cases::*;

use rayon::iter::{ParallelDrainRange, ParallelIterator};

#[allow(non_camel_case_types)] use parking_lot::Mutex as pMutex;

//---------- test mutex, using pending_once ----------

// This is so that a task which runs in a loop, releasing and then
// re-acquiring the mutex, can be preempted.  The predmutex would be
// ready to re-lock immediately; pending_once gives the executor a
// look-in.

#[allow(non_camel_case_types)] 
#[derive(Default)]
pub struct TestMutex<T>(predmutex::Mutex<T>);

use futures_util::task::ArcWake;

RelockMutexGuard!{
  (TestMutexGuard, TestMutex),
  l => l.lock(),
}

impl<T> TestMutex<T> {
  pub async fn lock<'l>(&'l self) -> predmutex::Guard<'l, T> {
    self.0.lock().pending_once().await
  }
}

//---------- exhsastive search executor ----------

#[derive(Debug,Copy,Clone)]
enum Status {
  WantsPoll,
  Blocked,
  Finished,
}
use Status::*;

struct Executor<'r,'c,'d, D: DebugWrite> {
  tasks: Vec<Task>,
  chosen: Vec<usize>,
  record: Option<&'r mut Record<'c>>,
  chosen_alert: String,
  d: &'d mut D,
}

struct Task {
  inner: TestFuture,
  status: Arc<StatusWrap>,
  waker: Waker,
}

#[derive(Debug)]
struct StatusWrap(pMutex<Status>, usize);

#[derive(Debug,Default)]
pub struct Record<'c> {
  // History possibilities are a tree.
  //
  // Each time we do a tick, we choose one of the possible tasks to
  // poll.
  //
  // At any point, all the partially-completed nodes form a single
  // path.  That's `done`.
  done: Vec<Node>,
  chosen_prefix: &'c str,
}

#[derive(Debug,Clone,Copy)]
struct Node {
  noneafter: usize,
  done: usize, // always < ntasks
  map: TrackBitmap,
}
type TrackBitmap = u32;
const MB: u32 = 2;

#[derive(Debug)]
enum TickReport {
  RunContinue,
  RunCompleteOk,
  RunNoNew,
  SearchComplete,
  DepthLimit,
}
use TickReport::*;

const MAX_DEPTH: usize = 100;

fn chosen_alert(_: &str) { }

struct ChosenDisplay<'p,'c> {
  prefix: &'p str,
  chosen: &'c [usize],
}
impl fmt::Display for ChosenDisplay<'_,'_> {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}", self.prefix)?;
    for c in self.chosen { write!(f, "{}", c)?; }
    Ok(())
  }
}

impl<'r,'c,'d, D: DebugWrite> Executor<'r,'c,'d,D> {
  fn tick(&mut self) -> TickReport {
    let record = self.record.as_mut().expect("search complete!");

    if record.done.len() <= self.chosen.len() {
      record.done.push(Node {
        map: !0,
        done: 0,
        noneafter: self.tasks.len(),
      });
      if record.done.len() >= MAX_DEPTH { return DepthLimit }
    }
    write!(self.d, "{}  ", record).unwrap();

    let (taski, end, any_blocked) = {
      let node = &mut record.done[self.chosen.len()];

      write!(self.d, "tasks ").unwrap();
      let mut taski = None;
      let mut any_blocked = false;
      let mut end = RunCompleteOk;
      let mut map = 0;
      let mut noneafter = 0;
      for i in 0..self.tasks.len() {
        let status = *self.tasks[i].status.0.lock();
        {
          let sti = status as TrackBitmap;
          assert!(sti < 1 << MB);
          map <<= MB;
          map |= sti;
        }
        match status {
          Blocked => {
            any_blocked = true;
            write!(self.d, "_").unwrap();
            continue;
          }
          Finished => {
            write!(self.d, ".").unwrap();
            continue;
          }
          WantsPoll => {
            noneafter = i+1;
            if i >= node.done {
              write!(self.d, "A").unwrap();
              taski.get_or_insert(i);
            } else {
              write!(self.d, "a").unwrap();
              end = RunNoNew;
            }
          }
        }
      }
      if node.map != ! 0 {
        assert_eq!(node.map, map,
                   "\nVAR then={:x} now={:x} node={:?} nt={} chosen {}\n",
                   node.map, map, node, self.tasks.len(), ChosenDisplay {
                     prefix: &record.chosen_prefix, chosen: &self.chosen
                   });
      }
      node.map = map;
      
      node.noneafter = noneafter;
      write!(self.d, " ").unwrap();
      (taski, end, any_blocked)
    };

    if D::ENABLED {
      write!(self.d, " chosen ").unwrap();
      let mut chosen = String::with_capacity(
        record.chosen_prefix.len() + self.chosen.len() + 1
      );
      write!(chosen, "{}", ChosenDisplay {
        prefix: &record.chosen_prefix, chosen: &self.chosen,
      }).unwrap();
      if let Some(i) = taski { write!(chosen, "{}", i).unwrap(); }
      write!(self.d, "{}", chosen).unwrap();
      if chosen == self.chosen_alert { chosen_alert(&chosen) }
    }

    let taski = match taski {
      Some(i) => i,
      None => {
        while let Some(did) = self.chosen.pop() {
          let mut done = &mut record.done[self.chosen.len()];
          let ddone = did+1;
          done.done = ddone;
          let done = *done;
          record.done.truncate(self.chosen.len()+1);
          if done.done < done.noneafter {
            writeln!(self.d, " {:?}", end).unwrap();
            if matches!(end, RunCompleteOk) { assert!(! any_blocked) }
            return end;
          }
        }
        self.record = None;
        return SearchComplete;
      },
    };
    self.chosen.push(taski);
    writeln!(self.d, "").unwrap();
  
    let task = &mut self.tasks[taski];

    // Doing this now allows a task to return Pending but wake itself
    *task.status.0.lock() = Blocked;

    let mut cx = std::task::Context::from_waker(&task.waker);
    let got = Pin::new(&mut task.inner).poll(&mut cx);

    match got {
      Pending => { },
      Ready(()) => *task.status.0.lock() = Finished,
    }

    return RunContinue;
  }
}

impl ArcWake for StatusWrap {
  fn wake_by_ref(arc_self: &Arc<StatusWrap>) {
    edbg!(arc_self.1);
    *arc_self.0.lock() = WantsPoll
  }
}

//---------- setup etc. ----------

impl Record<'_> {
  fn search_1<D: DebugWrite>(&mut self, d: &mut D,
                             mk_tasks: &TasksMakerFn<D>)
                             -> (TickReport, Vec<usize>)
  {
    let tasks = mk_tasks(d);
    assert!(tasks.len() <= ((TrackBitmap::BITS-1) / MB) as usize);

    let tasks = tasks.into_iter().enumerate().map(|(ix, inner)| {
      let status = WantsPoll;
      let status = Arc::new(StatusWrap(pMutex::new(status), ix));
      let waker = futures_util::task::waker(status.clone());
      Task { inner, status, waker }
    }).collect::<Vec<_>>();

    let mut coordinator = Executor::<D> {
      tasks, d,
      chosen: vec![],
      record: Some(self),
      chosen_alert: env::var("ASYNC_CV_TEST_BREAK_CHOSEN")
        .unwrap_or(" ".into()),
    };

    loop {
      match coordinator.tick() {
        RunContinue => continue,
        r@ RunCompleteOk |
        r@ RunNoNew |
        r@ DepthLimit |
        r@ SearchComplete => return (r, coordinator.chosen),
      }
    }
  }
}

impl Display for Record<'_> {
  fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
    write!(f, "record {}", self.chosen_prefix)?;
    for d in &self.done { write!(f, "{}", d.done)? }
    Ok(())
  }
}

pub fn search<D>(d: &mut D, (chosen_prefix, mk_tasks): TasksMaker<D>) -> u64
where D: DebugWrite
{
  let mut record: Record = Record {
    chosen_prefix: &chosen_prefix,
    done: default(),
  };
  let mut count: u64 = 0;
  loop {
    let (got, chosen) = record.search_1::<D>(d, &mk_tasks);
    match got {
      RunContinue => panic!(),
      DepthLimit => panic!("depth limit exceeded, path {}", ChosenDisplay {
        prefix: &chosen_prefix, chosen: &chosen }),
      RunNoNew => { },
      RunCompleteOk => {
        count += 1;
        if count % 100_000 == 0 {
          let s = format!("{:10} chosen {}\n", count,
                          ChosenDisplay { prefix: &chosen_prefix,
                                          chosen: &chosen });
          eprint!("{}", s);
        }
      },
      SearchComplete => return count,
    }
  }
}

#[macro_export]
macro_rules! define_test_exhaustive {
  { $case:ident, $is_short:expr, $tasks:expr } => { paste! {
    #[test]
    fn [< $case _exhaustive >](){
      use [< $case _exhaustive_generic >] as call;
      select_debug!(false, call,)
    }

    fn [< $case _exhaustive_generic >]<D:DebugWrite>(_d: D){
      let mut tasks: Vec<TasksMaker<_>> = $tasks;
      let count: u64 = tasks.par_drain(..).map(|tasks_gen| {
        let mut d: D = default();
        exhaustive::search(&mut d, tasks_gen)
      }).sum();
      eprintln!("{} count={}", stringify!($what), count);
    }
  } };
}
