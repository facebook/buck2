// Copyright Ian Jackson and contributors
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY.

use super::*;

use super::TestMutex as Mutex;

pub type TasksMakerFn<D> = Box<dyn Fn(&mut D) -> Vec<TestFuture> + Send>;
pub type TasksMaker<D> = (String, TasksMakerFn<D>);

#[derive(Default)]
struct Data {
  mx: Mutex<u32>,
  cv: Condvar,
}

pub mod pair_for_wait {
  use super::*;
  #[allow(dead_code)]
  pub fn for_wait<'l,T>(g: TestMutexGuard<'l,T>, l: &'l TestMutex<T>)
                        -> (TestMutexGuard<'l,T>, &'l TestMutex<T>) {
    (g,l)
  }
}
pub mod guard_for_wait {
  use super::*;
  #[allow(dead_code)]
  pub fn for_wait<'l,T>(g: TestMutexGuard<'l,T>, _l: &'l TestMutex<T>)
                        -> TestMutexGuard<'l,T> {
    g
  }
}

impl Data where {
  async fn wait_for_at_least<D>(&self, want: u32)
  where D: DebugWrite
  {
    let mut d = default::<D>();
    ddbg!(d; want);
    let mut guard = lock_async(&self.mx).await;
    ddbg!(d; want);
    loop {
      let now = *guard;
      ddbg!(d; want, now);
      if now >= want { break; }
      guard = self.cv.wait(for_wait(guard, &self.mx)).await;
    }
  }
}

//---------- broadcast ----------

pub fn broadcast_tasks<D>(_d: &mut D) -> Vec<TestFuture>
where D: DebugWrite
{
  let data_0: Arc<Data> = default();

  let data = data_0.clone();
  let t1 = async move { data.wait_for_at_least::<D>(1).await };

  let data = data_0.clone();
  let t2 = async move { data.wait_for_at_least::<D>(2).await };

  let data = data_0.clone();
  let tup = async move {
    let d = &mut default::<D>();
    for i in 1..=3 {
      {
        ddbg!(d; i);
        let mut guard = lock_async(&data.mx).await;
        ddbg!(d; i);
        *guard = i;
        ddbg!(d; i);
      }
      data.cv.notify_all();
      ddbg!(d; i);
    }
  };

  vec![
    Box::pin( t1  ),
    Box::pin( t2  ),
    Box::pin( tup ),
  ]
}

define_test!{ broadcast, true, vec![(
    "".to_string(),
  Box::new(broadcast_tasks)
)]}

//---------- signal ----------

const SIGEND: u32 = 1000;

impl Data {
  async fn worker<D>(&self, d: &mut D, want: u32) -> &'static str
  where D: DebugWrite
  {
    ddbg!(d; want);
    let mut guard = lock_async(&self.mx).await;
    ddbg!(d; want);
    loop {
      let now = *guard;
      ddbg!(d; want, now);
      if now >= SIGEND {
        return "end";
      } else if now >= want {
        *guard -= 1;
      } else {
        guard = self.cv.wait(for_wait(guard, &self.mx)).await;
      }
    }
  }
}

fn signal_tasks_1<D:DebugWrite>
  (_d: &mut D, carryon: usize) -> Vec<TestFuture>
{
  ddbg!(&mut default::<D>(); carryon);

  let data_0: Data = default();
  let data_0 = Arc::new(data_0);

  let mut tasks: Vec<TestFuture> = vec![];

  let mut mk_worker = |want| {
    let data = data_0.clone();
    let mut d: D = default();

    let stopper = if tasks.len() == carryon {
      None
    } else {
      let stop_mutex = Arc::new(predmutex::Mutex::new(()));
      let stop_guard = {
        let stop_mutex = stop_mutex.clone();
        stop_mutex.try_lock().unwrap();
      };
      tasks.push(Box::pin(async move { drop(stop_guard); }));
      Some(async move { stop_mutex.lock().await; "cancel" })
    };

    tasks.push(Box::pin(async move {
      let finish = if let Some(stopper) = stopper {
        select! {
          r = stopper.fuse() => r,
          r = data.worker(&mut d, want).fuse() => r,
        }
      } else {
        data.worker(&mut d, want).await
      };
      ddbg!(d; want, finish);
    }));
  };

  mk_worker(1);
  mk_worker(2);

  let data = data_0.clone();
  tasks.push(Box::pin(async move {
    let mut d: D = default();
    for _ in 0..3 {
      let mut guard = lock_async(&data.mx).await;
      *guard += 1;
      ddbg!(d; *guard);
      data.cv.notify_one();
    }
    let mut guard = lock_async(&data.mx).await;
    ddbg!(d; *guard);
    *guard = SIGEND;
    data.cv.notify_all();
  }));

  tasks
}

pub fn signal_tasks<D: DebugWrite>(_d: &mut D) -> Vec<TasksMaker<D>> {
  (0..2).map(|i| (
    ((b'A' + (i as u8)) as char).to_string(),
    Box::new(move |d: &'_ mut _| {
      signal_tasks_1(d, i)
    })
      as TasksMakerFn<_>
  )).collect()
}

define_test!{ signal, false, signal_tasks(&mut default()) }
