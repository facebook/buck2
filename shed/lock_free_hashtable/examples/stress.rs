/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Stress test for `LockFreeRawTable`.
//! It runs forever.

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::atomic;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use std::sync::Condvar;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;

use lock_free_hashtable::LockFreeRawTable;

struct Barrier {
    rem: AtomicUsize,
}

impl Barrier {
    fn new(n: usize) -> Barrier {
        Barrier {
            rem: AtomicUsize::new(n),
        }
    }

    /// Wait for all threads to reach this point.
    fn wait(&self) {
        // We are not using any OS synchronization primitives here,
        // so we start all the work with as little delay as possible.
        let mut rem = self.rem.fetch_sub(1, atomic::Ordering::Relaxed) - 1;
        while rem != 0 {
            rem = self.rem.load(atomic::Ordering::Relaxed);
        }
    }
}

fn hash(key: u32) -> u64 {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

fn hash_fn(key: &u32) -> u64 {
    hash(*key)
}

struct JobArg<'a> {
    /// Iteration number.
    i: u32,
    /// Thread number, 0..num_cpus.
    thread_i: u32,
    /// Number of threads.
    num_threads: u32,
    table: &'a LockFreeRawTable<u32>,
    /// Barrier can be used to synchronize threads in the middle of the job.
    barrier_0: &'a Barrier,
}

impl<'a> JobArg<'a> {
    /// Generate values unique to this job iteration among all threads.
    fn unique_values(&self, count: u32) -> impl Iterator<Item = u32> + '_ {
        (0..count).map(move |i| self.i + self.thread_i * count + i)
    }
}

fn run_stress_test(name: &str, job_count: u32, job_fn: impl Fn(JobArg) + Clone + Send + 'static) {
    println!("{name}:");

    let num_threads = num_cpus::get() as u32;

    struct JobData {
        /// Synchronize threads at the beginning of the job.
        start_barrier: Barrier,
        /// The table to be used by the job.
        table: LockFreeRawTable<u32>,
        /// Synchronize threads in the middle of the job.
        barrier_0: Barrier,
    }

    let jobs = (0..job_count)
        .map(|_| {
            let table = LockFreeRawTable::new();
            let start_barrier = Barrier::new(num_threads as usize);
            let barrier_0 = Barrier::new(num_threads as usize);
            Arc::new(JobData {
                table,
                start_barrier,
                barrier_0,
            })
        })
        .collect::<Vec<_>>();

    struct SharedState {
        /// Iteration.
        i: AtomicUsize,
        /// Threads still running.
        running_threads: Mutex<u32>,
        /// Wait for all threads to finish.
        condvar: Condvar,
    }

    let shared_state = Arc::new(SharedState {
        i: AtomicUsize::new(0),
        running_threads: Mutex::new(num_threads),
        condvar: Condvar::new(),
    });

    let mut threads = Vec::new();
    for thread_i in 0..num_threads {
        let jobs: Vec<Arc<JobData>> = jobs.clone();
        let shared_state = shared_state.clone();
        let job_fn = job_fn.clone();
        threads.push(thread::spawn(move || {
            for (i, job) in jobs.into_iter().enumerate() {
                if thread_i == 0 {
                    shared_state.i.store(i, atomic::Ordering::Relaxed);
                }
                job.start_barrier.wait();
                job_fn(JobArg {
                    thread_i,
                    i: i as u32,
                    num_threads,
                    table: &job.table,
                    barrier_0: &job.barrier_0,
                });
            }
            let mut rem_threads = shared_state.running_threads.lock().unwrap();
            *rem_threads = rem_threads.checked_sub(1).unwrap();
            if *rem_threads == 0 {
                shared_state.condvar.notify_all();
            }
        }));
    }

    // Release memory.
    drop(jobs);

    loop {
        let guard = shared_state.running_threads.lock().unwrap();
        if *guard == 0 {
            break;
        }
        let (_guard, _timeout) = shared_state
            .condvar
            .wait_timeout(guard, Duration::from_secs(1))
            .unwrap();
        println!("{}", shared_state.i.load(atomic::Ordering::Relaxed));
    }

    for thread in threads {
        thread.join().unwrap();
    }
}

/// Concurrently insert unique values into a table.
fn insert_unique(jobs: u32) {
    run_stress_test("insert_unique", jobs, |job| {
        for v in job.unique_values(1000) {
            job.table
                .insert(hash(v), Box::new(v), |a, b| a == b, hash_fn);
        }
        job.barrier_0.wait();
        for v in job.unique_values(1000) {
            assert!(job.table.lookup(hash(v), |a| *a == v).is_some());
        }
        assert_eq!(1000 * job.num_threads as usize, job.table.iter().count());
    });
}

/// Concurrently insert the same values into a table.
fn insert_same(jobs: u32) {
    run_stress_test("insert_same", jobs, |job| {
        for v in 0..10000 {
            let v = v + job.i;
            job.table
                .insert(hash(v), Box::new(v), |a, b| a == b, hash_fn);
        }
        job.barrier_0.wait();
        for v in 0..10000 {
            let v = v + job.i;
            assert!(job.table.lookup(hash(v), |a| *a == v).is_some());
        }
        assert_eq!(10000, job.table.iter().count());
    });
}

/// Concurrently insert unique values into a table and lookup them not long after.
fn insert_lookup(jobs: u32) {
    run_stress_test("insert_lookup", jobs, |job| {
        let mut prev = None;
        for v in job.unique_values(1000) {
            job.table
                .insert(hash(v), Box::new(v), |a, b| a == b, hash_fn);
            if let Some(prev) = prev {
                assert!(job.table.lookup(hash(prev), |a| *a == prev).is_some());
            }
            prev = Some(v);
        }

        job.barrier_0.wait();
        assert_eq!(1000 * job.num_threads as usize, job.table.iter().count());
    })
}

fn main() {
    println!("num_cpus: {}", num_cpus::get());

    let mut jobs = 100;
    loop {
        insert_unique(jobs);
        insert_same(jobs);
        insert_lookup(jobs);
        if jobs < 100000 {
            jobs *= 10;
        }
    }
}
