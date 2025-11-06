/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::Sender;
use std::thread;
use std::time::Duration;
use std::time::Instant;

use clap::Parser;

fn touch_pages(d: &mut [u8]) {
    for b in d.iter_mut().step_by(4096) {
        *b = (*b).wrapping_add(1);
    }
}

/// A routine that is run in a thread of its own which prevents memory from being paged out by
/// writing to it
fn run_busy_loop(rx: Receiver<&'static mut [u8]>) -> ! {
    let mut allocations: Vec<&'static mut [u8]> = Vec::new();
    loop {
        for a in allocations.iter_mut() {
            touch_pages(a)
        }

        while let Ok(a) = rx.try_recv() {
            touch_pages(a);
            allocations.push(a);
        }
    }
}

/// A routine that is run in a thread of its own which detects cgroup freezes and logs them
fn run_freeze_checker(logger: &Logger) -> ! {
    let mut last_inst = Instant::now();

    loop {
        std::thread::sleep(Duration::from_millis(10));
        let new_inst = Instant::now();
        let diff = new_inst.duration_since(last_inst);
        // 50ms is a safe threshold above the expected 10ms sleep to account for the possibility of
        // CPU pressure or some other reason that the thread might not have been scheduled
        // immediately
        if diff > Duration::from_millis(50) {
            logger.log(&format!("freeze_detected_ms {}", diff.as_millis()));
        }
        last_inst = new_inst;
    }
}

struct Logger {
    output: Mutex<File>,
}

impl Logger {
    fn log(&self, message: &str) {
        let mut output = self.output.lock().unwrap();
        writeln!(output, "{}", message).unwrap();
        output.flush().unwrap();
        println!("{}", message);
    }
}

fn run_main_loop(
    logger: &Logger,
    allocate_count: usize,
    tick_duration: Duration,
    memory_per_tick: usize,
    sleep_duration_before_exit: Duration,
    alloc_tn: Sender<&'static mut [u8]>,
) {
    for tick in 0..allocate_count {
        logger.log(&format!("Tick {}", tick));
        logger.log(&format!("Allocating {} MB", memory_per_tick));

        alloc_tn.send(vec![0; memory_per_tick].leak()).unwrap();

        thread::sleep(tick_duration);
    }

    thread::sleep(sleep_duration_before_exit);
}

#[derive(Parser, Debug)]
#[command(name = "use_some_memory")]
#[command(about = "Memory pressure test tool", long_about = None)]
struct Args {
    #[arg(long, help = "Number of allocation iterations")]
    allocate_count: usize,

    #[arg(
        long,
        default_value = "1.0",
        help = "Duration between allocations in seconds"
    )]
    tick_duration: f64,

    #[arg(
        long,
        default_value = "0.0",
        help = "Memory to allocate per tick in MB"
    )]
    each_tick_allocate_memory: f64,

    #[arg(long, help = "Output log file path")]
    output: Option<String>,

    #[arg(long, help = "A file that we append a 1 to after being started")]
    start_marker: Option<String>,

    #[arg(
        long,
        default_value = "5.0",
        help = "Sleep duration before exit in seconds"
    )]
    pre_exit_sleep_duration: f64,
}

fn main() {
    let args = Args::parse();

    if let Some(start_marker) = args.start_marker.as_ref() {
        OpenOptions::new()
            .create(true)
            .append(true)
            .open(start_marker)
            .unwrap()
            .write_all(b"1\n")
            .unwrap();
    }

    let output = if let Some(path) = args.output {
        OpenOptions::new()
            .write(true)
            // Used to verify that in the kill and retry case, we properly clean up outputs
            .create_new(true)
            .open(path)
            .unwrap()
    } else {
        OpenOptions::new().write(true).open("/dev/null").unwrap()
    };

    let logger = Arc::new(Logger {
        output: Mutex::new(output),
    });
    let logger2 = logger.clone();

    std::thread::spawn(move || run_freeze_checker(&logger2));

    let (alloc_tn, alloc_rx) = std::sync::mpsc::channel();

    std::thread::spawn(move || run_busy_loop(alloc_rx));

    run_main_loop(
        &logger,
        args.allocate_count,
        Duration::from_secs_f64(args.tick_duration),
        (args.each_tick_allocate_memory * 1_000_000f64) as usize,
        Duration::from_secs_f64(args.pre_exit_sleep_duration),
        alloc_tn,
    );
}
