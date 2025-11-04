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
use std::thread;
use std::time::Duration;
use std::time::Instant;

use clap::Parser;

const MB_TO_BYTES: usize = 1024 * 1024;
const PRESSURE_THRESHOLD_MULTIPLIER: f64 = 100.0;

struct MemoryBuffer {
    ptr: *mut u8,
    size: usize,
}

impl Drop for MemoryBuffer {
    fn drop(&mut self) {
        unsafe {
            libc::munlock(self.ptr as *const libc::c_void, self.size);
            libc::free(self.ptr as *mut libc::c_void);
        }
    }
}

fn mlock_memory(size_mb: f64) -> Result<MemoryBuffer, String> {
    let memory_size = (size_mb * MB_TO_BYTES as f64) as usize;

    unsafe {
        let ptr = libc::malloc(memory_size) as *mut u8;
        if ptr.is_null() {
            return Err("Failed to allocate memory".to_owned());
        }

        std::ptr::write_bytes(ptr, 0, memory_size);

        let result = libc::mlock(ptr as *const libc::c_void, memory_size);
        if result != 0 {
            libc::free(ptr as *mut libc::c_void);
            return Err(format!(
                "Failed to lock memory: {}",
                std::io::Error::last_os_error()
            ));
        }

        Ok(MemoryBuffer {
            ptr,
            size: memory_size,
        })
    }
}

fn compute_median(values: &[f64]) -> Option<f64> {
    if values.is_empty() {
        return None;
    }

    let mut sorted_values = values.to_vec();
    sorted_values.sort_by(|a, b| a.partial_cmp(b).unwrap());

    let n = sorted_values.len();
    if n.is_multiple_of(2) {
        Some((sorted_values[n / 2 - 1] + sorted_values[n / 2]) / 2.0)
    } else {
        Some(sorted_values[n / 2])
    }
}

fn is_memory_pressure_detected(current_time: f64, all_times: &[f64]) -> bool {
    if let Some(median_time) = compute_median(all_times) {
        current_time > PRESSURE_THRESHOLD_MULTIPLIER * median_time
    } else {
        false
    }
}

struct MemoryPressure {
    output_file: File,
    total_allocated: f64,
}

impl MemoryPressure {
    fn new(output_file: Option<String>) -> Result<Self, std::io::Error> {
        let file = if let Some(path) = output_file {
            OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)?
        } else {
            OpenOptions::new().write(true).open("/dev/null")?
        };

        Ok(MemoryPressure {
            output_file: file,
            total_allocated: 0.0,
        })
    }

    fn log(&mut self, message: &str) {
        writeln!(self.output_file, "{}", message).ok();
        self.output_file.flush().ok();
        println!("{}", message);
    }

    fn run_allocations(
        &mut self,
        allocate_count: usize,
        tick_duration: f64,
        memory_per_tick: f64,
        sleep_duration_before_exit: f64,
        buffers: &mut Vec<MemoryBuffer>,
    ) {
        let mut allocation_times = Vec::new();
        let run_start = Instant::now();

        for tick in 0..allocate_count {
            self.log(&format!("Tick {}", tick));
            self.log(&format!("Allocating {} MB", memory_per_tick));

            let start_time = Instant::now();
            match mlock_memory(memory_per_tick) {
                Ok(buffer) => {
                    let allocation_time = start_time.elapsed().as_secs_f64();
                    self.total_allocated += memory_per_tick;

                    let elapsed = run_start.elapsed().as_secs_f64();
                    self.log(&format!(
                        "[{:.2}] Allocated {} MB in {:.5} seconds | Total: {} MB",
                        elapsed, memory_per_tick, allocation_time, self.total_allocated
                    ));

                    buffers.push(buffer);

                    if is_memory_pressure_detected(allocation_time, &allocation_times) {
                        self.log("Memory pressure detected");
                        break;
                    }

                    allocation_times.push(allocation_time);
                }
                Err(e) => {
                    self.log(&format!("Failed to allocate memory: {}", e));
                    break;
                }
            }

            thread::sleep(Duration::from_secs_f64(tick_duration));
        }

        thread::sleep(Duration::from_secs_f64(sleep_duration_before_exit));
    }
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

    #[arg(
        long,
        default_value = "5.0",
        help = "Sleep duration before exit in seconds"
    )]
    pre_exit_sleep_duration: f64,
}

fn main() {
    let args = Args::parse();

    let mut buffers: Vec<MemoryBuffer> = Vec::new();

    let result = (|| {
        let mut runner = MemoryPressure::new(args.output)?;
        runner.run_allocations(
            args.allocate_count,
            args.tick_duration,
            args.each_tick_allocate_memory,
            args.pre_exit_sleep_duration,
            &mut buffers,
        );
        Ok::<(), std::io::Error>(())
    })();

    if let Err(e) = result {
        eprintln!("Error: {}", e);
    }

    println!("Clearing {} buffers...", buffers.len());
    drop(buffers);
    println!("Clearing completed");
}
