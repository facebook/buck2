# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import ctypes
import time
from typing import List, Optional, Tuple

MB_TO_BYTES = 1024 * 1024
PRESSURE_THRESHOLD_MULTIPLIER = 100

libc = ctypes.CDLL("libc.so.6")
BUFFERS: List[Tuple[ctypes.Array, ctypes.c_void_p]] = []


# Use mlock system call to allocate and lock memory in RAM, preventing it from being swapped by system
def mlock_memory(size_mb: float) -> Tuple[ctypes.Array, ctypes.c_void_p]:
    """Allocate and lock memory of the specified size in megabytes."""
    memory_size = int(size_mb * MB_TO_BYTES)
    buffer = ctypes.create_string_buffer(memory_size)
    addr = ctypes.cast(buffer, ctypes.c_void_p)

    result = libc.mlock(addr, len(buffer))
    if result != 0:
        raise RuntimeError(f"Failed to lock memory: {ctypes.get_errno()}")

    return buffer, addr


def compute_median(values: List[float]) -> Optional[float]:
    """Calculate the median of a list of values."""
    if not values:
        return None

    sorted_values = sorted(values)
    n = len(sorted_values)

    if n % 2 == 0:
        return (sorted_values[n // 2 - 1] + sorted_values[n // 2]) / 2
    else:
        return sorted_values[n // 2]


def is_memory_pressure_detected(current_time: float, all_times: List[float]) -> bool:
    """Check if memory pressure is detected based on allocation time."""
    median_time = compute_median(all_times)
    return (
        median_time is not None
        and current_time > PRESSURE_THRESHOLD_MULTIPLIER * median_time
    )


class MemoryPressure:
    def __init__(self, output_file: str):
        self.output_file = open(output_file, "w")
        self.total_allocated = 0.0

    def log(self, message: str) -> None:
        """Write message to both output file and stdout."""
        self.output_file.write(f"{message}\n")
        self.output_file.flush()
        print(message)

    def run_allocations(
        self,
        allocate_count: int,
        tick_duration: float,
        memory_per_tick: float,
        sleep_duration_before_exit: int = 5,
    ) -> None:
        """Run the memory allocation with pressure detection."""
        allocation_times = []

        run_start = time.time()

        for tick in range(allocate_count):
            self.log(f"Tick {tick}")
            self.log(f"Allocating {memory_per_tick} MB")

            start_time = time.time()
            buffer, addr = mlock_memory(memory_per_tick)
            allocation_time = time.time() - start_time

            self.total_allocated += memory_per_tick
            self.log(
                f"[{(time.time() - run_start):.2f}] Allocated {memory_per_tick} MB in {allocation_time:.5f} seconds | Total: {self.total_allocated} MB"
            )

            BUFFERS.append((buffer, addr))

            if is_memory_pressure_detected(allocation_time, allocation_times):
                self.log("Memory pressure detected")
                break

            allocation_times.append(allocation_time)
            time.sleep(tick_duration)

        # sleep for 5 seconds to simulate work
        time.sleep(sleep_duration_before_exit)

    def cleanup(self) -> None:
        """Close the output file."""
        self.output_file.close()


def cleanup_buffers() -> None:
    """Release all allocated memory buffers."""
    print(f"Clearing {len(BUFFERS)} buffers...")
    for buffer, addr in BUFFERS:
        try:
            libc.munlock(addr, len(buffer))
            ctypes.memset(buffer, 0, len(buffer))
        except Exception:
            pass
    print("Clearing completed")


def parse_arguments() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(description="Memory pressure test tool")
    parser.add_argument(
        "--allocate-count",
        type=int,
        required=True,
        help="Number of allocation iterations",
    )
    parser.add_argument(
        "--tick-duration",
        type=float,
        default=1.0,
        help="Duration between allocations in seconds",
    )
    parser.add_argument(
        "--each-tick-allocate-memory",
        type=float,
        default=0.0,
        help="Memory to allocate per tick in MB",
    )
    parser.add_argument(
        "--output", type=str, required=True, help="Output log file path"
    )
    parser.add_argument(
        "--pre-exit-sleep-duration",
        type=int,
        default=5,
        help="Sleep duration before exit",
    )

    return parser.parse_args()


def main() -> None:
    """Main entry point for the memory pressure test."""
    args = parse_arguments()

    runner = MemoryPressure(args.output)
    try:
        runner.run_allocations(
            args.allocate_count,
            args.tick_duration,
            args.each_tick_allocate_memory,
            args.pre_exit_sleep_duration,
        )
    finally:
        runner.cleanup()


if __name__ == "__main__":
    try:
        main()
    finally:
        cleanup_buffers()
