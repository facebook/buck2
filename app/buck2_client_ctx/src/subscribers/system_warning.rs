/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::is_open_source;
use buck2_event_observer::humanized::HumanizedBytes;

use crate::subscribers::recorder::process_memory;

pub(crate) struct MemoryPressureHigh {
    pub(crate) system_total_memory: u64,
    pub(crate) process_memory: u64,
}
pub const SYSTEM_MEMORY_REMEDIATION_LINK: &str = ": https://fburl.com/buck2_mem_remediation";

pub(crate) fn system_memory_exceeded_msg(memory_pressure: &MemoryPressureHigh) -> String {
    format!(
        "High memory pressure: buck2 is using {} out of {}{}",
        HumanizedBytes::new(memory_pressure.process_memory),
        HumanizedBytes::new(memory_pressure.system_total_memory),
        if is_open_source() {
            ""
        } else {
            SYSTEM_MEMORY_REMEDIATION_LINK
        }
    )
}

pub(crate) fn check_memory_pressure<T>(
    snapshot_tuple: &Option<(T, buck2_data::Snapshot)>,
    system_info: &buck2_data::SystemInfo,
) -> Option<MemoryPressureHigh> {
    snapshot_tuple.as_ref().and_then(|(_, snapshot)| {
        process_memory(snapshot).and_then(|process_memory| {
            system_info
                .system_total_memory_bytes
                .and_then(|system_total_memory| {
                    // TODO (ezgi): one-shot commands don't record this. Prevent panick (division-by-zero) until it is fixed.
                    system_info.memory_pressure_threshold_percent.and_then(
                        |memory_pressure_threshold_percent| {
                            if (process_memory * 100)
                                .checked_div(system_total_memory)
                                .is_some_and(|res| res >= memory_pressure_threshold_percent)
                            {
                                Some(MemoryPressureHigh {
                                    system_total_memory,
                                    process_memory,
                                })
                            } else {
                                None
                            }
                        },
                    )
                })
        })
    })
}
