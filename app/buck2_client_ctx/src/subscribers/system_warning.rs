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
use buck2_util::system_stats::system_memory_stats;

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

pub(crate) fn check_memory_pressure(snapshot: &buck2_data::Snapshot) -> Option<MemoryPressureHigh> {
    process_memory(snapshot).and_then(|process_memory| {
        // TODO(ezgi): We should check the recorded system memory, not the real one so that replay would show the warnings properly.
        let system_total_memory = system_memory_stats();
        if process_memory as f64 / system_total_memory as f64 >= 0.75 {
            Some(MemoryPressureHigh {
                system_total_memory,
                process_memory,
            })
        } else {
            None
        }
    })
}
