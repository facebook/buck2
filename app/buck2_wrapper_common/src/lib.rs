/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Code shared between `buck2_wrapper` and `buck2`.
//!
//! Careful! The wrapper is not released as part of the regular buck version bumps,
//! meaning code changes here are not "atomically" updated.

use std::collections::HashSet;

use sysinfo::Pid;
use sysinfo::PidExt;
use sysinfo::ProcessExt;
use sysinfo::System;
use sysinfo::SystemExt;

pub mod invocation_id;

pub const BUCK2_WRAPPER_ENV_VAR: &str = "BUCK2_WRAPPER";
pub const BUCK_WRAPPER_UUID_ENV_VAR: &str = "BUCK_WRAPPER_UUID";

/// Kills all running Buck2 processes, except this process's hierarchy. Returns whether it
/// succeeded without errors.
pub fn killall(write: impl Fn(String)) -> bool {
    let mut system = System::new();
    system.refresh_processes();

    let mut current_parents = HashSet::new();
    let mut parent = Some(Pid::from_u32(std::process::id()));
    while let Some(pid) = parent {
        current_parents.insert(pid);
        parent = system.process(pid).and_then(|p| p.parent());
    }

    let mut buck2_processes = Vec::new();
    for (pid, process) in system.processes() {
        let exe_name = process.exe().file_stem().and_then(|s| s.to_str());
        if exe_name == Some("buck2") && !current_parents.contains(pid) {
            buck2_processes.push(process);
        }
    }

    if buck2_processes.is_empty() {
        write("No buck2 processes found".to_owned());
        return true;
    }

    let mut ok = true;

    for process in buck2_processes {
        let result = if process.kill() {
            // TODO(nga): this says "killed", but it doesn't wait for the process to exit.
            "Killed"
        } else {
            ok = false;
            "Failed to kill"
        };

        write(format!(
            "{} {} ({}). {}",
            result,
            process.name(),
            process.pid(),
            process.cmd().join(" ")
        ));
    }

    ok
}
