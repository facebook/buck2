/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub fn get_execution_time_ms(commands: &[buck2_data::CommandExecution]) -> Option<u64> {
    if let Some(command_execution) = commands.last() {
        if let Some(details) = &command_execution.details {
            if let Some(metadata) = &details.metadata {
                if let Some(execution_time) = &metadata.execution_time {
                    return Some(
                        (execution_time.seconds * 1000) as u64
                            + (execution_time.nanos / 1000000) as u64,
                    );
                }
            }
        }
    }
    None
}
