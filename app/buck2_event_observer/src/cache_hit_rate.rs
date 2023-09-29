/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub fn total_cache_hit_rate(
    local_actions: u64,
    remote_actions: u64,
    cached_actions: u64,
    remote_dep_file_cached_actions: u64,
) -> f64 {
    let total_actions =
        local_actions + remote_actions + cached_actions + remote_dep_file_cached_actions;
    let total_cached_actions = cached_actions + remote_dep_file_cached_actions;
    if total_actions == 0 || total_cached_actions == total_actions {
        1.0
    } else if total_cached_actions == 0 {
        0.0
    } else {
        (total_cached_actions as f64) / (total_actions as f64)
    }
}
