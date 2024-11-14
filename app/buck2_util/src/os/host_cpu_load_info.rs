/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub fn host_cpu_load_info() -> buck2_error::Result<buck2_data::HostCpuLoadInfo> {
    #[cfg(target_os = "macos")]
    {
        crate::os::macos::host_cpu_load_info_low_level::host_cpu_load_info_low_level()
    }
    #[cfg(not(target_os = "macos"))]
    {
        // TODO(rajneeshl) : Implement for other platforms
        Ok(Default::default())
    }
}
