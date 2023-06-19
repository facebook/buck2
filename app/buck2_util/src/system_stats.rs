/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub struct UnixSystemStats {
    pub load1: f64,
    pub load5: f64,
    pub load15: f64,
}

impl UnixSystemStats {
    #[cfg(unix)]
    pub fn get() -> Option<Self> {
        let mut loadavg: [f64; 3] = [0.0, 0.0, 0.0];
        if unsafe { libc::getloadavg(&mut loadavg[0], 3) } != 3 {
            // This doesn't seem to set errno (or at least it's not documented to do so).
            return None;
        }
        Some(Self {
            load1: loadavg[0],
            load5: loadavg[1],
            load15: loadavg[2],
        })
    }

    #[cfg(not(unix))]
    pub fn get() -> Option<Self> {
        None
    }
}
